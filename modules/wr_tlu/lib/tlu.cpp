/** @file tlu.cpp
 *  @brief Implement interface to TLU via an Etherbone device.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2014 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Find all TLU units on device.
 *  Load contents of all pertinent registers.
 *
 *******************************************************************************
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 3 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *  
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library. If not, see <http://www.gnu.org/licenses/>.
 *******************************************************************************
 */

#define __STDC_FORMAT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <stdio.h>
#include <assert.h>
#include "tlu.h"
#include "hw-tlu.h"

namespace GSI_TLU {

struct SearchRecord {
  int done;
  status_t status;
  std::vector<TLU>* tlus;
};

static void trim(std::string& s) {
  std::string::size_type x = s.size();
  for (x = s.size(); x > 0; --x)
    if (s[x-1] != ' ') break;
  s.resize(x);
}

void tlu_sdb_search(SearchRecord* record, Device dev, const struct sdb_table* sdb, status_t status) {
  if (status != EB_OK) {
    record->status = status;
    record->done = 1;
    return;
  }
  
  unsigned devices = sdb->interconnect.sdb_records - 1;
  for (unsigned i = 0; i < devices; ++i) {
    const union sdb_record* des = &sdb->record[i];
    
    switch (des->empty.record_type) {
      case sdb_record_device: {
        if (des->device.sdb_component.product.vendor_id == GSI_VENDOR_ID) {
          switch (des->device.sdb_component.product.device_id) {
            case TLU_DEVICE_ID: {
              if (des->device.abi_ver_major != 1) break;
              TLU tlu;
              tlu.device        = dev;
              tlu.address       = des->device.sdb_component.addr_first;
              tlu.sdb_ver_major = des->device.abi_ver_major;
              tlu.sdb_ver_minor = des->device.abi_ver_minor;
              tlu.sdb_version   = des->device.sdb_component.product.version;
              tlu.sdb_date      = des->device.sdb_component.product.date;
              tlu.sdb_name      = std::string((const char*)&des->device.sdb_component.product.name[0], 19);
              trim(tlu.sdb_name);
              record->tlus->push_back(tlu);
              break;
            }
          }
        }
        break;
      }
      case sdb_record_bridge: {
        dev.sdb_scan_bus(&des->bridge, record, sdb_wrap_function_callback<SearchRecord, tlu_sdb_search>);
        
        record->done = 0;
        while (!record->done) dev.socket().run();
        break;
      }
    }
  }
  
  record->done = 1;
}

status_t TLU::probe(Device device, std::vector<TLU>& tlus) {
  /* Phase 1 -- locate TLU units using SDB */
  SearchRecord record;
  record.tlus = &tlus;
  
  tlus.clear();
  device.sdb_scan_root(&record, sdb_wrap_function_callback<SearchRecord, tlu_sdb_search>);
  
  record.done = 0;
  record.status = EB_OK;
  
  while (!record.done) device.socket().run();
  if (record.status != EB_OK) return record.status;
  
  /* Phase 2 -- Read TLU parameters */
  status_t status;
  data_t num_channels;
  data_t queue_size;
  
  Cycle cycle;
  
  for (unsigned i = 0; i < tlus.size(); ++i) {
    TLU& tlu = tlus[i];
    
    if ((status = cycle.open(tlu.device)) != EB_OK)
      return status;
    
    cycle.read(tlu.address + TLU_NUM_CHANNELS,  EB_DATA32, &num_channels);
    cycle.read(tlu.address + TLU_QUEUE_SIZE,    EB_DATA32, &queue_size);
    
    if ((status = cycle.close()) != EB_OK)
      return status;
    
    tlu.num_channels = num_channels;
    tlu.queue_size   = queue_size;
    tlu.channels.resize(num_channels);
    
    if ((status = tlu.refresh()) != EB_OK)
      return status;
  }
  
  return EB_OK;
}

struct Shadow {
  data_t fill, stable, dest, msg; 
};
status_t TLU::refresh() {
  status_t status;
  data_t v_active, v_edge, g_int, v_int, time0, time1, time2;
  Cycle cycle;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.read(address + TLU_ACTIVE_STATUS, EB_DATA32, &v_active);
  cycle.read(address + TLU_EDGE_STATUS,   EB_DATA32, &v_edge);
  cycle.read(address + TLU_INT_GLOBAL,    EB_DATA32, &g_int);
  cycle.read(address + TLU_INT_STATUS,    EB_DATA32, &v_int);
  cycle.read(address + TLU_TIME1,         EB_DATA32, &time1);
  cycle.read(address + TLU_TIME0,         EB_DATA32, &time0);
  cycle.read(address + TLU_TIME1,         EB_DATA32, &time2);
  
  std::vector<Shadow> shadow;
  shadow.resize(channels.size());
  
  for (unsigned i = 0; i < channels.size(); ++i) {
    Shadow& s = shadow[i];
    cycle.write(address + TLU_CH_SELECT,     EB_DATA32, i);
    cycle.read (address + TLU_CH_FILL_COUNT, EB_DATA32, &s.fill);
    cycle.read (address + TLU_CH_STABLE,     EB_DATA32, &s.stable);
    cycle.read (address + TLU_CH_INT_DEST,   EB_DATA32, &s.dest);
    cycle.read (address + TLU_CH_INT_MSG,    EB_DATA32, &s.msg);
  }
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  /* Timestamp overflowed; reread */
  if (time1 != time2) 
    return refresh();
  
  int_enable = (g_int != 0);
  
  current_time = time1;
  current_time <<= 32;
  current_time |= time0;
  current_time <<= 3;
  
  data_t mask = 1;
  for (unsigned i = 0; i < channels.size(); ++i) {
    Channel& ch = channels[i];
    Shadow&  s  = shadow[i];
    ch.int_enable = (v_int    & mask) != 0;
    ch.active     = (v_active & mask) != 0;
    ch.pos_edge   = (v_edge   & mask) != 0;
    ch.queued     = s.fill;
    ch.stable     = s.stable;
    ch.int_dest   = s.dest;
    ch.int_msg    = s.msg;
    mask <<= 1;
  }
  
  return EB_OK;
}

status_t TLU::set_enable(bool enable) {
  status_t status;
  if ((status = device.write(address + TLU_INT_GLOBAL, EB_DATA32, enable?1:0)) != EB_OK)
    return status;
  int_enable = enable;
  return EB_OK;
}

status_t TLU::hook(int channel, bool enable, uint32_t dest, uint32_t msg) {
  int first, last;
  Cycle cycle;
  status_t status;
  data_t mask;
  
  if (channel == -1) {
    first = 0;
    last = channels.size();
    mask = (uint32_t)-1;
  } else {
    first = channel;
    last  = channel+1;
    mask  = 1;
    mask <<= channel;
  }
  
  if (first < 0 || last > (int)channels.size()) 
    return EB_FAIL;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  if (enable) {
    cycle.write(address + TLU_INT_SET, EB_DATA32, mask);
  } else {
    cycle.write(address + TLU_INT_CLR, EB_DATA32, mask);
  }
  
  for (int i = first; i < last; ++i) {
    cycle.write(address + TLU_CH_SELECT,   EB_DATA32, i);
    cycle.write(address + TLU_CH_INT_DEST, EB_DATA32, dest);
    cycle.write(address + TLU_CH_INT_MSG,  EB_DATA32, msg);
  }
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  for (int i = first; i < last; ++i) {
    Channel& c = channels[i];
    c.int_enable = enable;
    c.int_dest   = dest;
    c.int_msg    = msg;
  }
  
  return EB_OK;
}

status_t TLU::listen(int channel, bool enable, bool pos_edge, uint32_t stable) {
  int first, last;
  Cycle cycle;
  status_t status;
  data_t mask;
  
  if (channel == -1) {
    first = 0;
    last = channels.size();
    mask = (uint32_t)-1;
  } else {
    first = channel;
    last  = channel+1;
    mask  = 1;
    mask <<= channel;
  }
  
  if (first < 0 || last > (int)channels.size()) 
    return EB_FAIL;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  if (enable) {
    cycle.write(address + TLU_ACTIVE_SET, EB_DATA32, mask);
  } else {
    cycle.write(address + TLU_ACTIVE_CLR, EB_DATA32, mask);
  }
  
  if (pos_edge) {
    cycle.write(address + TLU_EDGE_SET, EB_DATA32, mask);
  } else {
    cycle.write(address + TLU_EDGE_CLR, EB_DATA32, mask);
  }
  
  for (int i = first; i < last; ++i) {
    cycle.write(address + TLU_CH_SELECT, EB_DATA32, i);
    cycle.write(address + TLU_CH_STABLE, EB_DATA32, stable);
  }
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  for (int i = first; i < last; ++i) {
    Channel& c = channels[i];
    c.active   = enable;
    c.pos_edge = pos_edge;
  }
  
  return EB_OK;
}

status_t TLU::clear(int channel) {
  int first, last;
  Cycle cycle;
  status_t status;
  data_t mask;
  
  if (channel == -1) {
    first = 0;
    last = channels.size();
    mask = (uint32_t)-1;
  } else {
    first = channel;
    last  = channel+1;
    mask  = 1;
    mask <<= channel;
  }
  
  if (first < 0 || last > (int)channels.size()) 
    return EB_FAIL;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.write(address + TLU_CLEAR, EB_DATA32, mask);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  for (int i = first; i < last; ++i) {
    Channel& c = channels[i];
    c.queued = 0;
  }
  
  return EB_OK;
}

status_t TLU::test(int channel, uint8_t edge) {
  status_t status;
  Cycle cycle;
  
  if (channel == -1) {
    return device.write(address + TLU_TEST, EB_DATA32, edge);
  } else if (channel < 0 || channel >= (int)channels.size()) {
    return EB_FAIL;
  } else {
    if ((status = cycle.open(device)) != EB_OK)
      return status; 
    
    cycle.write(address + TLU_CH_SELECT, EB_DATA32, channel);
    cycle.write(address + TLU_CH_TEST,   EB_DATA32, edge);
    return cycle.close();
  }
}

status_t TLU::pop(int channel, uint64_t& time) {
  status_t status;
  data_t time1, time0, sub;
  Cycle cycle;
  
  if (channel < 0 || channel >= (int)channels.size())
    return EB_FAIL;
  
  Channel& c = channels[channel];
  if (c.queued == 0) {
    refresh();
    if (c.queued == 0) return EB_FAIL;
  }
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.write(address + TLU_CH_SELECT, EB_DATA32, channel);
  cycle.read (address + TLU_CH_TIME1,  EB_DATA32, &time1);
  cycle.read (address + TLU_CH_TIME0,  EB_DATA32, &time0);
  cycle.read (address + TLU_CH_SUB,    EB_DATA32, &sub);
  cycle.write(address + TLU_CH_POP,    EB_DATA32, 1);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  time = time1;
  time <<= 32;
  time |= time0;
  time <<= 3;
  time |= sub;
  --c.queued;
  
  return EB_OK;
}

struct Element {
  data_t time1, time0, sub;
};

status_t TLU::pop_all(int channel, std::vector<uint64_t>& queue) {
  status_t status;
  Cycle cycle;
  std::vector<Element> elements;
  data_t fill;
  uint64_t time;
  uint16_t chunks;
  
  if (channel < 0 || channel >= (int)channels.size())
    return EB_FAIL;
  
  Channel& c = channels[channel];
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.write(address + TLU_CH_SELECT,     EB_DATA32, channel);
  cycle.read (address + TLU_CH_FILL_COUNT, EB_DATA32, &fill);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  elements.resize(fill);
  if (fill == 0) return EB_OK;
  
  chunks = fill / UDP_MAX_POPS + unsigned(bool(fill % UDP_MAX_POPS));
  
  for (unsigned j = 0; j < chunks; j++)
  { 
      if ((status = cycle.open(device)) != EB_OK)
      return status;
      
      cycle.write(address + TLU_CH_SELECT, EB_DATA32, channel);
      for (unsigned i = j*UDP_MAX_POPS; i < std::min(unsigned(fill), (j+1)*UDP_MAX_POPS); i++)
      {
       Element& e = elements[i];
       cycle.read (address + TLU_CH_TIME1, EB_DATA32, &e.time1);
       cycle.read (address + TLU_CH_TIME0, EB_DATA32, &e.time0);
       cycle.read (address + TLU_CH_SUB,   EB_DATA32, &e.sub);
       cycle.write(address + TLU_CH_POP,   EB_DATA32, 1);
      }

     if ((status = cycle.close()) != EB_OK)
       return status;
   }  
  
  for (unsigned i = 0; i < elements.size(); ++i) {
    Element& e = elements[i];
    time = e.time1;
    time <<= 32;
    time |= e.time0;
    time <<= 3;
    time |= e.sub;
    
    queue.push_back(time);
  }
  
  c.queued = 0;
  
  return EB_OK;
}

status_t TLU::pop_all(std::vector<std::vector<uint64_t> >& queues) {
  status_t status;
  Cycle cycle;
  std::vector<std::vector<Element> > elements;
  std::vector<data_t> fill;
  uint64_t time;
  uint16_t chunks;
  
  queues.resize(channels.size());
  fill.resize(channels.size());
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  for (unsigned i = 0; i < channels.size(); ++i) {
    cycle.write(address + TLU_CH_SELECT,     EB_DATA32, i);
    cycle.read (address + TLU_CH_FILL_COUNT, EB_DATA32, &fill[i]);
  }
  
  if ((status = cycle.close()) != EB_OK)
    return status;
    
   elements.resize(channels.size());
   for (unsigned c = 0; c < channels.size(); ++c) {
      std::vector<Element>& esv = elements[c];
      esv.resize(fill[c]);
       
      chunks = fill[c] / UDP_MAX_POPS + unsigned(bool(fill[c] % UDP_MAX_POPS));

      for (unsigned j = 0; j < chunks; j++)
      { 
         if ((status = cycle.open(device)) != EB_OK)
         return status;


         cycle.write(address + TLU_CH_SELECT, EB_DATA32, c);
         for (unsigned e = j*UDP_MAX_POPS; e < std::min(unsigned(fill[c]), (j+1)*UDP_MAX_POPS); e++) {
            Element& es = esv[e];
            cycle.read (address + TLU_CH_TIME1, EB_DATA32, &es.time1);
            cycle.read (address + TLU_CH_TIME0, EB_DATA32, &es.time0);
            cycle.read (address + TLU_CH_SUB,   EB_DATA32, &es.sub);
            cycle.write(address + TLU_CH_POP,   EB_DATA32, 1);
         }
         if ((status = cycle.close()) != EB_OK)
         return status;
      }
   }

  for (unsigned c = 0; c < channels.size(); ++c) {
    std::vector<uint64_t>& o = queues[c];
    std::vector<Element>& esv = elements[c];
    
    for (unsigned e = 0; e < esv.size(); ++e) {
      Element& es = esv[e];
      time = es.time1;
      time <<= 32;
      time |= es.time0;
      time <<= 3;
      time |= es.sub;
      o.push_back(time);
    }
    
    channels[c].queued = 0;
  }
  
  return EB_OK;
}

}
