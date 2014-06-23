/** @file tlu.h
 *  @brief C++ Interface to the TLU hardware.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2014 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Public API to control all aspects of the Timestamp Latch Unit.
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

#ifndef TLU_H
#define TLU_H

#include <etherbone.h>
#include <string>
#include <vector>

namespace GSI_TLU {

using namespace etherbone;

/* ======================================================================= */
/* Software interface to hardware timestamp latch unit                     */
/* ======================================================================= */
struct TLU {
  /* ------------------------------------------------------------------- */
  /* Constant hardware values                                            */
  /* ------------------------------------------------------------------- */
  uint8_t      sdb_ver_major;
  uint8_t      sdb_ver_minor;
  uint32_t     sdb_version;
  uint32_t     sdb_date;
  std::string  sdb_name;
  
  uint32_t     num_channels;
  uint32_t     queue_size;
  
  /* ------------------------------------------------------------------- */
  /* Mutable hardware registers; only modify using methods below         */
  /* ------------------------------------------------------------------- */
  bool         int_enable;      /* Global interrupt enable */
  uint64_t     current_time;    /* # of 1ns ticks since 1970 to last refresh */
  
  struct Channel {
    bool       active;          /* Listening for edges? */
    bool       pos_edge;        /* Listen for positive/negative edge */
    bool       int_enable;      /* Interrupt enable per channel */
    uint32_t   queued;          /* Number of timestamps needing pop */
    uint32_t   stable;          /* Time a signal must remain stable */
    uint32_t   int_dest;        /* Destination of interrupt message */
    uint32_t   int_msg;         /* Content of interrupt message */
  };
  std::vector<Channel> channels;

  /* ------------------------------------------------------------------- */
  /* Access/modify the underlying hardware                               */
  /* ------------------------------------------------------------------- */
  
  Device       device;       /* Device which hosts this TLU */
  eb_address_t address;      /* Wishbone base address */
  
  /* Reload mutable registers from hardware */
  status_t refresh(); 
  
  /* Global interrupt enable */
  status_t set_enable(bool enable);
  
  /* Hook/unhook interrupt handling (channel -1 = all) */
  status_t hook(int channel, bool enable, uint32_t dest = 0, uint32_t msg = 0);
  
  /* Listen for positive/negative edge (channel -1 = all) */
  status_t listen(int channel, bool enable, bool pos_edge, uint32_t stable = 8);
  
  /* Clear the contents of a channel (channel -1 = all) */
  status_t clear(int channel);
  
  /* Inject a virtual timestamp for testing (channel -1 = all) */
  status_t test(int channel, uint8_t edge);
  
  /* Pop the next queued timestamp from the channel (units = 1ns step since 1970) */
  status_t pop(int channel, uint64_t& time);
  
  /* Pop all timestamps from a specific channel */
  status_t pop_all(int channel, std::vector<uint64_t>& queue);
  
  /* Pop all timestamps from all channels */
  status_t pop_all(std::vector<std::vector<uint64_t> >& queues);
  
  /* Find all TLUs in the SoC of a target device */
  static status_t probe(Device dev, std::vector<TLU>& tlus);
};

}

#endif
