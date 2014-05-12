/** @file hw-queue.cpp
 *  @brief C++ Wrapper for the ECA Queue hardware.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2014 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Read-write control of Queue registers.
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
#include "eca.h"
#include "hw-eca.h"

namespace GSI_ECA {

status_t ActionQueue::refresh() {
  Cycle cycle;
  eb_status_t status;
  eb_data_t d_int_mask;
  eb_data_t d_arrival_dest;
  eb_data_t d_overflow_dest;
  eb_data_t d_queued;
  eb_data_t d_dropped;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.read(address + ECAQ_INT_MASK, EB_DATA32, &d_int_mask);
  cycle.read(address + ECAQ_ARRIVAL,  EB_DATA32, &d_arrival_dest);
  cycle.read(address + ECAQ_OVERFLOW, EB_DATA32, &d_overflow_dest);
  cycle.read(address + ECAQ_QUEUED,   EB_DATA32, &d_queued);
  cycle.read(address + ECAQ_DROPPED,  EB_DATA32, &d_dropped);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  arrival_enable  = (d_int_mask & 1) != 0;
  overflow_enable = (d_int_mask & 2) != 0;
  arrival_dest    = d_arrival_dest  & 0xFFFFFFFF;
  overflow_dest   = d_overflow_dest & 0xFFFFFFFF;
  queued_actions  = d_queued        & 0xFFFFFFFF;
  dropped_actions = d_dropped       & 0xFFFFFFFF;
  
  return EB_OK;
}

status_t ActionQueue::reset() {
  Cycle cycle;
  eb_status_t status;
  
  if ((status = device.write(address + ECAQ_DROPPED, EB_DATA32|EB_BIG_ENDIAN, 0)) != EB_OK)
    return status;
  
  dropped_actions = 0;
  return EB_OK;
}

status_t ActionQueue::hook_arrival(bool enable, uint32_t dest) {
  Cycle cycle;
  eb_status_t status;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  /* Clear the interrupt, set the address, possibly re-enable interrupt */
  cycle.write(address + ECAQ_INT_MASK, EB_DATA32|EB_BIG_ENDIAN, overflow_enable?2:0);
  cycle.write(address + ECAQ_ARRIVAL,  EB_DATA32|EB_BIG_ENDIAN, dest);
  if (enable) {
    cycle.write(address + ECAQ_INT_MASK, EB_DATA32|EB_BIG_ENDIAN, overflow_enable?3:1);
  }
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  arrival_enable = enable;
  arrival_dest   = dest;
  
  return EB_OK;
}

status_t ActionQueue::hook_overflow(bool enable, uint32_t dest) {
  Cycle cycle;
  eb_status_t status;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  /* Clear the interrupt, set the address, possibly re-enable interrupt */
  cycle.write(address + ECAQ_INT_MASK, EB_DATA32|EB_BIG_ENDIAN, arrival_enable?1:0);
  cycle.write(address + ECAQ_OVERFLOW, EB_DATA32|EB_BIG_ENDIAN, dest);
  if (enable) {
    cycle.write(address + ECAQ_INT_MASK, EB_DATA32|EB_BIG_ENDIAN, arrival_enable?3:2);
  }
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  overflow_enable = enable;
  overflow_dest   = dest;
  
  return EB_OK;
}

status_t ActionQueue::pop(ActionEntry& queue) {
  Cycle cycle;
  eb_status_t  status;
  eb_data_t    queued, flags;
  eb_data_t    event1, event0;
  eb_data_t    param1, param0;
  eb_data_t    tag,    tef;
  eb_data_t    time1,  time0;
  
  if (queued_actions == 0) return EB_FAIL;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.read (address + ECAQ_FLAGS,  EB_DATA32, &flags);
  cycle.read (address + ECAQ_EVENT1, EB_DATA32, &event1);
  cycle.read (address + ECAQ_EVENT0, EB_DATA32, &event0);
  cycle.read (address + ECAQ_PARAM1, EB_DATA32, &param1);
  cycle.read (address + ECAQ_PARAM0, EB_DATA32, &param0);
  cycle.read (address + ECAQ_TAG,    EB_DATA32, &tag);
  cycle.read (address + ECAQ_TEF,    EB_DATA32, &tef);
  cycle.read (address + ECAQ_TIME1,  EB_DATA32, &time1);
  cycle.read (address + ECAQ_TIME0,  EB_DATA32, &time0);
  cycle.write(address + ECAQ_CTL,    EB_DATA32, 1); /* pop */
  cycle.read (address + ECAQ_QUEUED, EB_DATA32, &queued);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  queue.status =
    (flags & 2) != 0 ? CONFLICT :
    (flags & 1) != 0 ? LATE     :
    VALID;
  queue.event = event1; queue.event <<= 32; queue.event += event0;
  queue.param = param1; queue.param <<= 32; queue.param += param0;
  queue.tag   = tag;
  queue.tef   = tef;
  queue.time  = time1;  queue.time  <<= 32; queue.time  += time0;
  
  queued_actions = queued;
  
  return EB_OK;
}

}
