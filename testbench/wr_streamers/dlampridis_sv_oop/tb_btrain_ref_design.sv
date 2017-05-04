//-----------------------------------------------------------------------------
// Title      : Definitions for BTrain reference design testbench
// Project    : BTrain over White Rabbit
// URL        : https://wikis.cern.ch/display/HT/Btrain+over+White+Rabbit
//-----------------------------------------------------------------------------
// File       : tb_btrain_ref_design.sv
// Author(s)  : Dimitrios Lampridis <dimitrios.lampridis@cern.ch>
// Company    : CERN (BE-CO-HT)
// Created    : 2017-04-13
//-----------------------------------------------------------------------------
// Description:
// 
// SystemVerilog package with all definitions, interfaces, etc. necessary
// for the BTrain reference design testbench.
//
//-----------------------------------------------------------------------------
//
// Copyright (c) 2017 CERN
//
// This source file is free software; you can redistribute it
// and/or modify it under the terms of the GNU Lesser General
// Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any
// later version.
//
// This source is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU Lesser General Public License for more
// details.
//
// You should have received a copy of the GNU Lesser General
// Public License along with this source; if not, download it
// from http://www.gnu.org/licenses/lgpl-2.1.html
//

//-----------------------------------------------------------------------------


package PBTrainRefDesign;

`include "simdrv_defs.svh"
`include "wr_transmission_wb.svh"

class CWBPeriph;

   // Handle for all bus accesses
   CBusAccessor bus;

   // Offset in address map
   uint32_t base;
   
   function new(const ref CBusAccessor b, const ref uint32_t addr);
      bus  = b;
      base = addr;
   endfunction // new

   function uint32_t read32 (uint32_t offset);
      uint64_t rval;
      fork
	 bus.read(base + offset, rval, 4);
      join
      return uint32_t'(rval); 
   endfunction // read32
   
   task write32 (uint32_t offset, wval);
	 bus.write(base + offset, wval, 4);
   endtask // write32
    
endclass; // CWBPeriph
   
class CWRStreamer extends CWBPeriph;

   const uint32_t c_DUMMY_VAL = 'hDEADBEEF;
   
   function new(const ref CBusAccessor b, const ref uint32_t addr);
      super.new(b, addr);      
   endfunction // new

   function bit is_present ();
      uint32_t rval = read32(`ADDR_WR_TRANSMISSION_DUMMY);
      rval = (`WR_TRANSMISSION_DUMMY_DUMMY & rval) >> `WR_TRANSMISSION_DUMMY_DUMMY_OFFSET;
      if (c_DUMMY_VAL == rval)
	return 1;
      else
	return 0;      
   endfunction // is_present
   
endclass; // CWRStreamer

class CWRBTrain extends CWBPeriph;

   function new(const ref CBusAccessor b, const ref uint32_t addr);
      super.new(b, addr);      
   endfunction // new
   
endclass; // CWRBTrain
   
class CBTrainRefDesign;

   // base offsets/pointers for WR peripherals
   const uint32_t c_BASE_WRPC        = 'h00040000;
   const uint32_t c_BASE_AUX_IN_WRPC = 'h00020700;
   const uint32_t c_BASE_WRSTREAMERS = c_BASE_WRPC + c_BASE_AUX_IN_WRPC;
   const uint32_t c_BASE_WRBTRAIN    = 'h00001200;
   
   // Handles to WR streamer and BTrain WB peripherals
   CWRStreamer wrstream;
   CWRBTrain   wrbtrain;
   
   function new(CBusAccessor b);
      wrstream = new(b, c_BASE_WRSTREAMERS);
      wrbtrain = new(b, c_BASE_WRBTRAIN);      
   endfunction // new

endclass // CBTRainRefDesign
   
endpackage //PBTrainRefDesign
  
