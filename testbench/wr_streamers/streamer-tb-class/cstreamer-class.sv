//-----------------------------------------------------------------------------
// Title      : Definitions for WR streamers testbench
// Project    : White Rabbit Cores
// URL        : http://www.ohwr.org/projects/wr-cores/wiki/WR_Streamers
//-----------------------------------------------------------------------------
// File       : wr-streamers-tb-class.sv
// Author(s)  : Denia Bouhired <denia.bouhired@cern.ch>
// Company    : CERN (BE-CO-HT)
// Created    : 2017-04-28
//-----------------------------------------------------------------------------
// Description:
// 
// SystemVerilog package with all definitions, interfaces, etc. necessary
// for the wr streamers testbench.
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


package wr-streamers-tb-pkg;


class CWRSTREAMERS;
 int data_width
 bit disable_escape_code;

    //Class constructor
    function new ( input int width = 32, bit dis_esc = 0);
        data_width = width;
        disable_escape_code = dis_esc;
    endfunction
    
endclass;


//Transmitter class
class CWRSTREAMERS_TX extends CWRSTREAMERS;

    function 

endclass;


//Receiver class
class CWRSTREAMERS_RX extends CWRSTREAMERS;



endclass;

