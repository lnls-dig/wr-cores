//-----------------------------------------------------------------------------
// Title      : Testbench for verification of BTrain SPEC reference design
// Project    : Btrain
//-----------------------------------------------------------------------------
// File       : main.sv
// Author(s)  : Maciej Lipinski <maciej.lipinski@cern.ch>
//              Dimitrios Lampridis <dimitrios.lampridis@cern.ch>
// Company    : CERN (BE-CO-HT)
// Created    : 2016-05-30
//-----------------------------------------------------------------------------
// Description:
// 
// Simulation of two interconnected SPEC reference design modules (A and B):
// - both modules send and receive Bframes
// - Module A receives up/down/C0_reset pulses
// 
// Please, note that the initialization of LM32 and the software takes 
// some time, allow the simulation to run for ~5 minutes (at least 105us
// of simulation time)
//-----------------------------------------------------------------------------
//
// Copyright (c) 2016-2017 CERN
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

`timescale 1ns/1ps

`include "gn4124_bfm.svh"

import PBTrainRefDesign::*;
  
module main;

   bit clk_125m_pllref = 0;
   bit clk_20m_vcxo = 0;
   
   always #4ns clk_125m_pllref <= ~clk_125m_pllref;
   always #20ns clk_20m_vcxo <= ~clk_20m_vcxo;

   IGN4124PCIMaster I_GennumA ();
   IGN4124PCIMaster I_GennumB ();

  /// ///////////////////////////////////////////////////////////////////////////////////////
  /// Instantiation of two interconnected SPECs 
  /// ///////////////////////////////////////////////////////////////////////////////////////

   BtrainSpecTop
     #(
       .g_simulation (1),
       .g_dpram_initf("../../ip_cores/wr-cores/bin/wrpc/wrc_phy8_sim.bram")
       )
   DUT_SPEC_A 
     (
      .clk_125m_pllref_p_i(clk_125m_pllref),
      .clk_125m_pllref_n_i(~clk_125m_pllref),

      .clk_125m_gtp_p_i(clk_125m_pllref),
      .clk_125m_gtp_n_i(~clk_125m_pllref),

      .clk_20m_vcxo_i(clk_20m_vcxo),

      .sfp_txp_o(a_to_b_p_o),
      .sfp_txn_o(a_to_b_n),

      .sfp_rxp_i(b_to_a_p_i),
      .sfp_rxn_i(b_to_a_n),

      `GENNUM_WIRE_SPEC_PINS_WITH_PROPER_NAMING(I_GennumA)
      );

   BtrainSpecTop
     #(
       .g_simulation (1),
       .g_dpram_initf("../../ip_cores/wr-cores/bin/wrpc/wrc_phy8_sim.bram")
       )
   DUT_SPEC_B 
     (
      .clk_125m_pllref_p_i(clk_125m_pllref),
      .clk_125m_pllref_n_i(~clk_125m_pllref),
      
      .clk_125m_gtp_p_i(clk_125m_pllref),
      .clk_125m_gtp_n_i(~clk_125m_pllref),
      
      .clk_20m_vcxo_i(clk_20m_vcxo),

      .sfp_txp_o(b_to_a_p_o),
      .sfp_txn_o(b_to_a_n),
      
      .sfp_rxp_i(a_to_b_p_i),
      .sfp_rxn_i(a_to_b_n),
      
      `GENNUM_WIRE_SPEC_PINS_WITH_PROPER_NAMING(I_GennumB)
      );
   
   CBTrainRefDesign ModA = new(I_GennumA.get_accessor());
   CBTrainRefDesign ModB = new(I_GennumB.get_accessor());
   
   initial begin
      
      $display("test %d", ModA.wrstream.is_present);
      
      $finish;
      
   end // initial begin
   
endmodule // main


