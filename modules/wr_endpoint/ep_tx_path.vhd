-------------------------------------------------------------------------------
-- Title      : Complete Transmit Path
-- Project    : White Rabbit MAC/Endpoint
-------------------------------------------------------------------------------
-- File       : ep_tx_path.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2012-11-01
-- Last update: 2012-11-16
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Full transmit path of the endpoint (excluding the PCS).
-- Chains the wishbone interface and header processor, followed by the VLAN unit
-- (optional), packet injector (optional) and CRC inserter.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2009 - 2012 CERN / BE-CO-HT
--
-- This source file is free software; you can redistribute it   
-- and/or modify it under the terms of the GNU Lesser General   
-- Public License as published by the Free Software Foundation; 
-- either version 2.1 of the License, or (at your option) any   
-- later version.                                               
--
-- This source is distributed in the hope that it will be       
-- useful, but WITHOUT ANY WARRANTY; without even the implied   
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      
-- PURPOSE.  See the GNU Lesser General Public License for more 
-- details.                                                     
--
-- You should have received a copy of the GNU Lesser General    
-- Public License along with this source; if not, download it   
-- from http://www.gnu.org/licenses/lgpl-2.1.html
--
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;

library work;
use work.wr_fabric_pkg.all;
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;

entity ep_tx_path is

  generic(
    g_with_vlans            : boolean;
    g_with_timestamper      : boolean;
    g_with_packet_injection : boolean;
    g_force_gap_length      : integer
    );

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

------------------------------------------------------------------------------
-- Physical Coding Sublayer (PCS) interface
------------------------------------------------------------------------------

    pcs_fab_o   : out t_ep_internal_fabric;
    pcs_error_i : in  std_logic;
    pcs_busy_i  : in  std_logic;
    pcs_dreq_i  : in  std_logic;

-------------------------------------------------------------------------------
-- WRF Sink (see WRF specification for the details)
-------------------------------------------------------------------------------    

    snk_i : in  t_wrf_sink_in;
    snk_o : out t_wrf_sink_out;

-------------------------------------------------------------------------------
-- Flow Control Unit signals
-------------------------------------------------------------------------------    

-- TX send pause frame - when active, the framer will send a PAUSE frame
-- as soon as possible. The pause quanta must be provided on tx_pause_delay_i input.
    fc_pause_req_i   : in std_logic;
    fc_pause_delay_i : in std_logic_vector(15 downto 0);

-- TX send pause acknowledge - active after the current pause send request has
-- been completed
    fc_pause_ready_o : out std_logic;

-- When active, the framer will allow for packet transmission.
    fc_flow_enable_i : in std_logic;

-------------------------------------------------------------------------------
-- OOB/TSU signals
-------------------------------------------------------------------------------    

-- Port ID value
    txtsu_port_id_o      : out std_logic_vector(4 downto 0);
-- Frame ID value
    txtsu_fid_o          : out std_logic_vector(16 -1 downto 0);
-- Encoded timestamps
    txtsu_ts_value_o     : out std_logic_vector(28 + 4 - 1 downto 0);
    txtsu_ts_incorrect_o : out std_logic;

-- TX timestamp strobe: HI tells the TX timestamping unit that a timestamp is
-- available on txtsu_ts_value_o, txtsu_fid_o andd txtsu_port_id_o. The correctness
-- of the timestamping is indiacted on txtsu_ts_incorrect_o. Line remains HI
-- until assertion of txtsu_ack_i.
    txtsu_stb_o : out std_logic;

-- TX timestamp acknowledge: HI indicates that TXTSU has successfully received
-- the timestamp
    txtsu_ack_i : in std_logic;

------------------------------------------------------------------------------
-- Timestamp input from the timestamping unit
------------------------------------------------------------------------------
    txts_timestamp_i       : in std_logic_vector(31 downto 0);
    txts_timestamp_valid_i : in std_logic;

------------------------------------------------------------------------------
-- Packet Injector (for TRU & HW-RSTP)
------------------------------------------------------------------------------

    inject_req_i        : in  std_logic                     := '0';
    inject_ready_o      : out std_logic;
    inject_packet_sel_i : in  std_logic_vector(2 downto 0)  := "000";
    inject_user_value_i : in  std_logic_vector(15 downto 0) := x"0000";


-------------------------------------------------------------------------------
-- Control registers
-------------------------------------------------------------------------------
    ep_ctrl_i           : in std_logic :='1';
    regs_i : in t_ep_out_registers;
    dbg_o          : out std_logic_vector(11 downto 0)
    );


end ep_tx_path;

architecture rtl of ep_tx_path is

  type t_fab_pipe is array(integer range <>) of t_ep_internal_fabric;

  signal fab_pipe  : t_fab_pipe(3 downto 0);
  signal dreq_pipe : std_logic_vector(3 downto 0);

  signal vlan_mem_addr : std_logic_vector(9 downto 0);
  signal vlan_mem_data : std_logic_vector(17 downto 0);
  
begin  -- rtl

  U_Header_Processor : ep_tx_header_processor
    generic map (
      g_with_packet_injection => g_with_packet_injection,
      g_with_timestamper      => g_with_timestamper,
      g_force_gap_length      => g_force_gap_length)
    port map (
      clk_sys_i              => clk_sys_i,
      rst_n_i                => rst_n_i,
      src_fab_o              => fab_pipe(0),
      src_dreq_i             => dreq_pipe(0),
      pcs_busy_i             => pcs_busy_i,
      pcs_error_i            => pcs_error_i,
      wb_snk_i               => snk_i,
      wb_snk_o               => snk_o,
      fc_pause_req_i         => fc_pause_req_i,
      fc_pause_delay_i       => fc_pause_delay_i,
      fc_pause_ready_o       => fc_pause_ready_o,
      fc_flow_enable_i       => fc_flow_enable_i,
      txtsu_port_id_o        => txtsu_port_id_o,
      txtsu_fid_o            => txtsu_fid_o,
      txtsu_ts_value_o       => txtsu_ts_value_o,
      txtsu_ts_incorrect_o   => txtsu_ts_incorrect_o,
      txtsu_stb_o            => txtsu_stb_o,
      txtsu_ack_i            => txtsu_ack_i,
      txts_timestamp_i       => txts_timestamp_i,
      txts_timestamp_valid_i => txts_timestamp_valid_i,
      ep_ctrl_i              => ep_ctrl_i,
      regs_i                 => regs_i);


  assert not (g_with_packet_injection and not g_with_vlans)
    report "wr_endpoint: packet injection requires VLAN support to be enabled" severity failure;

  gen_with_vlan_unit : if(g_with_vlans) generate
    U_VLAN_Unit : ep_tx_vlan_unit
      port map (
        clk_sys_i         => clk_sys_i,
        rst_n_i           => rst_n_i,
        snk_fab_i         => fab_pipe(0),
        snk_dreq_o        => dreq_pipe(0),
        src_fab_o         => fab_pipe(1),
        src_dreq_i        => dreq_pipe(1),
        inject_mem_addr_i => vlan_mem_addr,
        inject_mem_data_o => vlan_mem_data,
        regs_i            => regs_i);
  end generate gen_with_vlan_unit;

  gen_without_vlan_unit : if(not g_with_vlans) generate
    fab_pipe(1)  <= fab_pipe(0);
    dreq_pipe(0) <= dreq_pipe(1);
  end generate gen_without_vlan_unit;

  gen_with_injection : if(g_with_packet_injection) generate
    U_Injector : ep_tx_packet_injection
      port map (
        clk_sys_i           => clk_sys_i,
        rst_n_i             => rst_n_i,
        snk_fab_i           => fab_pipe(1),
        snk_dreq_o          => dreq_pipe(1),
        src_fab_o           => fab_pipe(2),
        src_dreq_i          => dreq_pipe(2),
        inject_req_i        => inject_req_i,
        inject_ready_o      => inject_ready_o,
        inject_packet_sel_i => inject_packet_sel_i,
        inject_user_value_i => inject_user_value_i,
        mem_addr_o          => vlan_mem_addr,
        mem_data_i          => vlan_mem_data);
  end generate gen_with_injection;

  gen_without_injection : if (not g_with_packet_injection) generate
    fab_pipe(2)  <= fab_pipe(1);
    dreq_pipe(1) <= dreq_pipe(2);
  end generate gen_without_injection;

  U_Insert_CRC : ep_tx_crc_inserter
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      snk_fab_i  => fab_pipe(2),
      snk_dreq_o => dreq_pipe(2),
      src_fab_o  => fab_pipe(3),
      src_dreq_i => dreq_pipe(3));

  pcs_fab_o    <= fab_pipe(3);
  dreq_pipe(3) <= pcs_dreq_i;

  GEN_DBG: for i in 0 to 3 generate
    dbg_o(i)    <= fab_pipe(i).sof;
    dbg_o(i+4)  <= fab_pipe(i).eof;
    dbg_o(i+8)  <= dreq_pipe(i);
  end generate GEN_DBG;

end rtl;
