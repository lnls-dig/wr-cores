-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint - top level
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : xwr_endpoint.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-04-26
-- Last update: 2012-11-16
-- Platform   : FPGA-generic
-- Standard   : VHDL '93
-------------------------------------------------------------------------------
-- Description: Struct-ized wrapper for WR Endpoint.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2011 - 2012 CERN / BE-CO-HT
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
use ieee.numeric_std.all;

library work;

use work.endpoint_pkg.all;
use work.wr_fabric_pkg.all;
use work.wishbone_pkg.all;

entity xwr_endpoint is
  
  generic (
    g_interface_mode        : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity   : t_wishbone_address_granularity := WORD;
    g_simulation            : boolean                        := false;
    g_tx_force_gap_length   : integer                        := 0;
    g_pcs_16bit             : boolean                        := false;
    g_rx_buffer_size        : integer                        := 1024;
    g_with_rx_buffer        : boolean                        := true;
    g_with_flow_control     : boolean                        := true;
    g_with_timestamper      : boolean                        := true;
    g_with_dpi_classifier   : boolean                        := true;
    g_with_vlans            : boolean                        := true;
    g_with_rtu              : boolean                        := true;
    g_with_leds             : boolean                        := true;
    g_with_dmtd             : boolean                        := true;
    g_with_packet_injection : boolean                        := false
    );
  port (

-------------------------------------------------------------------------------
-- Clocks
-------------------------------------------------------------------------------

-- Endpoint transmit reference clock. Must be 125 MHz +- 100 ppm
    clk_ref_i : in std_logic;

-- reference clock / 2 (62.5 MHz, in-phase with refclk)
    clk_sys_i : in std_logic;

    clk_dmtd_i : in std_logic := '0';

-- sync reset (clk_sys_i domain), active LO
    rst_n_i : in std_logic;

-- PPS input (1 clk_ref_i cycle HI) for synchronizing timestamp counter
    pps_csync_p1_i : in std_logic := '0';

-- PPS valid input (clk_ref_i domain), when 1, the external PPS generator/servo
-- is not adjusting the time scale, so we can safely timestamp.
    pps_valid_i : in std_logic := '1';

-------------------------------------------------------------------------------
-- PHY Interace (8/16 bit PCS)
-------------------------------------------------------------------------------    

    phy_rst_o    : out std_logic;
    phy_loopen_o : out std_logic;
    phy_enable_o : out std_logic;
    phy_syncen_o : out std_logic;

    phy_ref_clk_i      : in  std_logic := '0';
    phy_tx_data_o      : out std_logic_vector(15 downto 0);
    phy_tx_k_o         : out std_logic_vector(1 downto 0);
    phy_tx_disparity_i : in  std_logic := '0';
    phy_tx_enc_err_i   : in  std_logic := '0';

    phy_rx_data_i     : in std_logic_vector(15 downto 0) := x"0000";
    phy_rx_clk_i      : in std_logic                     := '0';
    phy_rx_k_i        : in std_logic_vector(1 downto 0)  := "00";
    phy_rx_enc_err_i  : in std_logic                     := '0';
    phy_rx_bitslide_i : in std_logic_vector(4 downto 0)  := "00000";

-------------------------------------------------------------------------------
-- GMII Interface (8-bit)
-------------------------------------------------------------------------------

    gmii_tx_clk_i : in  std_logic := '0';
    gmii_txd_o    : out std_logic_vector(7 downto 0);
    gmii_tx_en_o  : out std_logic;
    gmii_tx_er_o  : out std_logic;

    gmii_rx_clk_i : in std_logic                    := '0';
    gmii_rxd_i    : in std_logic_vector(7 downto 0) := x"00";
    gmii_rx_er_i  : in std_logic                    := '0';
    gmii_rx_dv_i  : in std_logic                    := '0';

    ---------------------------------------------------------------------------
    -- Wishbone I/O
    ---------------------------------------------------------------------------

    src_o : out t_wrf_source_out;
    src_i : in  t_wrf_source_in;

    snk_o : out t_wrf_sink_out;
    snk_i : in  t_wrf_sink_in;

-------------------------------------------------------------------------------
-- TX timestamping unit interface
-------------------------------------------------------------------------------  

-- Port ID value
    txtsu_port_id_o  : out std_logic_vector(4 downto 0);
-- Frame ID value
    txtsu_frame_id_o : out std_logic_vector(16 -1 downto 0);

-- TX Timestamp and correctness info
    txtsu_ts_value_o     : out std_logic_vector(28 + 4 - 1 downto 0);
    txtsu_ts_incorrect_o : out std_logic;

-- TX timestamp strobe: HI tells the TX timestamping unit that a timestamp is
-- available on txtsu_ts_value_o, txtsu_fid_o andd txtsu_port_id_o. The correctness
-- of the timestamping is indiacted on txtsu_ts_incorrect_o. Line remains HI
-- until assertion of txtsu_ack_i.
    txtsu_stb_o : out std_logic;

-- TX timestamp acknowledge: HI indicates that TXTSU has successfully received
-- the timestamp
    txtsu_ack_i : in std_logic := '1';

-------------------------------------------------------------------------------
-- RTU interface
-------------------------------------------------------------------------------

-- 1 indicates that coresponding RTU port is full.
    rtu_full_i : in std_logic := '0';

-- 1 indicates that coresponding RTU port is almost full.
    rtu_almost_full_i : in std_logic := '0';

-- request strobe, single HI pulse begins evaluation of the request. 
    rtu_rq_strobe_p1_o : out std_logic;

-- source and destination MAC addresses extracted from the packet header
    rtu_rq_smac_o : out std_logic_vector(48 - 1 downto 0);
    rtu_rq_dmac_o : out std_logic_vector(48 - 1 downto 0);

-- VLAN id (extracted from the header for TRUNK ports and assigned by the port
-- for ACCESS ports)
    rtu_rq_vid_o : out std_logic_vector(12 - 1 downto 0);

-- HI means that packet has valid assigned a valid VID (low - packet is untagged)
    rtu_rq_has_vid_o : out std_logic;

-- packet priority (either extracted from the header or assigned per port).
    rtu_rq_prio_o : out std_logic_vector(3 - 1 downto 0);

-- HI indicates that packet has assigned priority.
    rtu_rq_has_prio_o : out std_logic;

-------------------------------------------------------------------------------   
-- Wishbone bus
-------------------------------------------------------------------------------

    wb_i : in  t_wishbone_slave_in;
    wb_o : out t_wishbone_slave_out;

-------------------------------------------------------------------------------
-- direct output of packet filter  (for TRU/HW-RSTP)
-------------------------------------------------------------------------------
   
   pfilter_pclass_o       : out   std_logic_vector(7 downto 0);
   pfilter_drop_o         : out   std_logic;
   pfilter_done_o         : out   std_logic;

-------------------------------------------------------------------------------
-- control of PAUSE sending (ML: not used and not tested... TRU uses packet injection) -- 
-------------------------------------------------------------------------------
   
   fc_tx_pause_req_i   : in std_logic := '0';
   fc_tx_pause_delay_i : in std_logic_vector(15 downto 0) := x"0000";
   fc_tx_pause_ready_o : out std_logic;

-------------------------------------------------------------------------------
-- information about received PAUSE (for SWcore)
-------------------------------------------------------------------------------

   fc_rx_pause_start_p_o     : out std_logic;
   fc_rx_pause_quanta_o      : out std_logic_vector(15 downto 0);
   fc_rx_pause_prio_mask_o   : out std_logic_vector(7 downto 0);
   fc_rx_buffer_occupation_o : out std_logic_vector(7 downto 0);
-------------------------------------------------------------------------------
-- Packet Injection Interface (for TRU/HW-RSTP)
-------------------------------------------------------------------------------

-- injection request: triggers transmission of the packet to be injected,
-- allowed when inject_ready = 1
    inject_req_i : in std_logic := '0';

-- injection ready flag: when true, user application can request asynchronous
-- injection of a predefined packet
    inject_ready_o : out std_logic;

-- injection template selection (8 available)
    inject_packet_sel_i : in std_logic_vector(2 downto 0) := "000";

-- user-defined value to be embedded in the injected packet at a predefined
-- location
    inject_user_value_i : in std_logic_vector(15 downto 0) := x"0000";

-------------------------------------------------------------------------------
-- Misc stuff
-------------------------------------------------------------------------------
    rmon_events_o        : out std_logic_vector(c_epevents_sz-1 downto 0);

    led_link_o : out std_logic;
    led_act_o  : out std_logic;

    link_kill_i : in  std_logic := '0';
    link_up_o   : out std_logic;
    dbg_o       : out std_logic_vector(63 downto 0);
    dbg_tx_pcs_wr_count_o     : out std_logic_vector(5+4 downto 0);
    dbg_tx_pcs_rd_count_o     : out std_logic_vector(5+4 downto 0)
    );

end xwr_endpoint;

architecture syn of xwr_endpoint is

begin

  U_Wrapped_Endpoint : wr_endpoint
    generic map (
      g_interface_mode      => g_interface_mode,
      g_address_granularity => g_address_granularity,
      g_tx_force_gap_length => g_tx_force_gap_length,

      g_simulation            => g_simulation,
      g_pcs_16bit             => g_pcs_16bit,
      g_rx_buffer_size        => g_rx_buffer_size,
      g_with_rx_buffer        => g_with_rx_buffer,
      g_with_flow_control     => g_with_flow_control,
      g_with_timestamper      => g_with_timestamper,
      g_with_dpi_classifier   => g_with_dpi_classifier,
      g_with_vlans            => g_with_vlans,
      g_with_rtu              => g_with_rtu,
      g_with_leds             => g_with_leds,
      g_with_dmtd             => g_with_dmtd,
      g_with_packet_injection => g_with_packet_injection)
    port map (
      clk_ref_i            => clk_ref_i,
      clk_sys_i            => clk_sys_i,
      clk_dmtd_i           => clk_dmtd_i,
      rst_n_i              => rst_n_i,
      pps_csync_p1_i       => pps_csync_p1_i,
      pps_valid_i          => pps_valid_i,
      phy_rst_o            => phy_rst_o,
      phy_loopen_o         => phy_loopen_o,
      phy_enable_o         => phy_enable_o,
      phy_syncen_o         => phy_syncen_o,
      phy_ref_clk_i        => phy_ref_clk_i,
      phy_tx_data_o        => phy_tx_data_o,
      phy_tx_k_o           => phy_tx_k_o,
      phy_tx_disparity_i   => phy_tx_disparity_i,
      phy_tx_enc_err_i     => phy_tx_enc_err_i,
      phy_rx_data_i        => phy_rx_data_i,
      phy_rx_clk_i         => phy_rx_clk_i,
      phy_rx_k_i           => phy_rx_k_i,
      phy_rx_enc_err_i     => phy_rx_enc_err_i,
      phy_rx_bitslide_i    => phy_rx_bitslide_i,
      gmii_tx_clk_i        => gmii_tx_clk_i,
      gmii_txd_o           => gmii_txd_o,
      gmii_tx_en_o         => gmii_tx_en_o,
      gmii_tx_er_o         => gmii_tx_er_o,
      gmii_rx_clk_i        => gmii_rx_clk_i,
      gmii_rxd_i           => gmii_rxd_i,
      gmii_rx_er_i         => gmii_rx_er_i,
      gmii_rx_dv_i         => gmii_rx_dv_i,
      src_dat_o            => src_o.dat,
      src_adr_o            => src_o.adr,
      src_sel_o            => src_o.sel,
      src_cyc_o            => src_o.cyc,
      src_stb_o            => src_o.stb,
      src_we_o             => src_o.we,
      src_stall_i          => src_i.stall,
      src_ack_i            => src_i.ack,
      src_err_i            => src_i.err,
      snk_dat_i            => snk_i.dat,
      snk_adr_i            => snk_i.adr,
      snk_sel_i            => snk_i.sel,
      snk_cyc_i            => snk_i.cyc,
      snk_stb_i            => snk_i.stb,
      snk_we_i             => snk_i.we,
      snk_stall_o          => snk_o.stall,
      snk_ack_o            => snk_o.ack,
      snk_err_o            => snk_o.err,
      snk_rty_o            => snk_o.rty,
      txtsu_port_id_o      => txtsu_port_id_o,
      txtsu_frame_id_o     => txtsu_frame_id_o,
      txtsu_ts_value_o     => txtsu_ts_value_o,
      txtsu_ts_incorrect_o => txtsu_ts_incorrect_o,
      txtsu_stb_o          => txtsu_stb_o,
      txtsu_ack_i          => txtsu_ack_i,
      rtu_full_i           => rtu_full_i,
      rtu_almost_full_i    => rtu_almost_full_i,
      rtu_rq_strobe_p1_o   => rtu_rq_strobe_p1_o,
      rtu_rq_smac_o        => rtu_rq_smac_o,
      rtu_rq_dmac_o        => rtu_rq_dmac_o,
      rtu_rq_vid_o         => rtu_rq_vid_o,
      rtu_rq_has_vid_o     => rtu_rq_has_vid_o,
      rtu_rq_prio_o        => rtu_rq_prio_o,
      rtu_rq_has_prio_o    => rtu_rq_has_prio_o,
      wb_cyc_i             => wb_i.cyc,
      wb_stb_i             => wb_i.stb,
      wb_we_i              => wb_i.we,
      wb_sel_i             => wb_i.sel,
      wb_adr_i             => wb_i.adr(7 downto 0),
      wb_dat_i             => wb_i.dat,
      wb_dat_o             => wb_o.dat,
      wb_ack_o             => wb_o.ack,
      wb_stall_o           => wb_o.stall,
      rmon_events_o        => rmon_events_o,
      led_link_o           => led_link_o,
      led_act_o            => led_act_o,
      link_up_o            => link_up_o,
      link_kill_i          => link_kill_i,
      pfilter_pclass_o     => pfilter_pclass_o,
      pfilter_drop_o       => pfilter_drop_o,
      pfilter_done_o       => pfilter_done_o,
      fc_tx_pause_req_i    => fc_tx_pause_req_i,
      fc_tx_pause_delay_i  => fc_tx_pause_delay_i,
      fc_tx_pause_ready_o  => fc_tx_pause_ready_o,
      fc_rx_pause_start_p_o   => fc_rx_pause_start_p_o,
      fc_rx_pause_quanta_o    => fc_rx_pause_quanta_o,
      fc_rx_pause_prio_mask_o => fc_rx_pause_prio_mask_o,
      fc_rx_buffer_occupation_o =>fc_rx_buffer_occupation_o,
      inject_req_i         => inject_req_i,
      inject_user_value_i  => inject_user_value_i,
      inject_packet_sel_i  => inject_packet_sel_i,
      inject_ready_o       => inject_ready_o,
      dbg_o                => dbg_o,
      dbg_tx_pcs_wr_count_o=>dbg_tx_pcs_wr_count_o,
      dbg_tx_pcs_rd_count_o=>dbg_tx_pcs_rd_count_o);

  wb_o.err <= '0';
  wb_o.rty <= '0';
  wb_o.int <= '0';
  
end syn;


