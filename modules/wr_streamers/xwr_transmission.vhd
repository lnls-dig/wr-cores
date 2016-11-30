-------------------------------------------------------------------------------
-- Title      : Btrain over White Rabbit
-- Project    : Btrain
-------------------------------------------------------------------------------
-- File       : xwr_transmission.vhd
-- Author     : Maciej Lipinski
-- Company    : CERN
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
--
-- This module wraps WR_Streamers-related stuff: i.e.
-- 1) IP core modules provided in wr-cores: xtx_streamer, xrx_streamer, 
--    xrtx_streamers_stats
-- 2) wishbone registers that provide access to the statistics and streamer's
--    control/status registers.
--
-- This module interfaces:
-- 1) WR PTP Core for transmission/reception of raw ethernet frames
-- 2) Application-specific module for transmission/reception of data
-------------------------------------------------------------------------------
--
-- Copyright (c) 2016 CERN/BE-CO-HT
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
-- Revisions  :
-- Date        Version  Author          Description
-- 2016-05-30  1.0      mlipinsk        created
---------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.wishbone_pkg.all;  -- needed for t_wishbone_slave_in, etc
use work.streamers_pkg.all; -- needed for streamers and  c_WR_TRANS_ARR_SIZE_*
use work.wr_fabric_pkg.all; -- needed for :t_wrf_source_in, etc
use work.wrcore_pkg.all;    -- needed for t_generic_word_array
use work.wr_transmission_wbgen2_pkg.all;

entity xwr_transmission is
  generic (
    -----------------------------------------------------------------------------------------
    -- Transmission (tx)
    -----------------------------------------------------------------------------------------
    -- Width of data words on tx_data_i.
    g_tx_data_width           : integer := 32;

    -- Minimum number of data words in the TX buffer that will trigger transmission of an
    -- Ethernet frame. Also defines the buffer size (2 * g_tx_threshold). Note
    -- that in order for a frame to be transmitted, the buffer must conatain at
    -- least one complete block.
    g_tx_threshold            : integer := 128;

    -- Maximum number of data words in a single Ethernet frame. It also defines
    -- the maximum block size (since blocks can't be currently split across
    -- multiple frames). 
    g_tx_max_words_per_frame  : integer := 128;

    -- Transmission timeout (in clk_sys_i cycles), after which the contents
    -- of TX buffer are sent regardless of the amount of data that is currently
    -- stored in the buffer, so that data in the buffer does not get stuck.
    g_tx_timeout               : integer := 1024;

    -- DO NOT USE unless you know what you are doing
    -- legacy stuff: the streamers initially used in Btrain did not check/insert the escape
    -- code. This is justified if only one block of a known number of words is sent/expected
    g_tx_escape_code_disable   : boolean := FALSE;

    -----------------------------------------------------------------------------------------
    -- Reception (rx)
    -----------------------------------------------------------------------------------------
    -- Width of the data words. Must be same as in the TX streamer.
    g_rx_data_width            : integer := 32;

    -- Size of RX buffer, in data words.
    g_rx_buffer_size           : integer := 16;

    -- DO NOT USE unless you know what you are doing
    -- legacy stuff: the streamers that were initially used in Btrain did not check/insert 
    -- the escape code. This is justified if only one block of a known number of words is 
    -- sent/expected.
    g_rx_escape_code_disable   : boolean := FALSE;

    -- DO NOT USE unless you know what you are doing
    -- legacy stuff: the streamers that were initially used in Btrain accepted only a fixed
    -- number of words, regardless of the frame content. If this generic is set to number
    -- other than zero, only a fixed number of words is accepted. 
    -- In combination with the g_escape_code_disable generic set to TRUE, the behaviour of
    -- the "Btrain streamers" can be recreated.
    g_rx_expected_words_number : integer := 0;

    -----------------------------------------------------------------------------------------
    -- Statistics config
    -----------------------------------------------------------------------------------------
    -- width of counters: frame rx/tx/lost, block lost, counter of accumuted latency
    -- (minimum 15 bits, max 32)
    g_stats_cnt_width          : integer := 32;
    -- width of latency accumulator (max value 64)
    g_stats_acc_width          : integer := 64;
    -----------------------------------------------------------------------------------------
    -- WB I/F configuration
    -----------------------------------------------------------------------------------------
    g_slave_mode               : t_wishbone_interface_mode      := CLASSIC;
    g_slave_granularity        : t_wishbone_address_granularity := BYTE
    );

  port (
    clk_sys_i                  : in std_logic;
    rst_n_i                    : in std_logic;

    ---------------------------------------------------------------------------
    -- WR tx/rx interface
    ---------------------------------------------------------------------------
    -- Tx
    src_i                      : in  t_wrf_source_in;
    src_o                      : out t_wrf_source_out;
    -- Rx
    snk_i                      : in  t_wrf_sink_in;
    snk_o                      : out t_wrf_sink_out;

    ---------------------------------------------------------------------------
    -- User tx interface
    ---------------------------------------------------------------------------
    -- Data word to be sent.
    tx_data_i                  : in std_logic_vector(g_tx_data_width-1 downto 0);
    -- 1 indicates that the tx_data_i contains a valid data word.
    tx_valid_i                 : in std_logic;
    -- Synchronous data request: if active, the user may send a data word in
    -- the following clock cycle.
    tx_dreq_o                  : out std_logic;
    -- Last signal. Can be used to indicate the last data word in a larger
    -- block of samples (see documentation for more details).
    tx_last_p1_i               : in std_logic := '1';
    -- Flush input. When asserted, the streamer will immediatly send out all
    -- the data that is stored in its TX buffer, ignoring g_tx_timeout.
    tx_flush_p1_i              : in std_logic := '0';

    ---------------------------------------------------------------------------
    -- User rx interface
    ---------------------------------------------------------------------------
    -- 1 indicates the 1st word of the data block on rx_data_o.
    rx_first_p1_o              : out std_logic;
    -- 1 indicates the last word of the data block on rx_data_o.
    rx_last_p1_o               : out std_logic;
    -- Received data.
    rx_data_o                  : out std_logic_vector(g_rx_data_width-1 downto 0);
    -- 1 indicted that rx_data_o is outputting a valid data word.
    rx_valid_o                 : out std_logic;
    -- Synchronous data request input: when 1, the streamer may output another
    -- data word in the subsequent clock cycle.
    rx_dreq_i                  : in  std_logic;

    ---------------------------------------------------------------------------
    -- WRC Timing interface, used for latency measurement
    ---------------------------------------------------------------------------

    -- White Rabbit reference clock
    clk_ref_i                  : in std_logic := '0';
    -- Time valid flag
    tm_time_valid_i            : in std_logic := '0';
    -- TAI seconds
    tm_tai_i                   : in std_logic_vector(39 downto 0) := x"0000000000";
    -- Fractional part of the second (in clk_ref_i cycles)
    tm_cycles_i                : in std_logic_vector(27 downto 0) := x"0000000";

    -- wishbone interface 
    wb_slave_i                 : in  t_wishbone_slave_in := cc_dummy_slave_in;
    wb_slave_o                 : out t_wishbone_slave_out;

    snmp_array_o               : out t_generic_word_array(c_WR_TRANS_ARR_SIZE_OUT-1 downto 0);
    snmp_array_i               : in  t_generic_word_array(c_WR_TRANS_ARR_SIZE_IN -1 downto 0);

    -----------------------------------------------------------------------------------------
    -- Transmission (tx) configuration
    -----------------------------------------------------------------------------------------
    -- Local MAC address. Leave at 0x0...0 when using with the WR MAC/Core, it will
    -- insert its own source MAC.
    tx_cfg_mac_local_i         : in std_logic_vector(47 downto 0) := x"000000000000";
    -- Destination MAC address, i.e. MAC of a device to which data is streamed.
    tx_cfg_mac_target_i        : in std_logic_vector(47 downto 0):= x"ffffffffffff";
    -- Ethertype of our frames. Default value is accepted by standard
    -- configuration of the WR PTP Core
    tx_cfg_ethertype_i         : in std_logic_vector(15 downto 0) := x"dbff";

    -----------------------------------------------------------------------------------------
    -- Reception (rx)configuration
    -----------------------------------------------------------------------------------------
    -- Local MAC address. Leave at 0x0...0 when using with the WR MAC/Core, it will
    -- insert its own source MAC.
    rx_cfg_mac_local_i         : in std_logic_vector(47 downto 0) := x"000000000000";
    -- Remote MAC address, i.e. MAC of the device from which the data should be accpated
    rx_cfg_mac_remote_i        : in std_logic_vector(47 downto 0) := x"000000000000";
    -- Ethertype of our frames. Default value is accepted by standard
    -- configuration of the WR PTP Core
    rx_cfg_ethertype_i         : in std_logic_vector(15 downto 0) := x"dbff";
    -- 1: accept all broadcast packets
    -- 0: accept only unicasts
    rx_cfg_accept_broadcasts_i : in std_logic                     := '1';
    -- filtering of streamer frames on reception by source MAC address
    -- 0: accept frames from any source
    -- 1: accept frames only from the source MAC address defined in cfg_mac_remote_i
    rx_cfg_filter_remote_i     : in std_logic                     := '0';
    -- value in cycles of fixed-latency enforced on data
    rx_cfg_fixed_latency_i     : in std_logic_vector(27 downto 0) := x"0000000"

    );

end xwr_transmission;

architecture rtl of xwr_transmission is

  component  wr_transmission_wb is
    port (
      rst_n_i                                  : in     std_logic;
      clk_sys_i                                : in     std_logic;
      wb_adr_i                                 : in     std_logic_vector(4 downto 0);
      wb_dat_i                                 : in     std_logic_vector(31 downto 0);
      wb_dat_o                                 : out    std_logic_vector(31 downto 0);
      wb_cyc_i                                 : in     std_logic;
      wb_sel_i                                 : in     std_logic_vector(3 downto 0);
      wb_stb_i                                 : in     std_logic;
      wb_we_i                                  : in     std_logic;
      wb_ack_o                                 : out    std_logic;
      wb_stall_o                               : out    std_logic;
      regs_i                                   : in     t_wr_transmission_in_registers;
      regs_o                                   : out    t_wr_transmission_out_registers
    );
  end component;

  signal to_wb              : t_wr_transmission_in_registers;
  signal from_wb            : t_wr_transmission_out_registers;
  signal dbg_word                : std_logic_vector(31 downto 0);
  signal dbg_tx_bfield           : std_logic_vector(31 downto 0);
  signal dbg_rx_bfield           : std_logic_vector(31 downto 0);
  signal start_bit               : std_logic_vector(from_wb.dbg_ctrl_start_byte_o'length-1+3 downto 0);
  signal rx_data                 : std_logic_vector(g_rx_data_width-1 downto 0);
  signal wb_regs_slave_in        : t_wishbone_slave_in;
  signal wb_regs_slave_out       : t_wishbone_slave_out;  
  signal rx_latency_valid        : std_logic;
  signal rx_latency              : std_logic_vector(27 downto 0);
  signal rx_lost_frames          : std_logic;
  signal rx_lost_blocks          : std_logic;
  signal rx_frame                : std_logic;
  signal tx_frame                : std_logic;
  signal reset_time_tai          : std_logic_vector(39 downto 0);
  signal latency_acc             : std_logic_vector(63 downto 0);
  signal rx_valid                : std_logic;
  signal rx_lost_frames_cnt      : std_logic_vector(14 downto 0);
  signal tx_cfg_mac_local        : std_logic_vector(47 downto 0);
  signal tx_cfg_mac_target       : std_logic_vector(47 downto 0);
  signal tx_cfg_ethertype        : std_logic_vector(15 downto 0);
  signal rx_cfg_mac_local        : std_logic_vector(47 downto 0);
  signal rx_cfg_mac_remote       : std_logic_vector(47 downto 0);
  signal rx_cfg_ethertype        : std_logic_vector(15 downto 0);
  signal rx_cfg_accept_broadcasts: std_logic;
  signal rx_cfg_filter_remote    : std_logic;
  signal rx_cfg_fixed_latency    : std_logic_vector(27 downto 0);

  function f_dbg_word_starting_at_byte(data_in, start_bit : std_logic_vector; g_data_width: integer) return std_logic_vector is
    variable sb     : integer := 0;
    variable result : std_logic_vector(31 downto 0);
  begin
    sb     := to_integer(unsigned(start_bit));
    for i in 0 to 31 loop
      if (sb + i < g_data_width) then 
        result(i) := data_in(sb + i);
      else 
        result(i) := '0';
      end if;
    end loop;
    return result;
  end f_dbg_word_starting_at_byte;

begin

  U_TX: xtx_streamer
    generic map(
      g_data_width             => g_tx_data_width,
      g_tx_threshold           => g_tx_threshold,
      g_tx_max_words_per_frame => g_tx_max_words_per_frame,
      g_tx_timeout             => g_tx_timeout,
      g_escape_code_disable    => g_tx_escape_code_disable)
    port map(
      clk_sys_i                => clk_sys_i,
      rst_n_i                  => rst_n_i,
      src_i                    => src_i,
      src_o                    => src_o,
      clk_ref_i                => clk_ref_i,
      tm_time_valid_i          => tm_time_valid_i,
      tm_tai_i                 => tm_tai_i,
      tm_cycles_i              => tm_cycles_i,
      tx_data_i                => tx_data_i,
      tx_valid_i               => tx_valid_i,
      tx_dreq_o                => tx_dreq_o,
      tx_last_p1_i             => tx_last_p1_i,
      tx_flush_p1_i            => tx_flush_p1_i,
      tx_reset_seq_i           => from_wb.sscr1_rst_seq_id_o,
      tx_frame_p1_o            => tx_frame,
      cfg_mac_local_i          => tx_cfg_mac_local_i,
      cfg_mac_target_i         => tx_cfg_mac_target_i,
      cfg_ethertype_i          => tx_cfg_ethertype_i);

  U_RX: xrx_streamer
    generic map(
      g_data_width             => g_rx_data_width,
      g_buffer_size            => g_rx_buffer_size,
      g_escape_code_disable    => g_rx_escape_code_disable,
      g_expected_words_number  => g_rx_expected_words_number
      )
    port map(
      clk_sys_i                => clk_sys_i,
      rst_n_i                  => rst_n_i,
      snk_i                    => snk_i,
      snk_o                    => snk_o,
      clk_ref_i                => clk_ref_i,
      tm_time_valid_i          => tm_time_valid_i,
      tm_tai_i                 => tm_tai_i,
      tm_cycles_i              => tm_cycles_i,
      rx_first_p1_o            => rx_first_p1_o,
      rx_last_p1_o             => rx_last_p1_o,
      rx_data_o                => rx_data,
      rx_valid_o               => rx_valid,
      rx_dreq_i                => rx_dreq_i,
      rx_lost_p1_o             => rx_lost_blocks,
      rx_lost_frames_p1_o      => rx_lost_frames,
      rx_lost_frames_cnt_o     => rx_lost_frames_cnt,
      rx_latency_o             => rx_latency,
      rx_latency_valid_o       => rx_latency_valid,
      rx_frame_p1_o            => rx_frame,
      cfg_mac_local_i          => rx_cfg_mac_local_i,
      cfg_mac_remote_i         => rx_cfg_mac_remote_i,
      cfg_ethertype_i          => rx_cfg_ethertype_i,
      cfg_accept_broadcasts_i  => rx_cfg_accept_broadcasts_i,
      cfg_filter_remote_i      => rx_cfg_filter_remote,
      cfg_fixed_latency_i      => rx_cfg_fixed_latency);

  rx_data_o  <= rx_data;
  rx_valid_o <= rx_valid;

  U_STATS: xrtx_streamers_stats
    generic map(
      g_cnt_width              => g_stats_cnt_width,
      g_acc_width              => g_stats_acc_width
      )
    port map(
      clk_i                    => clk_sys_i,
      rst_n_i                  => rst_n_i,
      sent_frame_i             => tx_frame,
      rcvd_frame_i             => rx_frame,
      lost_frame_i             => rx_lost_frames,
      lost_block_i             => rx_lost_blocks,
      lost_frames_cnt_i        => rx_lost_frames_cnt,
      rcvd_latency_i           => rx_latency,
      rcvd_latency_valid_i     => rx_latency_valid,
      clk_ref_i                => clk_ref_i,
      tm_time_valid_i          => tm_time_valid_i,
      tm_tai_i                 => tm_tai_i,
      tm_cycles_i              => tm_cycles_i,
      reset_stats_i            => from_wb.sscr1_rst_stats_o,
      snapshot_ena_i           => from_wb.sscr1_snapshot_stats_o,
      reset_time_tai_o         => reset_time_tai,
      reset_time_cycles_o      => to_wb.sscr1_rst_ts_cyc_i,
      sent_frame_cnt_o         => to_wb.tx_stat_tx_sent_cnt_i,
      rcvd_frame_cnt_o         => to_wb.rx_stat1_rx_rcvd_cnt_i,
      lost_frame_cnt_o         => to_wb.rx_stat2_rx_loss_cnt_i,
      lost_block_cnt_o         => to_wb.rx_stat8_rx_lost_block_cnt_i,
      latency_cnt_o            => to_wb.rx_stat7_rx_latency_acc_cnt_i,
      latency_acc_o            => latency_acc,
      latency_max_o            => to_wb.rx_stat3_rx_latency_max_i,
      latency_min_o            => to_wb.rx_stat4_rx_latency_min_i,
      latency_acc_overflow_o   => to_wb.sscr1_rx_latency_acc_overflow_i,
      snmp_array_o             => snmp_array_o(c_STREAMERS_ARR_SIZE_OUT-1 downto 0),
      snmp_array_i             => snmp_array_i
      );

  to_wb.sscr2_rst_ts_tai_lsb_i        <= reset_time_tai(31 downto 0);
  to_wb.rx_stat5_rx_latency_acc_lsb_i <= latency_acc(31 downto 0);
  to_wb.rx_stat6_rx_latency_acc_msb_i <= latency_acc(63 downto 32);


  U_WB_ADAPTER : wb_slave_adapter
    generic map (
      g_master_use_struct  => true,
      g_master_mode        => CLASSIC,
      g_master_granularity => WORD,
      g_slave_use_struct   => true,
      g_slave_mode         => g_slave_mode,
      g_slave_granularity  => g_slave_granularity)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      slave_i    => wb_slave_i,
      slave_o    => wb_slave_o,
      master_i   => wb_regs_slave_out,
      master_o   => wb_regs_slave_in);

  U_WB:  wr_transmission_wb
    port map (
      rst_n_i      => rst_n_i,
      clk_sys_i    => clk_sys_i,
      wb_adr_i     => wb_regs_slave_in.adr(4 downto 0),
      wb_dat_i     => wb_regs_slave_in.dat,
      wb_dat_o     => wb_regs_slave_out.dat,
      wb_cyc_i     => wb_regs_slave_in.cyc,
      wb_sel_i     => wb_regs_slave_in.sel(3 downto 0),
      wb_stb_i     => wb_regs_slave_in.stb,
      wb_we_i      => wb_regs_slave_in.we,
      wb_ack_o     => wb_regs_slave_out.ack,
      wb_stall_o   => wb_regs_slave_out.stall,
      regs_i       => to_wb,
      regs_o       => from_wb
    );

  start_bit <= from_wb.dbg_ctrl_start_byte_o & "000";

  p_debug_mux: process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        dbg_word <= (others =>'0');
      else
        if(from_wb.dbg_ctrl_mux_o = '1') then --rx
          if(rx_valid = '1') then
            dbg_word <= f_dbg_word_starting_at_byte(rx_data,start_bit,g_tx_data_width);
          end if;
        else -- tx
          if(tx_valid_i = '1') then
            dbg_word <= f_dbg_word_starting_at_byte(tx_data_i,start_bit,g_tx_data_width);
          end if;
        end if;
      end if;
    end if;
  end process;

  gen_btrain_debug: if(g_tx_data_width > 47 and g_rx_data_width > 47) generate
    -- this is b-train specific stuff that allows snooping bfield easily. Btrain is
    -- the main (and currently) only application of streamers
    p_bfield_for_SNMP: process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        if rst_n_i = '0' then
          dbg_tx_bfield <= (others =>'0');
          dbg_rx_bfield <= (others =>'0');
        else
          if(rx_valid = '1') then
            dbg_rx_bfield <= rx_data(15+16 downto 0+16) & rx_data(31+16 downto 16+16);
          end if;
          if(tx_valid_i = '1') then
            dbg_tx_bfield <= tx_data_i(15+16 downto 0+16) & tx_data_i(31+16 downto 16+16);
          end if;
        end if;
      end if;
    end process;
    snmp_array_o(c_STREAMERS_ARR_SIZE_OUT+1) <= dbg_rx_bfield;
    snmp_array_o(c_STREAMERS_ARR_SIZE_OUT+2) <= dbg_tx_bfield;
  end generate gen_btrain_debug;

  snmp_array_o(c_STREAMERS_ARR_SIZE_OUT)   <= dbg_word;

  to_wb.dbg_data_i      <= dbg_word;
  to_wb.dbg_rx_bvalue_i <= dbg_rx_bfield;
  to_wb.dbg_tx_bvalue_i <= dbg_tx_bfield;
  to_wb.dummy_dummy_i   <= x"DEADBEEF";

  tx_cfg_ethertype               <= from_wb.tx_cfg0_ethertype_o      when (from_wb.cfg_or_tx_ethtype_o='1') else
                                            tx_cfg_ethertype_i;
  tx_cfg_mac_local(31 downto 0)  <= from_wb.tx_cfg1_mac_local_lsb_o  when (from_wb.cfg_or_tx_mac_loc_o='1') else
                                            tx_cfg_mac_local_i(31 downto 0);
  tx_cfg_mac_local(47 downto 32) <= from_wb.tx_cfg2_mac_local_msb_o  when (from_wb.cfg_or_tx_mac_loc_o='1') else
                                            tx_cfg_mac_local_i(47 downto 32);
  tx_cfg_mac_target(31 downto 0) <= from_wb.tx_cfg3_mac_target_lsb_o when (from_wb.cfg_or_tx_mac_tar_o='1') else
                                            tx_cfg_mac_target_i(31 downto 0);
  tx_cfg_mac_target(47 downto 32)<= from_wb.tx_cfg4_mac_target_msb_o when (from_wb.cfg_or_tx_mac_tar_o='1') else
                                            tx_cfg_mac_target_i(47 downto 32);

  rx_cfg_ethertype               <= from_wb.rx_cfg0_ethertype_o      when (from_wb.cfg_or_rx_ethertype_o='1') else
                                            rx_cfg_ethertype_i;
  rx_cfg_mac_local(31 downto 0)  <= from_wb.rx_cfg1_mac_local_lsb_o  when (from_wb.cfg_or_rx_mac_loc_o='1') else
                                            rx_cfg_mac_local_i(31 downto 0);
  rx_cfg_mac_local(47 downto 32) <= from_wb.rx_cfg2_mac_local_msb_o  when (from_wb.cfg_or_rx_mac_loc_o='1') else
                                            rx_cfg_mac_local_i(47 downto 32);
  rx_cfg_mac_remote(31 downto 0) <= from_wb.rx_cfg3_mac_remote_lsb_o when (from_wb.cfg_or_rx_mac_rem_o='1') else
                                            rx_cfg_mac_remote_i(31 downto 0);
  rx_cfg_mac_remote(47 downto 32)<= from_wb.rx_cfg4_mac_remote_msb_o when (from_wb.cfg_or_rx_mac_rem_o='1') else
                                            rx_cfg_mac_remote_i(47 downto 32);
  rx_cfg_accept_broadcasts       <= from_wb.rx_cfg0_accept_broadcast_o when (from_wb.cfg_or_rx_acc_broadcast_o='1') else
                                            rx_cfg_accept_broadcasts_i;
  rx_cfg_filter_remote           <= from_wb.rx_cfg0_filter_remote_o    when (from_wb.cfg_or_rx_ftr_remote_o='1') else
                                            rx_cfg_filter_remote_i;
  rx_cfg_fixed_latency           <= from_wb.rx_cfg5_fixed_latency_o    when (from_wb.cfg_or_rx_fix_lat_o='1') else
                                            rx_cfg_fixed_latency_i;
end rtl;