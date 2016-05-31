-------------------------------------------------------------------------------
-- Title      : Btrain over White Rabbit
-- Project    : Btrain
-------------------------------------------------------------------------------
-- File       : xwr_transmission.vhd
-- Author     : Maciej Lipinski
-- Company    : CERN
-- Created    : 2016-05-30
-- Last update: 2016-05-31
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- 
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
use work.streamers_pkg.all; -- needed for streamers
use work.wr_fabric_pkg.all; -- neede for :t_wrf_source_in, etc
use work.wr_transmission_wbgen2_pkg.all;

entity xwr_transmission is
  
  generic (
    -- Width of data words on tx_data_i.
    g_data_width : integer := 32
    );

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    ---------------------------------------------------------------------------
    -- WR tx/rx interface
    ---------------------------------------------------------------------------
    -- Tx
    src_i : in  t_wrf_source_in;
    src_o : out t_wrf_source_out;

    -- Rx
    snk_i : in  t_wrf_sink_in;
    snk_o : out t_wrf_sink_out;

    ---------------------------------------------------------------------------
    -- User tx interface
    ---------------------------------------------------------------------------
    -- Data word to be sent.
    tx_data_i : in std_logic_vector(g_data_width-1 downto 0);
    -- 1 indicates that the tx_data_i contains a valid data word.
    tx_valid_i : in std_logic;
    -- Synchronous data request: if active, the user may send a data word in
    -- the following clock cycle.
    tx_dreq_o : out std_logic;
    -- Last signal. Can be used to indicate the last data word in a larger
    -- block of samples (see documentation for more details).
    tx_last_i : in std_logic := '1';
    -- Flush input. When asserted, the streamer will immediatly send out all
    -- the data that is stored in its TX buffer, ignoring g_tx_timeout.
    tx_flush_i : in std_logic := '0';

    ---------------------------------------------------------------------------
    -- User rx interface
    ---------------------------------------------------------------------------
    -- 1 indicates the 1st word of the data block on rx_data_o.
    rx_first_o         : out std_logic;
    -- 1 indicates the last word of the data block on rx_data_o.
    rx_last_o          : out std_logic;
    -- Received data.
    rx_data_o          : out std_logic_vector(g_data_width-1 downto 0);
    -- 1 indicted that rx_data_o is outputting a valid data word.
    rx_valid_o         : out std_logic;
    -- Synchronous data request input: when 1, the streamer may output another
    -- data word in the subsequent clock cycle.
    rx_dreq_i          : in  std_logic;

    ---------------------------------------------------------------------------
    -- WRC Timing interface, used for latency measurement
    -- Caution: uses clk_ref_i clock domain!
    ---------------------------------------------------------------------------

    -- White Rabbit reference clock
    clk_ref_i : in std_logic := '0';
    -- Time valid flag
    tm_time_valid_i : in std_logic := '0';
    -- TAI seconds
    tm_tai_i : in std_logic_vector(39 downto 0) := x"0000000000";
    -- Fractional part of the second (in clk_ref_i cycles)
    tm_cycles_i : in std_logic_vector(27 downto 0) := x"0000000";

    -- wishbone interface 
    wb_slave_i               : in  t_wishbone_slave_in := cc_dummy_slave_in;
    wb_slave_o               : out t_wishbone_slave_out

    );

end xwr_transmission;
  
architecture rtl of xwr_transmission is

  component  wr_transmission_wb is
    port (
      rst_n_i                                  : in     std_logic;
      clk_sys_i                                : in     std_logic;
      wb_adr_i                                 : in     std_logic_vector(1 downto 0);
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

  constant c_STREAMER_DATA_WIDTH : integer :=208;
  constant c_STREAMER_ETHERTYPE  : std_logic_vector(15 downto 0) := x"dbff";	
  signal regs_to_wb              : t_wr_transmission_in_registers;
  signal regs_from_wb            : t_wr_transmission_out_registers;
  signal start_bit               : std_logic_vector(regs_from_wb.dbg_ctrl_start_byte_o'length-1+3 downto 0);
  signal rx_data                 : std_logic_vector(g_data_width-1 downto 0);
  
  function f_dbg_word_starting_at_byte(data_in, start_bit : std_logic_vector) return std_logic_vector is
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
      g_data_width             => c_STREAMER_DATA_WIDTH,
      g_tx_threshold           => 8,
      g_tx_timeout             => 1024)
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
      tx_last_i                => tx_last_i,
      tx_flush_i               => tx_flush_i,
      tx_reset_seq_i           => '0',
      cfg_mac_local_i          => x"000000000000",
      cfg_mac_target_i         => x"ffffffffffff",
      cfg_ethertype_i          => c_STREAMER_ETHERTYPE);

  U_RX: xrx_streamer
    generic map(
      g_data_width             => c_STREAMER_DATA_WIDTH,
      g_filter_remote_mac      => false)
    port map(
      clk_sys_i                => clk_sys_i,
      rst_n_i                  => rst_n_i,
      snk_i                    => snk_i,
      snk_o                    => snk_o,
      clk_ref_i                => clk_ref_i,
      tm_time_valid_i          => tm_time_valid_i,
      tm_tai_i                 => tm_tai_i,
      tm_cycles_i              => tm_cycles_i,
      rx_first_o               => rx_first_o,
      rx_last_o                => rx_last_o,
      rx_data_o                => rx_data,
      rx_valid_o               => rx_valid_o,
      rx_dreq_i                => rx_dreq_i,
      rx_lost_o                => open,
      rx_latency_o             => open,
      rx_latency_valid_o       => open,
      cfg_mac_local_i          => x"000000000000",
      cfg_mac_remote_i         => x"000000000000",
      cfg_ethertype_i          => c_STREAMER_ETHERTYPE,
      cfg_accept_broadcasts_i  => '1');

  rx_data_o <= rx_data;

  U_WB:  wr_transmission_wb
    port map (
      rst_n_i      => rst_n_i,
      clk_sys_i    => clk_sys_i,
      wb_adr_i     => wb_slave_i.adr(3 downto 2),
      wb_dat_i     => wb_slave_i.dat,
      wb_dat_o     => wb_slave_o.dat,
      wb_cyc_i     => wb_slave_i.cyc,
      wb_sel_i     => wb_slave_i.sel(3 downto 0),
      wb_stb_i     => wb_slave_i.stb,
      wb_we_i      => wb_slave_i.we,
      wb_ack_o     => wb_slave_o.ack,
      wb_stall_o   => wb_slave_o.stall,
      regs_i       => regs_to_wb,
      regs_o       => regs_from_wb
    );

  start_bit <= regs_from_wb.dbg_ctrl_start_byte_o & "000";

  p_debug_mux: process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        regs_to_wb.dbg_data_i <= (others =>'0');
      else
        if(regs_from_wb.dbg_ctrl_mux_o = '1') then --rx
          regs_to_wb.dbg_data_i <= f_dbg_word_starting_at_byte(rx_data,start_bit);
        else -- tx
          regs_to_wb.dbg_data_i <= f_dbg_word_starting_at_byte(tx_data_i,start_bit);
        end if;
      end if;
    end if;
  end process;
  -- statistics ideas:
  -- * note the timestamp of reset (tai) or number of set frames since reset
  --   to make good statistics

  regs_to_wb.rx_stat_rx_loss_cnt_i <=x"DEADBEEF";

end rtl;