-------------------------------------------------------------------------------
-- Title      : WR Switch IRIG slave module
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : xwr_irig_slave.vhd
-- Author     : Harvey Leicester
-- Company    : CERN BE-CEM-EDL
-- Created    : 2024-10-23
-- Last update: 2024-10-23
-- Platform   : FPGA-generic
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- IRIG-B004 slave interface wrapper
-------------------------------------------------------------------------------
--
-- Copyright (c) 2012 - 2024 CERN / BE-CEM-EDL
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

use work.wishbone_pkg.all;
use work.gencores_pkg.all;
use work.irig_slave_regs_pkg.all;

entity xwr_irig_slave is
  generic
  (
    g_interface_mode      : t_wishbone_interface_mode      := PIPELINED;
    g_address_granularity : t_wishbone_address_granularity := BYTE;
    g_clks_per_ms         : integer := 62500
  );
  port
  (
    clk_i   : in std_logic;
    rst_n_i : in std_logic;

    irig_i    : in std_logic;
    pps_o     : out std_logic;
    enable_o  : out std_logic;  --enable flag out, used for muxing pps in top level

    wb_i : in  t_wishbone_slave_in;
    wb_o : out t_wishbone_slave_out
  );
end entity xwr_irig_slave;

architecture wrapper of xwr_irig_slave is

  component wr_irig_slave
    generic (
      g_clks_per_ms : integer := 62500
    );
    port (
      clk_sys_i     : in std_logic;  --sys clock
      rst_sys_n_i   : in std_logic;  --sys rst
      irig_i        : in std_logic;  --irig data in

      secs_o        : out std_logic_vector(7 downto 0);  --seconds out, 0-59
      mins_o        : out std_logic_vector(8 downto 0);  --minutes out, 0-59
      hrs_o         : out std_logic_vector(8 downto 0);  --hours out, 0-23
      days_o        : out std_logic_vector(10 downto 0); --day of year, 1-366
      year_o        : out std_logic_vector(8 downto 0);  --year, 0-99
      ctrl0_o       : out std_logic_vector(8 downto 0);  --control function 0
      ctrl1_o       : out std_logic_vector(8 downto 0);  --contorl function 1
      sbs_o         : out std_logic_vector(17 downto 0); --straight binary seconds
      valid_o       : out std_logic;                     --output valid
      pps_o         : out std_logic  --pps out, generated from start of irig frame
    );
  end component;

  signal wb_in : t_wishbone_slave_in;
  signal wb_out : t_wishbone_slave_out;
  signal irig_regs_in : t_irig_regs_master_in;
  signal irig_regs_out : t_irig_regs_master_out;

  signal secs : std_logic_vector(7 downto 0);
  signal mins : std_logic_vector(8 downto 0);
  signal hrs  : std_logic_vector(8 downto 0);
  signal days : std_logic_vector(10 downto 0);
  signal tos  : std_logic_vector(3 downto 0);
  signal yrs  : std_logic_vector(8 downto 0);
  signal ctrl0 : std_logic_vector(8 downto 0);
  signal ctrl1 : std_logic_vector(8 downto 0);
  signal sbs   : std_logic_vector(17 downto 0);
  signal valid : std_logic;

begin

  U_Adapter : wb_slave_adapter
  generic map
  (
    g_master_use_struct  => true,
    g_master_mode        => CLASSIC,
    g_master_granularity => WORD,
    g_slave_use_struct   => true,
    g_slave_mode         => g_interface_mode,
    g_slave_granularity  => g_address_granularity
  )
  port map
  (
    clk_sys_i => clk_i,
    rst_n_i   => rst_n_i,
    slave_i   => wb_i,
    slave_o   => wb_o,
    master_i  => wb_out,
    master_o  => wb_in
  );

  U_irig_slave_regs: entity work.irig_slave_regs
  port map
  (
    rst_n_i => rst_n_i,
    clk_i   => clk_i,
    wb_cyc_i  => wb_in.cyc,
    wb_stb_i  => wb_in.stb,
    wb_adr_i  => wb_in.adr(2 downto 0),
    wb_sel_i  => wb_in.sel,
    wb_we_i   => wb_in.we,
    wb_dat_i  => wb_in.dat,
    wb_ack_o  => wb_out.ack,
    wb_err_o  => wb_out.err,
    wb_rty_o  => wb_out.rty,
    wb_stall_o   => wb_out.stall,
    wb_dat_o     => wb_out.dat,
    irig_regs_i => irig_regs_in,
    irig_regs_o => irig_regs_out
  );

  U_irig_slave: wr_irig_slave
  generic map
  (
    g_clks_per_ms => g_clks_per_ms
  )
  port map
  (
    clk_sys_i   => clk_i,
    rst_sys_n_i => rst_n_i,
    irig_i      => irig_i,

    secs_o  => secs,
    mins_o  => mins,
    hrs_o   => hrs,
    days_o  => days,
    year_o  => yrs,
    ctrl0_o => ctrl0,
    ctrl1_o => ctrl1,
    sbs_o   => sbs,
    valid_o => valid,
    pps_o   => pps_o
  );

  irig_regs_in.TOD_seconds    <= secs;
  irig_regs_in.TOD_minutes    <= mins;
  irig_regs_in.TOD_hours      <= hrs;
  irig_regs_in.TOD_valid      <= valid;
  irig_regs_in.DATE_days      <= days;
  irig_regs_in.DATE_years     <= yrs;
  irig_regs_in.DATE_valid     <= valid;
  irig_regs_in.CTRL_fcn0      <= ctrl0;
  irig_regs_in.CTRL_fcn1      <= ctrl1;
  irig_regs_in.CTRL_valid     <= valid;
  irig_regs_in.SBS_sbs        <= sbs;
  irig_regs_in.SBS_valid      <= valid;

  enable_o <= irig_regs_out.CR_enable;

end architecture;

