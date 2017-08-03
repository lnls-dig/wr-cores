-------------------------------------------------------------------------------
-- Title      : Digital DMTD Phase Measurement Unit
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : dmtd_phase_meas.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-02-25
-- Last update: 2017-02-20
-- Platform   : FPGA-generic
-- Standard   : VHDL '93
-------------------------------------------------------------------------------
-- Description: Module measures phase shift between the two input clocks
-- using a DDMTD phase detector. The raw measurement can be further averaged to
-- increase the accuracy.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2009 - 2017 CERN
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
-- 2010-02-25  1.0      twlostow        Created
-- 2011-04-18  1.1      twlostow        Added comments and header
-- 2017-08-03  1.2      lerwys          Use dmtd_phase_meas_full
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.NUMERIC_STD.all;

library work;
use work.gencores_pkg.all;

entity dmtd_phase_meas is
  generic (
-- DDMTD deglitcher threshold (in clk_dmtd_i) clock cycles
    g_deglitcher_threshold: integer;
-- Phase tag counter size (see dmtd_with_deglitcher.vhd for explanation)
    g_counter_bits        : integer := 14);

  port (
-- resets
    rst_sys_n_i  : in std_logic;
    rst_dmtd_n_i : in std_logic;

-- system clock
    clk_sys_i  : in std_logic;
-- Input clocks
    clk_a_i    : in std_logic;
    clk_b_i    : in std_logic;
    clk_dmtd_i : in std_logic;


    en_i : in std_logic;

    navg_i         : in  std_logic_vector(11 downto 0);
    phase_meas_o   : out std_logic_vector(31 downto 0);
    phase_meas_p_o : out std_logic
    );

end dmtd_phase_meas;

architecture syn of dmtd_phase_meas is

  component dmtd_phase_meas_full
    generic (
      g_deglitcher_threshold     : integer;
      g_counter_bits             : integer := 14);
    port (
      rst_sys_n_i                : in std_logic;
      rst_dmtd_n_i               : in std_logic;
      clk_sys_i                  : in std_logic;
      clk_a_i                    : in std_logic;
      clk_b_i                    : in std_logic;
      clk_dmtd_i                 : in std_logic;
      en_i                       : in std_logic;
      tag_a_o                    : out std_logic_vector(g_counter_bits-1 downto 0);
      tag_a_p_o                  : out std_logic;
      tag_b_o                    : out std_logic_vector(g_counter_bits-1 downto 0);
      tag_b_p_o                  : out std_logic;
      navg_i                     : in  std_logic_vector(11 downto 0);
      phase_raw_o                : out std_logic_vector(g_counter_bits-1 downto 0);
      phase_raw_p_o              : out std_logic;
      phase_meas_o               : out std_logic_vector(31 downto 0);
      phase_meas_p_o             : out std_logic);
  end component;

end dmtd_phase_meas_full;

  U_phase_meas_full : dmtd_phase_meas_full
    generic map (
      g_deglitcher_threshold => g_deglitcher_threshold,
      g_counter_bits         => g_counter_bits)
    port map (
      rst_sys_n_i    => rst_sys_n_i,
      rst_dmtd_n_i   => rst_dmtd_n_i,
      clk_sys_i      => clk_sys_i,
      clk_a_i        => clk_a_i,
      clk_b_i        => clk_b_i,
      clk_dmtd_i     => clk_dmtd_i,
      en_i           => en_i,
      tag_a_o        => open,
      tag_a_p_o      => open,
      tag_b_o        => open,
      tag_b_p_o      => open,
      navg_i         => navg_i,
      phase_raw_o    => open,
      phase_raw_p_o  => open,
      phase_meas_o   => phase_meas_o,
      phase_meas_p_o => phase_meas_p_o);

end syn;

