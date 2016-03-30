--! @file eca_msi.vhd
--! @brief ECA MSI vector
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component receives actions to be executed, in any order.
--! It outputs actions when their deadline is due, essentially sorting them.
--!
--------------------------------------------------------------------------------
--! This library is free software; you can redistribute it and/or
--! modify it under the terms of the GNU Lesser General Public
--! License as published by the Free Software Foundation; either
--! version 3 of the License, or (at your option) any later version.
--!
--! This library is distributed in the hope that it will be useful,
--! but WITHOUT ANY WARRANTY; without even the implied warranty of
--! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--! Lesser General Public License for more details.
--!  
--! You should have received a copy of the GNU Lesser General Public
--! License along with this library. If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.eca_internals_pkg.all;

entity eca_msi is
  generic(
    g_num_channels : natural :=  1);
  port(
    -- Configuration interface
    c_clk_i        : in  std_logic;
    c_rst_n_i      : in  std_logic;
    c_chan_i       : in  std_logic_vector(7 downto 0);
    c_enable_stb_i : in  std_logic;
    c_enable_i     : in  std_logic;
    c_enable_o     : out std_logic;
    c_target_stb_i : in  std_logic;
    c_target_i     : in  std_logic_vector(31 downto 0);
    c_target_o     : out std_logic_vector(31 downto 0);
    c_stall_o      : out std_logic;
    -- MSI controller
    i_clk_i        : in  std_logic;
    i_rst_n_i      : in  std_logic;
    i_ack_o        : out std_logic_vector(g_num_channels-1 downto 0);
    i_stb_i        : in  std_logic_vector(g_num_channels-1 downto 0);
    i_code_i       : in  t_code_array(g_num_channels-1 downto 0);
    i_num_i        : in  t_num_array(g_num_channels-1 downto 0);
    i_master_i     : in  t_wishbone_master_in;
    i_master_o     : out t_wishbone_master_out);
end eca_msi;

architecture rtl of eca_msi is
begin
  -- !!! implement this
end rtl;
