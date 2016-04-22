--! @file eca_msi.vhd
--! @brief ECA MSI vector
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component combines MSI outputs of multiple channels with an address map.
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
use work.wishbone_pkg.all;

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
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_chan_bits : natural := f_eca_log2(g_num_channels);
  constant c_zero      : std_logic_vector(g_num_channels-1 downto 0) := (others => '0');
  
  signal s_wen : std_logic;
  
  signal r_stall : std_logic;
  signal rc_xor0 : std_logic := '0';
  signal ri_xor  : std_logic_vector(3 downto 0) := (others => '0');
  signal rc_xor  : std_logic_vector(3 downto 0) := (others => '0');
  
  signal sc_enable  : std_logic;
  signal rc_enable  : std_logic_vector(g_num_channels-1 downto 0) := (others => '0');
  signal ri_enable2 : std_logic_vector(g_num_channels-1 downto 0);
  signal ri_enable1 : std_logic_vector(g_num_channels-1 downto 0);
  signal ri_enable  : std_logic_vector(g_num_channels-1 downto 0);
  
  signal s_ack     : std_logic;
  signal s_pending : std_logic_vector(g_num_channels-1 downto 0);
  signal s_channel : std_logic_vector(c_chan_bits-1 downto 0);
  signal r_select  : std_logic_vector(g_num_channels-1 downto 0) := (others => '0');
  signal r_channel : std_logic_vector(c_chan_bits   -1 downto 0) := (others => '0');
  signal r_ready   : std_logic := '0';
  signal r_cyc     : std_logic := '0';
  signal r_stb     : std_logic := '0';
  
  signal s_address : std_logic_vector(31 downto 0);
  signal r_address : std_logic_vector(31 downto 0);
  signal r_delay   : std_logic_vector(g_num_channels-1 downto 0);
  signal r_mask    : std_logic_vector(g_num_channels-1 downto 0);
  signal r_num     : t_num;
  signal r_code    : t_code;
  signal s_dat     : std_logic_vector(31 downto 0) := (others => '0');

begin

  input : process(c_clk_i, c_rst_n_i) is
  begin
    if c_rst_n_i = '0' then
      rc_enable <= (others => '0');
      r_stall   <= '0';
      rc_xor0   <= '0';
      rc_xor    <= (others => '0');
    elsif rising_edge(c_clk_i) then
      -- Toggle the MSI enable state
      if c_enable_stb_i = '1' then
        rc_enable(to_integer(unsigned(c_chan_i))) <= c_enable_i;
      end if;
      
      -- Block write access until it would be safe to set the MSI address
      if c_enable_stb_i = '1' and c_enable_i = '0' then
        r_stall <= '1';
        rc_xor0 <= not rc_xor0;
      end if;
      rc_xor <= ri_xor(0) & rc_xor(rc_xor'high downto 1);
      if (rc_xor(1) xor rc_xor(0)) = '1' then
        r_stall <= '0';
      end if;
    end if;
  end process;
  
  delay : process(i_clk_i, i_rst_n_i) is
  begin
    if i_rst_n_i = '0' then
      ri_xor <= (others => '0');
    elsif rising_edge(i_clk_i) then
      ri_xor <= rc_xor0 & ri_xor(ri_xor'high downto 1);
    end if;
  end process;
  
  -- Is the currently selected channel enabled?
  sc_enable <= rc_enable(to_integer(unsigned(c_chan_i))) when f_eca_safe(c_chan_i)='1' else 'X';

  -- Block access until it would be safe to set the MSI
  c_stall_o  <= r_stall;
  c_enable_o <= sc_enable;
  
  -- Only allow updating the MSI address if the MSI is currently disabled
  s_wen <= not sc_enable and c_target_stb_i;
  
  shadow : eca_sdp
    generic map(
      g_addr_bits  => c_chan_bits,
      g_data_bits  => 32,
      g_bypass     => false,
      g_dual_clock => false)
    port map(
      r_clk_i  => c_clk_i,
      r_addr_i => c_chan_i(c_chan_bits-1 downto 0),
      r_data_o => c_target_o,
      w_clk_i  => c_clk_i,
      w_en_i   => s_wen,
      w_addr_i => c_chan_i(c_chan_bits-1 downto 0),
      w_data_i => c_target_i);
      
  cross : eca_sdp
    generic map(
      g_addr_bits  => c_chan_bits,
      g_data_bits  => 32,
      g_bypass     => false,
      g_dual_clock => true)
    port map(
      r_clk_i  => i_clk_i,
      r_addr_i => s_channel,
      r_data_o => s_address,
      w_clk_i  => c_clk_i,
      w_en_i   => s_wen,
      w_addr_i => c_chan_i(c_chan_bits-1 downto 0),
      w_data_i => c_target_i);
  
  s_ack <= i_master_i.ack or i_master_i.err or i_master_i.rty;
  i_ack_o <= f_eca_mux(s_ack, r_mask, c_zero);
  
  s_pending <= ri_enable and i_stb_i;
  s_channel <= f_eca_1hot_decode(r_select);
  main : process(i_clk_i, i_rst_n_i) is
  begin
    if i_rst_n_i = '0' then
      r_select  <= (others => '0');
      r_channel <= (others => '0');
      r_ready   <= '0';
      r_cyc     <= '0';
      r_stb     <= '0';
    elsif rising_edge(i_clk_i) then
      r_select  <= s_pending and f_eca_add(not s_pending, 1);
      r_channel <= s_channel;
      r_ready   <= f_eca_or(r_select);
      
      if r_cyc = '0' then
        r_cyc <= r_ready;
        r_stb <= r_ready;
      else
        r_cyc <= not s_ack;
        -- clear these so that the MSI won't be reissued when the cycle ends
        r_select <= (others => '0');
        r_ready  <= '0';
      end if;
      
      if r_stb = '1' then
        r_stb <= i_master_i.stall;
      end if;
    end if;
  end process;
  
  bulk : process(i_clk_i) is
  begin
    if rising_edge(i_clk_i) then
      -- Cross the enable signals
      ri_enable2 <= rc_enable;
      ri_enable1 <= ri_enable2;
      ri_enable  <= ri_enable1;
      
      r_delay <= r_select; -- same cycle as r_ready
      if r_cyc = '0' then
        r_mask    <= r_delay; -- latch the mask for the ack
        r_address <= s_address;
        r_num     <= i_num_i(to_integer(unsigned(r_channel)));
        r_code    <= i_code_i(to_integer(unsigned(r_channel)));
      end if;
    end if;
  end process;
  
  i_master_o.cyc <= r_cyc;
  i_master_o.stb <= r_stb;
  i_master_o.adr <= r_address;
  i_master_o.sel <= (others => '1');
  i_master_o.we  <= '1';
  i_master_o.dat <= s_dat;
  
  s_dat(18 downto 16) <= r_code;
  s_dat(r_num'range)  <= r_num;
  
end rtl;
