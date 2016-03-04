-------------------------------------------------------------------------------
-- Title      : Pipelined timestamp adder with normalization
-- Project    : Fine Delay Core (FmcDelay1ns4cha)
-------------------------------------------------------------------------------
-- File       : fd_ts_adder.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN
-- Created    : 2011-08-29
-- Last update: 2013-03-13
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Pipelined timestamp adder with re-normalization of the result.
-- Adds a to b, producing normalized timestamp q. A timestmap is normalized when
-- the 0 <= frac < 2**g_frac_bits, 0 <= coarse <= g_coarse_range-1 and utc >= 0.
-- For correct operation of renormalizer, input timestamps must meet the
-- following constraints:
-- 1. 0 <= (a/b)_frac_i <= 2**g_frac_bits-1
-- 2. -g_coarse_range+1 <= (a_coarse_i + b_coarse_i) <= 3*g_coarse_range-1
-------------------------------------------------------------------------------
--
-- Copyright (c) 2011 CERN / BE-CO-HT
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
-- 2011-08-29  1.0      twlostow        Created
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity timestamp_adder is
  generic
    (
      -- sizes of the respective bitfields of the input/output timestamps
      g_frac_bits   : integer := 12;
      g_cycles_bits : integer := 28;
      g_tai_bits    : integer := 40;

      -- upper bound of the coarse part
      g_ref_clk_rate: integer := 125000000
      );

  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic;

    valid_i  : in std_logic;  -- when HI, a_* and b_* contain valid timestamps
    enable_i : in std_logic := '1';     -- pipeline enable

    -- Input timestamps
    a_tai_i    : in std_logic_vector(g_tai_bits-1 downto 0);
    a_cycles_i : in std_logic_vector(g_cycles_bits-1 downto 0);
    a_frac_i   : in std_logic_vector(g_frac_bits-1 downto 0) := (others => '0');

    b_tai_i    : in std_logic_vector(g_tai_bits-1 downto 0);
    b_cycles_i : in std_logic_vector(g_cycles_bits-1 downto 0);
    b_frac_i   : in std_logic_vector(g_frac_bits-1 downto 0) := (others => '0');

    -- Normalized sum output (valid when valid_o == 1)
    valid_o    : out std_logic;
    q_tai_o    : out std_logic_vector(g_tai_bits-1 downto 0);
    q_cycles_o : out std_logic_vector(g_cycles_bits-1 downto 0);
    q_frac_o   : out std_logic_vector(g_frac_bits-1 downto 0)
    );
end timestamp_adder;

architecture rtl of timestamp_adder is

  constant c_NUM_PIPELINE_STAGES : integer := 4;

  type t_internal_sum is record
    utc    : signed(g_tai_bits-1 downto 0);
    coarse : signed(g_cycles_bits+1 downto 0);
    frac   : signed(g_frac_bits+1 downto 0);
  end record;

  type t_internal_sum_array is array (integer range <>) of t_internal_sum;

  signal pipe : std_logic_vector(c_NUM_PIPELINE_STAGES-1 downto 0);
  signal sums : t_internal_sum_array(0 to c_NUM_PIPELINE_STAGES-1);

  signal ovf_frac   : std_logic;
  signal ovf_coarse : std_logic_vector(1 downto 0);
  signal unf_coarse : std_logic;
  
begin  -- rtl

  -- Pipeline stage 0: just add the two timestamps field by field
  p_stage0 : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        pipe(0) <= '0';
      elsif(enable_i = '1') then
        pipe(0) <= valid_i;

        sums(0).frac   <= signed("00" & a_frac_i) + signed("00" & b_frac_i);
        sums(0).coarse <= resize(signed(a_cycles_i), sums(0).coarse'length) +
                          resize(signed(b_cycles_i), sums(0).coarse'length);
        sums(0).utc <= signed(a_tai_i) + signed(b_tai_i);
      else
        pipe(0) <= '0';
      end if;
    end if;
  end process;

  ovf_frac <= std_logic(sums(0).frac(g_frac_bits));

  -- Pipeline stage 1: check the fractional sum for overflow and eventually adjust
  -- the coarse sum
  p_stage1 : process(clk_i)
  begin
    if rising_edge(clk_i) then
      
      if rst_n_i = '0' then
        pipe(1) <= '0';
      else
        pipe(1) <= pipe(0);

        if(ovf_frac = '1') then
          sums(1).frac   <= sums(0).frac - 2**g_frac_bits;
          sums(1).coarse <= sums(0).coarse + 1;
        else
          sums(1).frac   <= sums(0).frac;
          sums(1).coarse <= sums(0).coarse;
        end if;

        sums(1).utc <= sums(0).utc;
      end if;
    end if;
  end process;


  -- Pipeline stage 2: check the coarse sum for under/overflows
  p_stage2 : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        pipe(2) <= '0';
      else

        sums(2) <= sums(1);
        pipe(2) <= pipe(1);

        if(sums(1).coarse < 0) then
          unf_coarse <= '1';
          ovf_coarse <= "00";
        elsif(sums(1).coarse >= 2 * g_ref_clk_rate) then
          ovf_coarse <= "10";
          unf_coarse <= '0';
        elsif(sums(1).coarse >= g_ref_clk_rate) then
          ovf_coarse <= "01";
          unf_coarse <= '0';
        else
          ovf_coarse <= "00";
          unf_coarse <= '0';
        end if;
      end if;
    end if;
  end process;

  -- Pipeline stage 3: adjust the coarse & UTC sums according to normalize the
  -- previously detected under/overflows
  p_stage3 : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        pipe(3) <= '0';
      else

        pipe(3) <= pipe(2);

        if(unf_coarse = '1') then
          sums(3).coarse <= sums(2).coarse + g_ref_clk_rate;
          sums(3).utc    <= sums(2).utc - 1;
        elsif(ovf_coarse = "10") then
          sums(3).coarse <= sums(2).coarse - (2*g_ref_clk_rate);
          sums(3).utc    <= sums(2).utc + 2;
        elsif(ovf_coarse = "01") then
          sums(3).coarse <= sums(2).coarse - g_ref_clk_rate;
          sums(3).utc    <= sums(2).utc + 1;
        else
          sums(3).coarse <= sums(2).coarse;
          sums(3).utc    <= sums(2).utc;
        end if;

        sums(3).frac <= sums(2).frac;

      end if;
    end if;
  end process;

  -- clip the extra bits and output the result
  valid_o    <= pipe(c_NUM_PIPELINE_STAGES-1);
  q_tai_o    <= std_logic_vector(sums(c_NUM_PIPELINE_STAGES-1).utc(g_tai_bits-1 downto 0));
  q_cycles_o <= std_logic_vector(sums(c_NUM_PIPELINE_STAGES-1).coarse(g_cycles_bits-1 downto 0));
  q_frac_o   <= std_logic_vector(sums(c_NUM_PIPELINE_STAGES-1).frac(g_frac_bits-1 downto 0));

end rtl;
