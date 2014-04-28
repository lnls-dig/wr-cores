--! @file wr_tlu_fsm.vhd
--! @brief Timestamp latch unit for WR core with WB b4 interface
--!
--! Copyright (C) 2011-2012 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--!----------------------------------------------------------------------------
--1
--! @author Mathias Kreider <m.kreider@gsi.de>
--!
--! @bug No know bugs.
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

--! Standard library
library IEEE;
--! Standard packages    
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library work;
use work.wishbone_pkg.all;
use work.gencores_pkg.all;
use work.tlu_pkg.all;

entity wr_tlu_fsm is
   generic( g_offset       : natural);  
   port (
      clk_ref_i         : in  std_logic;    -- tranceiver clock domain
      rst_ref_n_i       : in  std_logic;
      
      trigger_i         : in  t_trigger;  -- trigger vector for latch. meant to be used with 8bits from
      tm_tai_cyc_i      : in  std_logic_vector(63 downto 0);   -- TAI Timestamp in 8ns cycles  

      captured_time_o   : out std_logic_vector(64 + 3 - 1 downto 0);
      valid_o           : out std_logic;

      -- sys clk domain signals, sync them first
      stable_i          : t_wishbone_data;
      active_i          : std_logic;
      edge_i            : std_logic
   );               

end wr_tlu_fsm;

architecture behavioral of wr_tlu_fsm is

  function f_count(input : std_logic_vector; thing2count : std_logic) return natural is
      variable result, i : natural;
      
   begin
      result := 0;
      for i in input'left downto 0 loop 
         if input(i) = thing2count then
            result := result + 1;
         end if;
      end loop;
      return result; 
   end f_count;
   
   function f_countUnbroken(input : std_logic_vector; thing2count : std_logic; dir : string := "lsb") return natural is
      variable result, i : natural;   
   begin
      result := 0;
      if(dir = "lsb") then
         for i in 0 to input'left loop 
            if input(i) = thing2count then   
               result := result + 1;
            else
               exit;
            end if;
         end loop; 
      else
         for i in input'left downto 0 loop 
            if input(i) = thing2count then   
               result := result + 1;
            else
               exit;
            end if;
         end loop;
      end if;
      return result; 
   end f_countUnbroken;
   
-------------------------------------------------------------------------------

   constant c_datbits : natural := 72;

   type   t_state is (e_LOW, e_TRANS_RISE, e_HIGH, e_TRANS_FALL, e_ERROR);
   signal r_state : t_state;

   signal r_wed0,
          r_wed1,
          r_wed2        : std_logic;
   
   signal s_we          : std_logic;


   signal r_timeshift   : unsigned(31 downto 0);
   signal r_cnt         : unsigned(31 downto 0);   
   signal r_cntUnbr     : unsigned(31 downto 0);
   
   signal s_add_a,
          s_add_b,
          s_add_x       : std_logic_vector(c_datbits-1 downto 0);

----------------------------------------------------------------------------

begin  -- behavioral

----------------------------------------------------------------------------
--Adder
----------------------------------------------------------------------------
-- ref cycle count * 8 => ns. Pad to 72 bits ( 9 * 8) for adder stages
s_add_a <= "00000" & tm_tai_cyc_i & "000"; 
 --negate timeshift value to create Ones Clomplement for subtraction
s_add_b <= x"ffffffffff" & not std_logic_vector(r_timeshift);
 --take ns value from result for output to fifo
captured_time_o <= s_add_x(66 downto 0);


   correct_time : gc_big_adder
   generic map(
    g_data_bits => c_datbits,
    g_parts => 9)
  port map(
    clk_i   => clk_ref_i,
    a_i     => s_add_a,    -- time in ns
    b_i     => s_add_b,    -- timeshift value in ns to subtract
    c_i     => '1',        -- set carry to 1 to create timeshift Twos complement
    x2_o    => s_add_x 
    
    );
   
   valid_o <= r_wed1;
   
   --delay we signal to fifo in order to compensate for adder delay 
   we_delay: process(clk_ref_i)
   begin
   if rising_edge(clk_ref_i) then
      if(rst_ref_n_i = '0') then
         r_wed0    <= '0';
      else
         r_wed0   <= s_we;
         r_wed1   <= r_wed0;
      end if;
   end if;      
 end process;
   
   
   --edge detection/deglitcher state machine
   deglitch : process(clk_ref_i)
      variable v_diff : unsigned (stable_i'range);
      variable v_1unbr_msb : unsigned(r_cntUnbr'range);
      variable v_0unbr_msb : unsigned(r_cntUnbr'range);
      variable v_1unbr_lsb : unsigned(r_cntUnbr'range);
      variable v_0unbr_lsb : unsigned(r_cntUnbr'range);
      variable v_1 : unsigned(r_cnt'range);
      variable v_0 : unsigned(r_cnt'range);
   begin
   if rising_edge(clk_ref_i) then
      if(rst_ref_n_i = '0') then
         r_state     <= e_LOW;
         r_cnt       <= (others => '0');
         r_cntUnbr   <= (others => '0');
      else
         s_we <= '0';
         
         -- unbroken bit sequence count from msb
         -- count starting from MSB is there to catch unbroken sequences spread over two incoming bytes 
         v_1unbr_msb  := to_unsigned(f_countUnbroken(trigger_i, '1', "msb"), r_cntUnbr'length);
         v_0unbr_msb  := to_unsigned(f_countUnbroken(trigger_i, '0', "msb"), r_cntUnbr'length);
         -- unbroken bit sequence count from lsb
         v_1unbr_lsb  := to_unsigned(f_countUnbroken(trigger_i, '1', "lsb"), r_cntUnbr'length);
         v_0unbr_lsb  := to_unsigned(f_countUnbroken(trigger_i, '0', "lsb"), r_cntUnbr'length);
         -- bit count
         v_1          := to_unsigned(f_count(trigger_i, '1'), r_cnt'length);
         v_0          := to_unsigned(f_count(trigger_i, '0'), r_cnt'length);
         
         
         
         
         v_diff := (r_cnt - r_cntUnbr); 
         r_timeshift <= (r_cntUnbr + ('0' & v_diff(v_diff'left downto 1)) +  to_unsigned(g_offset, 32));
                                
      
         
         
         case r_state is
                           
            when e_LOW        => if(trigger_i /=  x"00") then -- any changes lead to check for transition point
                                    r_cnt       <= v_1;
                                    r_cntUnbr   <= v_1unbr_lsb;
                                    r_state     <= e_TRANS_RISE;
                                 end if;
                                 
            when e_TRANS_RISE => r_cnt <= r_cnt + v_1;
                                 if(trigger_i /=  x"ff") then
                                    r_cntUnbr   <= v_1unbr_lsb;
                                 else
                                    r_cntUnbr   <= r_cntUnbr + 8;
                                 end if;  
            
                                 if(r_cntUnbr + v_1unbr_msb >= unsigned(stable_i)) then
                                    s_we <= edge_i and active_i;
                                   
                                    r_state     <= e_HIGH;
                                 end if;   
                                
            when e_HIGH       => if(trigger_i /= x"ff") then
                                    r_cnt       <= v_0;
                                    r_cntUnbr   <= v_0unbr_lsb;
                                    r_state     <= e_TRANS_FALL;
                                 end if;
                                  
                                 
            when e_TRANS_FALL => r_cnt <= r_cnt + v_0;
                                 if(trigger_i /=  x"00") then
                                    r_cntUnbr   <= v_0unbr_lsb;
                                 else
                                    r_cntUnbr   <= r_cntUnbr + 8;
                                 end if;  
            
                                 if(r_cntUnbr + v_0unbr_msb >= unsigned(stable_i)) then
                                    s_we <= not edge_i and active_i;
                                    
                                    r_state     <= e_LOW;
                                 end if;  
            
            
            when e_ERROR       => r_state <=  e_LOW;
            
            when others       => r_state  <=  e_ERROR;
         end case;
         
      end if;
   end if;
   end process deglitch;

end behavioral;


