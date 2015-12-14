--! @file eca_gpio_channel.vhd
--! @brief ECA-GPIO Adapter
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--! @author Stefan Rauch <s.rauch@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component takes an action channel and sends the tag to the SCU bus
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

use work.wishbone_pkg.all;
use work.eca_pkg.all;

entity eca_scubus_channel is
  port(
    clk_i     : in  std_logic;
    rst_n_i   : in  std_logic;
    channel_i : in  t_channel;
    tag_valid : out std_logic;
    tag       : out std_logic_vector(31 downto 0));  
end eca_scubus_channel;

architecture rtl of eca_scubus_channel is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  signal s_tag    : std_logic_vector(31 downto 0);
  signal s_valid  : std_logic;
  type state_type is (idle, cnt);
  signal sm_state : state_type;
  
begin
  
  tag_valid <= s_valid;
  tag       <= s_tag;

  main : process(clk_i) is
    variable v_cnt : unsigned(1 downto 0);
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        s_tag <= (others => '0');
        s_valid <= '0';
        v_cnt := (others => '0');
      else
        case sm_state is
        
          when idle =>
            s_valid <= '0';
            v_cnt := (others => '0');
            if channel_i.valid = '1' then
              s_tag     <= channel_i.tag;
              sm_state  <= cnt;
            end if;
            
          when cnt =>
            s_valid <= '1';
            if v_cnt = 3 then
              sm_state <= idle;
            else
              v_cnt := v_cnt + 1;
            end if;
               
        end case;
      

      end if;
    end if;
  end process;
  
end rtl;
