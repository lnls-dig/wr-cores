-------------------------------------------------------------------------------
-- Title      : RX Bypass queue
-- Project    : White Rabbit MAC/Endpoint
-------------------------------------------------------------------------------
-- File       : ep_rx_bypass_queue.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2011-08-10
-- Last update: 2012-02-09
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: A small, LUT/SRL based FIFO keeping constant latency. Used to
-- strip off the CRC of the frame.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2009-2011 CERN / BE-CO-HT
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
use work.wr_fabric_pkg.all;

entity ep_rx_bypass_queue is
  generic(
    g_size  : integer := 3;
    g_width : integer := 18);

  port(
    rst_n_i : in std_logic;
    clk_i   : in std_logic;

    d_i     : in  std_logic_vector(g_width-1 downto 0);
    valid_i : in  std_logic;
    dreq_o  : out std_logic;

    q_o     : out std_logic_vector(g_width-1 downto 0);
    valid_o : out std_logic;
    dreq_i  : in  std_logic;

    flush_i : in  std_logic;
    purge_i : in  std_logic;
    empty_o : out std_logic
    );

end ep_rx_bypass_queue;

architecture behavioral of ep_rx_bypass_queue is

  component ep_shift_reg
    generic (
      g_size : integer);
    port (
      clk_i : in  std_logic;
      ce_i  : in  std_logic;
      d_i   : in  std_logic;
      q_o   : out std_logic);
  end component;

  function f_queue_occupation(q : std_logic_vector; check_empty : std_logic) return std_logic is
    variable i : integer;
  begin
    for i in 0 to q'length-1 loop
      if(q(i) = check_empty) then
        return '0';
      end if;
    end loop;  -- i
    return '1';
  end function;

  type t_queue_array is array(0 to g_size-1) of std_logic_vector(g_width-1 downto 0);

  signal q_data  : t_queue_array;
  signal q_valid : std_logic_vector(g_size-1 downto 0);


  signal qempty, qfull : std_logic;
  signal flushing      : std_logic;
  signal valid_mask    : std_logic;
  signal valid_int     : std_logic;

  signal sreg_enable : std_logic;
  
  signal first_word :  std_logic_vector(g_width-1 downto 0);
  signal oob_in     : std_logic;
begin  -- behavioral

  qempty <= f_queue_occupation(q_valid, '1');
  qfull  <= f_queue_occupation(q_valid, '0');

  empty_o <= qempty;

  first_word <= q_data(g_size-1) ;

  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if(sreg_enable = '1') then
        q_data(g_size-1)  <= d_i;
        L0: for i in 0 to g_size-2 loop
            q_data(i)       <= q_data(i+1);
        end loop L0;        
      end if;
    end if;
  end process;

  q_o         <= q_data(g_size-1) when (first_word(17 downto 16) = c_WRF_OOB) else q_data(0);
  oob_in      <= '1'  when (first_word(17 downto 16) = c_WRF_OOB and q_valid(0) = '1') else '0';
  sreg_enable <= '1' when ((valid_i = '1') or (qempty = '0' and valid_int = '1')) else '0';

  p_queue : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' or purge_i = '1' then
        flushing   <= '0';
        valid_mask <= '0';
        q_valid    <= (others => '0');
      else
        if(flushing = '1' and qempty = '1') then
          flushing <= '0';
        elsif(flush_i = '1' and qempty = '0') then
          flushing <= '1';
        end if;

        valid_mask <= dreq_i;

        if sreg_enable = '1' then
          q_valid(0)                           <= valid_i;
          if(oob_in = '1' ) then -- flashing CRC
            q_valid(q_valid'length-1 downto 1) <=(others => '0');
          else
            q_valid(q_valid'length-1 downto 1) <= q_valid(q_valid'length-2 downto 0);
          end if;
        end if;
      end if;
    end if;
  end process;

  dreq_o    <= (dreq_i or not qfull ) and not ((flush_i or flushing) and not qempty);
  valid_int <= (qfull and valid_i) or (not qempty and (oob_in or flushing) and valid_mask);
  valid_o   <= valid_int;
  
end behavioral;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ep_shift_reg is
  generic(g_size : integer := 16);
  port(
    clk_i : in  std_logic;
    ce_i  : in  std_logic;
    d_i   : in  std_logic;
    q_o   : out std_logic);
end ep_shift_reg;

architecture rtl of ep_shift_reg is
  signal sreg : std_logic_vector(g_size-1 downto 0);

begin  -- rtl

  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if(ce_i = '1') then
        sreg(0)                 <= d_i;
        sreg(g_size-1 downto 1) <= sreg(g_size-2 downto 0);
      end if;
    end if;
  end process;

end rtl;
