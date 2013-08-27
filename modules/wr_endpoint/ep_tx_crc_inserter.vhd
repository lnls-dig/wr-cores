-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint
-- Project    : White Rabbit 
-------------------------------------------------------------------------------
-- File       : ep_tx_crc_inserter.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2009-06-22
-- Last update: 2012-11-15
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Calculates and embeds the CRC into the transmitted frame.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2009-2012 CERN / BE-CO-HT
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

library work;

use work.gencores_pkg.all;
use work.wr_fabric_pkg.all;
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;

entity ep_tx_crc_inserter is
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    snk_fab_i  : in  t_ep_internal_fabric;
    snk_dreq_o : out std_logic;

    src_fab_o  : out t_ep_internal_fabric;
    src_dreq_i : in  std_logic
    );
end ep_tx_crc_inserter;

architecture behavioral of ep_tx_crc_inserter is

  type t_state is (IDLE, WAIT_CRC, EMBED_1, EMBED_2, EMBED_3);

-- general signals
  signal state : t_state;

-- CRC generator signals
  signal crc_gen_reset  : std_logic;
  signal crc_gen_enable : std_logic;
  signal crc_value      : std_logic_vector(31 downto 0);

  signal odd_length : std_logic;

  signal embed_valid, embed_eof : std_logic;

  signal stored_msb : std_logic_vector(7 downto 0);
  signal in_payload : std_logic;
  signal src_dreq_d0 : std_logic;
  
begin  -- behavioral

  in_payload     <= '1' when (state = IDLE or state = WAIT_CRC) else '0';
-- ML: potential optimization (if desperate)
-- in_payload     <= '1' when (state = IDLE or state = WAIT_CRC) or (src_dreq_d0 = '1' and odd_length = '0' and state = EMBED_2)) else '0';
  crc_gen_reset  <= '1' when rst_n_i = '0' or snk_fab_i.sof = '1'                                         else '0';
  crc_gen_enable <= '1' when (snk_fab_i.dvalid = '1' and in_payload = '1') else '0';

  U_tx_crc_generator : gc_crc_gen
    generic map (
      g_polynomial              => x"04C11DB7",
      g_init_value              => x"ffffffff",
      g_residue                 => x"38fb2284",
      g_data_width              => 16,
      g_half_width              => 8,
      g_sync_reset              => 1,
      g_dual_width              => 1,
      g_registered_match_output => false,
      g_registered_crc_output   => true)
    port map (
      clk_i   => clk_sys_i,
      rst_i   => crc_gen_reset,
      en_i    => crc_gen_enable,
      half_i  => snk_fab_i.bytesel,
      data_i  => snk_fab_i.data,
      match_o => open,
      crc_o   => crc_value);

  p_delay_dreq: process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        src_dreq_d0 <= src_dreq_i;
      end if;
    end process;
  
  p_crc_fsm : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0' or snk_fab_i.error = '1') then
        state     <= IDLE;
        embed_eof <= '0';
      else
        case state is
          when IDLE =>
            embed_eof  <= '0';
            odd_length <= '0';

            if(snk_fab_i.sof = '1') then
              state <= WAIT_CRC;
            end if;
            
          when WAIT_CRC =>
            if(snk_fab_i.bytesel = '1') then
              odd_length <= '1';
              stored_msb <= snk_fab_i.data(15 downto 8);
            end if;

            if(snk_fab_i.eof = '1' or snk_fab_i.addr = c_WRF_OOB) then
              state <= EMBED_1;
            end if;

          when EMBED_1 =>
            if(src_dreq_d0 = '1') then
              state <= EMBED_2;
            end if;

          when EMBED_2 =>
            if(src_dreq_d0 = '1') then
              if(odd_length = '1') then
                state <= EMBED_3;
              else
                state     <= IDLE;
                embed_eof <= '1';
              end if;
            end if;
            
          when EMBED_3 =>
            if(src_dreq_d0 = '1') then
              state     <= IDLE;
              embed_eof <= '1';
            end if;
        end case;
      end if;
    end if;
  end process;

  p_drive_data_addr : process(state, crc_value, odd_length, stored_msb, snk_fab_i, src_dreq_d0)
  begin

    case state is
      when EMBED_1 =>
        if(odd_length = '1') then
          src_fab_o.data    <= stored_msb & crc_value(31 downto 24);
          src_fab_o.addr    <= c_WRF_DATA;
          src_fab_o.bytesel <= '0';
          src_fab_o.dvalid <= src_dreq_d0;
          src_fab_o.eof <= '0';
        else
          src_fab_o.data    <= crc_value(31 downto 16);
          src_fab_o.addr    <= c_WRF_DATA;
          src_fab_o.bytesel <= '0';
          src_fab_o.dvalid <= src_dreq_d0;
          src_fab_o.eof <= '0';
        end if;

      when EMBED_2 =>
        if(odd_length = '1') then
          src_fab_o.data    <= crc_value(23 downto 8);
          src_fab_o.addr    <= c_WRF_DATA;
          src_fab_o.bytesel <= '0';
          src_fab_o.dvalid <= src_dreq_d0;
          src_fab_o.eof <= '0';
        else
          src_fab_o.data    <= crc_value(15 downto 0);
          src_fab_o.addr    <= c_WRF_DATA;
          src_fab_o.bytesel <= '0';
          src_fab_o.dvalid <= src_dreq_d0;
          src_fab_o.eof <= '1';
        end if;
        
      when EMBED_3 =>
        src_fab_o.data    <= crc_value(7 downto 0) & "XXXXXXXX";
        src_fab_o.addr    <= c_WRF_DATA;
        src_fab_o.bytesel <= '1';
        src_fab_o.dvalid <= src_dreq_d0;
        src_fab_o.eof <= '1';

      when others =>
        src_fab_o.data    <= snk_fab_i.data;
        src_fab_o.addr    <= snk_fab_i.addr;
        src_fab_o.bytesel <= '0';
        src_fab_o.dvalid <= snk_fab_i.dvalid and not snk_fab_i.bytesel;
        src_fab_o.eof <= '0';
  
    end case;

    src_fab_o.addr <= c_WRF_DATA;
  end process;

  snk_dreq_o       <= src_dreq_i and in_payload;
  src_fab_o.sof    <= snk_fab_i.sof;
  src_fab_o.error  <= snk_fab_i.error;


end behavioral;

