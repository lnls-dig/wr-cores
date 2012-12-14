-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint - TX packet injection unit
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : ep_tx_packet_injection.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2012-11-01
-- Last update: 2012-11-16
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Asynchronously sends pre-defined packets upon a hardware request.
-- Packet contents are defined in a buffer accessible via Wishbone. The buffer
-- is shared with the TX VLAN unit and can contain templates of up to 8 packets
-- of up to 128 bytes of size. It is possible to replace a selected 16-bit word
-- within each template with a user-provided value.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2012 CERN
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

use work.endpoint_private_pkg.all;

entity ep_tx_packet_injection is

  port
    (
      clk_sys_i : in std_logic;
      rst_n_i   : in std_logic;

      snk_fab_i  : in  t_ep_internal_fabric;
      snk_dreq_o : out std_logic;

      src_fab_o  : out t_ep_internal_fabric;
      src_dreq_i : in  std_logic;

      inject_req_i        : in  std_logic;
      inject_ready_o      : out std_logic;
      inject_packet_sel_i : in  std_logic_vector(2 downto 0);
      inject_user_value_i : in  std_logic_vector(15 downto 0);

      mem_addr_o : out std_logic_vector(9 downto 0);
      mem_data_i : in  std_logic_vector(17 downto 0)
      );

end ep_tx_packet_injection;

architecture rtl of ep_tx_packet_injection is

  type t_state is (WAIT_IDLE, SOF, DO_INJECT, EOF);

  alias template_last : std_logic is mem_data_i(16);
  alias template_user : std_logic is mem_data_i(17);

  signal state   : t_state;
  signal counter : unsigned(8 downto 0);

  signal within_packet : std_logic;
  signal select_inject : std_logic;

  signal inj_src            : t_ep_internal_fabric;
  signal inject_req_latched : std_logic;
  
begin  -- rtl

  snk_dreq_o <= '0' when (state = DO_INJECT) else src_dreq_i;

  p_detect_within : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        within_packet <= '0';
      else
        if(snk_fab_i.sof = '1')then
          within_packet <= '1';
        end if;

        if(snk_fab_i.eof = '1' or snk_fab_i.error = '1') then
          within_packet <= '0';
        end if;
      end if;
    end if;
  end process;

  p_injection_request_ready : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        inject_ready_o     <= '1';
        inject_req_latched <= '0';
      else
        if(inject_req_i = '1') then
          inject_ready_o     <= '0';
          inject_req_latched <= '1';
        elsif(state = EOF and src_dreq_i = '1') then
          inject_ready_o     <= '1';
          inject_req_latched <= '0';
        end if;
      end if;
    end if;
  end process;

  p_injection_fsm : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state         <= WAIT_IDLE;
        select_inject <= '0';
      else
        case state is
          when WAIT_IDLE =>
            inj_src.sof    <= '0';
            inj_src.eof    <= '0';
            inj_src.dvalid <= '0';
            inj_src.error  <= '0';
            select_inject  <= '0'; -- added by ML

            counter(8 downto 6) <= unsigned(inject_packet_sel_i);
            counter(5 downto 0) <= (others => '0');

            if(within_packet = '0' and inject_req_latched = '1') then
              state         <= SOF;
              select_inject <= '1';
            else
              select_inject <= '0';
            end if;
            
          when SOF =>
            if(src_dreq_i = '1') then
              inj_src.sof <= '1';
              state       <= DO_INJECT;
            end if;

          when DO_INJECT =>
            inj_src.sof <= '0';

            if(src_dreq_i = '1') then

              inj_src.dvalid <= '1';
              counter        <= counter + 1;

            else
              inj_src.dvalid <= '0';
            end if;

            if(template_last = '1' and inj_src.dvalid = '1') then
              state <= EOF;
            end if;
            
          when EOF =>
            inj_src.dvalid <= '0';
            if(src_dreq_i = '1') then
              inj_src.eof   <= '1';
              state         <= WAIT_IDLE;
              --select_inject <= '0'; -- removed by ML: we miss EOF on src_fab_o with this
            end if;
        end case;
      end if;
    end if;
  end process;

  inj_src.bytesel <= '0';
  inj_src.error   <= '0';

  p_inj_src_data : process(template_user, inject_user_value_i, mem_data_i)
  begin
    if(template_user = '1') then
      inj_src.data <= inject_user_value_i;
    else
      inj_src.data <= mem_data_i(15 downto 0);
    end if;
  end process;

  src_fab_o <= inj_src when select_inject = '1' else snk_fab_i;


  mem_addr_o <= '1' & std_logic_vector(counter);
end rtl;
