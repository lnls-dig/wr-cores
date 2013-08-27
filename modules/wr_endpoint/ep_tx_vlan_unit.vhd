-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint - TX VLAN unit
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : ep_tx_vlan_unit.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2012-11-01
-- Last update: 2012-11-16
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Performs VLAN untagging, if the VID of the egress packet is in
-- the VLAN untagged set.
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
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2012-11-01  1.0      twlostow	    Created
-- 2013-04-24  1.1      mlipinsk	    corrected VLAN untagging
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.genram_pkg.all;
use work.wr_fabric_pkg.all;
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;

entity ep_tx_vlan_unit is

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    snk_fab_i  : in  t_ep_internal_fabric;
    snk_dreq_o : out std_logic;

    src_fab_o  : out t_ep_internal_fabric;
    src_dreq_i : in  std_logic;

-- Shared buffer interface to the packet injection unit
    inject_mem_addr_i : in  std_logic_vector(9 downto 0);
    inject_mem_data_o : out std_logic_vector(17 downto 0);

    regs_i : in t_ep_out_registers
    );


end ep_tx_vlan_unit;

architecture behavioral of ep_tx_vlan_unit is


  type t_state is (IDLE, CHECK_ETHERTYPE, PUSH_QHEADER_1, PUSH_QHEADER_2, POP_QHEADER_1, POP_QHEADER_2, POP_QHEADER_3, POP_ETHERTYPE);

-- general signals
  signal state   : t_state;
  signal counter : unsigned(2 downto 0);

  signal vut_rd_vid : std_logic_vector(11 downto 0);
  signal vut_wr_vid : std_logic_vector(11 downto 0);
  signal vut_untag  : std_logic;

  signal vut_stored_tag       : std_logic_vector(15 downto 0);
  signal vut_stored_ethertype : std_logic_vector(15 downto 0);

  signal mem_addr_muxed : std_logic_vector(9 downto 0);
  signal mem_rdata      : std_logic_vector(17 downto 0);
  signal src_dreq_d0    : std_logic;

begin  -- behavioral

  vut_rd_vid <= snk_fab_i.data(11 downto 0);

  mem_addr_muxed <= ("00" & vut_rd_vid(11 downto 4)) when state /= IDLE else inject_mem_addr_i;

  U_Untagged_Set_RAM : generic_dpram
    generic map (
      g_data_width => 18,
      g_size       => 1024,
      g_dual_clock => false)
    port map (
      rst_n_i => rst_n_i,
      clka_i  => clk_sys_i,
      clkb_i  => '0',
      wea_i   => '0',
      aa_i    => mem_addr_muxed,
      qa_o    => mem_rdata,
      web_i   => regs_i.vcr1_offset_wr_o,
      ab_i    => regs_i.vcr1_offset_o,
      db_i    => regs_i.vcr1_data_o);

  inject_mem_data_o <= mem_rdata;

  vut_untag <= mem_rdata(to_integer(unsigned(vut_stored_tag(3 downto 0))));

  p_delay_signals : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      src_dreq_d0 <= src_dreq_i;
      
    end if;
  end process;

  p_main_fsm : process (clk_sys_i)
  begin  -- process
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state <= IDLE;
      else
        case state is
          when IDLE =>
            if(snk_fab_i.sof = '1') then
              counter <= (others => '0');
            end if;
            if(snk_fab_i.dvalid = '1' and counter /= 6) then
              counter <= counter + 1;
            end if;

            if(snk_fab_i.dvalid = '1' and counter = 5) then
              state <= CHECK_ETHERTYPE;
            end if;

          when CHECK_ETHERTYPE =>
            vut_stored_ethertype <= snk_fab_i.data;

            if(snk_fab_i.dvalid = '1') then

              if(snk_fab_i.data = x"8100") then
                state <= PUSH_QHEADER_1;
              else
                state <= IDLE;
              end if;
            end if;

          when PUSH_QHEADER_1 =>

            if(snk_fab_i.dvalid = '1') then
              vut_stored_tag <= snk_fab_i.data;
              state                <= PUSH_QHEADER_2;
            end if;

          when PUSH_QHEADER_2 =>

            if(snk_fab_i.dvalid = '1') then
              vut_stored_ethertype <= snk_fab_i.data; 
              if(vut_untag = '1') then
                state <= POP_ETHERTYPE;
              else
                state <= POP_QHEADER_1;
              end if;
            end if;

          when POP_ETHERTYPE =>
            if(src_dreq_d0 = '1') then
              state <= IDLE;
            end if;

          when POP_QHEADER_1 =>
            if(src_dreq_d0 = '1') then
              state <= POP_QHEADER_2;
            end if;

          when POP_QHEADER_2 =>
            if(src_dreq_d0 = '1') then
              state <= POP_QHEADER_3;
            end if;

          when POP_QHEADER_3 =>
            if(src_dreq_d0 = '1') then
              state <= IDLE;
            end if;

        end case;
      end if;
    end if;
  end process;


  p_main_fsm_comb : process(snk_fab_i, src_dreq_d0, state, vut_stored_tag, vut_stored_ethertype, counter)
  begin

    case state is
      when IDLE =>
        src_fab_o.sof     <= snk_fab_i.sof;
        src_fab_o.eof     <= snk_fab_i.eof;
        src_fab_o.error   <= snk_fab_i.error;
        src_fab_o.bytesel <= snk_fab_i.bytesel;
      when others =>
        src_fab_o.sof     <= '0';
        src_fab_o.eof     <= '0';
        src_fab_o.error   <= '0';
        src_fab_o.bytesel <= '0';
    end case;

    case state is
      when IDLE =>
        snk_dreq_o       <= src_dreq_i;
        src_fab_o.dvalid <= snk_fab_i.dvalid;
        src_fab_o.data   <= snk_fab_i.data;
        src_fab_o.addr   <= snk_fab_i.addr;

      when CHECK_ETHERTYPE =>
        snk_dreq_o <= src_dreq_i;
        if(snk_fab_i.data /= x"8100") then
          src_fab_o.dvalid <= snk_fab_i.dvalid;
        else
          src_fab_o.dvalid <= '0';
        end if;
        src_fab_o.data <= snk_fab_i.data;
        src_fab_o.addr <= snk_fab_i.addr;
        
      when PUSH_QHEADER_1 =>
        snk_dreq_o       <= '1';
        src_fab_o.dvalid <= '0';
        src_fab_o.data   <= (others => 'X');

      when PUSH_QHEADER_2 =>
        snk_dreq_o       <= not snk_fab_i.dvalid;
        src_fab_o.dvalid <= '0';
        src_fab_o.data   <= (others => 'X');

--       when CHECK_UNTAG =>
--         snk_dreq_o       <= '0';
--         src_fab_o.dvalid <= '0';
--         src_fab_o.data   <= (others => 'X');
        
      when POP_ETHERTYPE =>
        snk_dreq_o       <= src_dreq_i and src_dreq_d0;
        src_fab_o.dvalid <= src_dreq_d0;
        src_fab_o.data   <= vut_stored_ethertype;

      when POP_QHEADER_1 =>
        snk_dreq_o       <= '0';
        src_fab_o.dvalid <= src_dreq_d0;
        src_fab_o.data   <= x"8100";

      when POP_QHEADER_2 =>
        snk_dreq_o       <= '0';
        src_fab_o.dvalid <= src_dreq_d0;
        src_fab_o.data   <= vut_stored_tag;--vut_stored_ethertype;

      when POP_QHEADER_3 =>
        snk_dreq_o       <= src_dreq_i and src_dreq_d0;
        src_fab_o.dvalid <= src_dreq_d0;
        src_fab_o.data   <= vut_stored_ethertype; --vut_stored_tag;
    end case;
  end process;
  
end behavioral;

