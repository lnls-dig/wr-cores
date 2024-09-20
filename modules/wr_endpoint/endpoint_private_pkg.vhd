-------------------------------------------------------------------------------
-- Title      : Private constants/types/functions package
-- Project    : White Rabbit MAC/Endpoint
-------------------------------------------------------------------------------
-- File       : endpoint_private_pkg.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-11-18
-- Last update: 2023-03-13
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Endpoint private definitions:
-- - 8B10B codes
-- - VLAN control registers
-- - Data types: internal fabric, RMON, RTU
-- - 18-bit FIFO fabric packing/unpacking functions
-- - Endpoint subcomponents declarations
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
use ieee.numeric_std.all;

use work.ep_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;
use work.endpoint_pkg.all;
use work.genram_pkg.all;

package endpoint_private_pkg is

  -- special/control characters
  constant c_k28_5 : std_logic_vector(7 downto 0) := "10111100";  -- bc
  constant c_k23_7 : std_logic_vector(7 downto 0) := "11110111";  -- f7
  constant c_k27_7 : std_logic_vector(7 downto 0) := "11111011";  -- fb
  constant c_k29_7 : std_logic_vector(7 downto 0) := "11111101";  -- fd
  constant c_k30_7 : std_logic_vector(7 downto 0) := "11111110";  -- fe
  constant c_k28_7 : std_logic_vector(7 downto 0) := "11111100";  -- fc
  constant c_d21_5 : std_logic_vector(7 downto 0) := "10110101";  -- b5

  constant c_d2_2          : std_logic_vector(7 downto 0) := "01000010";  -- 42
  constant c_d5_6          : std_logic_vector(7 downto 0) := "11000101";  -- c5
  constant c_d16_2         : std_logic_vector(7 downto 0) := "01010000";  -- 50
  constant c_preamble_char : std_logic_vector(7 downto 0) := "01010101";
  constant c_preamble_sfd  : std_logic_vector(7 downto 0) := "11010101";

  constant c_QMODE_PORT_ACCESS        : std_logic_vector(1 downto 0) := "00";
  constant c_QMODE_PORT_TRUNK         : std_logic_vector(1 downto 0) := "01";
  constant c_QMODE_PORT_UNQUALIFIED   : std_logic_vector(1 downto 0) := "11";
  constant c_QMODE_PORT_VLAN_DISABLED : std_logic_vector(1 downto 0) := "10";

  -- fixme: remove these along with the non-WB version of the endpoint
  constant c_wrsw_ctrl_none      : std_logic_vector(4 - 1 downto 0) := x"0";
  constant c_wrsw_ctrl_dst_mac   : std_logic_vector(4 - 1 downto 0) := x"1";
  constant c_wrsw_ctrl_src_mac   : std_logic_vector(4 - 1 downto 0) := x"2";
  constant c_wrsw_ctrl_ethertype : std_logic_vector(4 - 1 downto 0) := x"3";
  constant c_wrsw_ctrl_vid_prio  : std_logic_vector(4 - 1 downto 0) := x"4";
  constant c_wrsw_ctrl_tx_oob    : std_logic_vector(4 - 1 downto 0) := x"5";
  constant c_wrsw_ctrl_rx_oob    : std_logic_vector(4 - 1 downto 0) := x"6";
  constant c_wrsw_ctrl_payload   : std_logic_vector(4 - 1 downto 0) := x"7";
  constant c_wrsw_ctrl_fcs       : std_logic_vector(4 - 1 downto 0) := x"8";

  type t_ep_internal_rtu_request is record
    smac     : std_logic_vector(47 downto 0);
    dmac     : std_logic_vector(47 downto 0);
    vid      : std_logic_vector(11 downto 0);
    prio     : std_logic_vector(2 downto 0);
    has_vid  : std_logic;
    has_prio : std_logic;
    hash     : std_logic_vector(15 downto 0);
  end record;

  type t_rmon_triggers is record
    rx_sync_lost           : std_logic;
    rx_invalid_code        : std_logic;
    rx_overrun             : std_logic;
    rx_crc_err             : std_logic;
    rx_ok                  : std_logic;
    rx_pfilter_drop        : std_logic;
    rx_runt                : std_logic;
    rx_giant               : std_logic;
    rx_pause               : std_logic;
    rx_pcs_err             : std_logic;
    rx_buffer_overrun      : std_logic;
    rx_rtu_overrun         : std_logic;
    rx_path_timing_failure : std_logic;
    tx_pause               : std_logic;
    tx_underrun            : std_logic;
    rx_pclass              : std_logic_vector(7 downto 0); -- packet class (from filter)
    rx_tclass              : std_logic_vector(7 downto 0); -- traffic class (priority)
    tx_frame               : std_logic;
    rx_frame               : std_logic;
    rx_drop_at_rtu_full    : std_logic;
  end record;

  procedure f_pack_fifo_contents (
    signal fab        : in  t_ep_internal_fabric;
    signal dout       : out std_logic_vector;
    signal dout_valid : out std_logic;
    early_eof         :     boolean := false);


  procedure f_unpack_fifo_contents (
    signal din         : in  std_logic_vector;
    constant din_valid : in  std_logic;
    signal fab         : out t_ep_internal_fabric;
    early_eof          : boolean := false);


  procedure f_pack_rmon_triggers (
      signal trig_in  : in t_rmon_triggers;
      signal trig_out : out std_logic_vector(c_epevents_sz-1 downto 0));

end endpoint_private_pkg;

-------------------------------------------------------------------------------

package body endpoint_private_pkg is

  procedure f_pack_fifo_contents
    (
      signal fab        : in  t_ep_internal_fabric;
      signal dout       : out std_logic_vector;
      signal dout_valid : out std_logic;
      early_eof         :     boolean := false) is
  begin
    -- the encodings are slightly different:
    -- - if early_eof == 1, the target needs the EOF information along with the last data word.
    --   This is the case for ep_tx_pcs.
    -- - if early_eof == 0, EOF is an independent transfer
    if(early_eof) then
      if(fab.sof = '1' or fab.error = '1') then
        -- tag = 01
        dout(17 downto 16) <= "01";
        dout(15)           <= fab.sof;
        dout(14)           <= 'X';
        dout(13)           <= fab.error;
        dout(12 downto 0)  <= (others => 'X');
        dout_valid         <= '1';
      elsif(fab.eof = '1' and 
         fab.dvalid = '1') then -- ML: it happened that the last word was "stalled" by PCS (dreq_i LOW)
                                -- at the EOF... CRC indiated that this word is not valid (dvalid LOW)
                                -- but this information was ignored... and tx was hanging the tree
        -- tag = 1x
        dout(17)          <= '1';
        dout(16)          <= fab.bytesel;
        dout(15 downto 0) <= fab.data;
        dout_valid        <= '1';
      elsif(fab.dvalid = '1') then
        -- tag = 00
        dout(17)          <= '0';
        dout(16)          <= '0';
        dout(15 downto 0) <= fab.data;
        dout_valid        <= '1';
      else
        dout(17 downto 0) <= (others => 'X');
        dout_valid        <= '0';
      end if;
    else
      if(fab.sof = '1' or fab.error = '1' or fab.eof = '1' or fab.has_rx_timestamp = '1') then
        -- tag = 01
        dout(17)          <= 'X';
        dout(16)          <= '1';
        dout(15)          <= fab.sof;
        dout(14)          <= fab.eof;
        dout(13)          <= fab.error;
        dout(12)          <= fab.has_rx_timestamp;
        dout(11)          <= fab.rx_timestamp_valid;
        dout(10 downto 0) <= (others => 'X');
        dout_valid        <= '1';
      elsif(fab.dvalid = '1') then
        dout(17)          <= fab.bytesel;
        dout(16)          <= '0';
        dout(15 downto 0) <= fab.data;
        dout_valid        <= '1';
      else
        dout(17 downto 0) <= (others => 'X');
        dout_valid        <= '0';
      end if;

    end if;
  end f_pack_fifo_contents;

  procedure f_unpack_fifo_contents
    (
      signal din         : in  std_logic_vector;
      constant din_valid : in  std_logic;
      signal fab         : out t_ep_internal_fabric;
      early_eof          : boolean := false) is
  begin

    fab.data <= din(15 downto 0);
    fab.addr <= (others => '0');
    if(din_valid = '1') then
      if(early_eof) then
        fab.dvalid             <= not (not din(17) and din(16));
        fab.sof                <= not din(17) and din(16) and din(15);
        fab.eof                <= din(17);
        fab.error              <= not din(17) and din(16) and din(13);
        fab.has_rx_timestamp   <= '0';
        fab.rx_timestamp_valid <= '0';
        fab.bytesel            <= din(17) and din(16);

      else
        fab.dvalid             <= not din(16);
        fab.sof                <= din(16) and din(15);
        fab.eof                <= din(16) and din(14);
        fab.error              <= din(16) and din(13);
        fab.has_rx_timestamp   <= din(16) and din(12);
        fab.rx_timestamp_valid <= din(16) and din(11);
        fab.bytesel            <= (not din(16)) and din(17);
      end if;
    else
      fab.bytesel            <= 'X';
      fab.dvalid             <= '0';
      fab.sof                <= '0';
      fab.eof                <= '0';
      fab.error              <= '0';
      fab.has_rx_timestamp   <= '0';
      fab.rx_timestamp_valid <= '0';
      fab.data               <= (others => 'X');
    end if;
  end f_unpack_fifo_contents;


  procedure f_pack_rmon_triggers
    (
      signal trig_in  : in t_rmon_triggers;
      signal trig_out : out std_logic_vector(c_epevents_sz-1 downto 0)) is
  begin
    --from 1000base pcs
    trig_out(0) <= trig_in.tx_underrun;
    trig_out(1) <= trig_in.rx_overrun;
    trig_out(2) <= trig_in.rx_invalid_code;
    trig_out(3) <= trig_in.rx_sync_lost;
    trig_out(4) <= trig_in.rx_pause;
    trig_out(5) <= trig_in.rx_pfilter_drop;
    trig_out(6) <= trig_in.rx_pcs_err;
    trig_out(7) <= trig_in.rx_giant;
    trig_out(8) <= trig_in.rx_runt;
    trig_out(9) <= trig_in.rx_crc_err;
    trig_out(17 downto 10) <= trig_in.rx_pclass(7 downto 0);
    trig_out(18)<= trig_in.tx_frame;
    trig_out(19)<= trig_in.rx_frame;
    trig_out(20)<= trig_in.rx_drop_at_rtu_full;
    trig_out(28 downto 21) <= trig_in.rx_tclass(7 downto 0);
  end f_pack_rmon_triggers;


end endpoint_private_pkg;

-------------------------------------------------------------------------------
