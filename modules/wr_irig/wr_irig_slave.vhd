-------------------------------------------------------------------------------
-- Title      : WR Switch IRIG slave module
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : wr_irig_slave.vhd
-- Author     : Harvey Leicester
-- Company    : CERN BE-CEM-EDL
-- Created    : 2024-10-23
-- Last update: 2024-10-23
-- Platform   : FPGA-generic
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- IRIG-B004 slave interface
-------------------------------------------------------------------------------
--
-- Copyright (c) 2012 - 2024 CERN / BE-CEM-EDL
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

use work.gencores_pkg.all;

entity wr_irig_slave is
  generic (
    g_clks_per_ms : integer := 62500
  );
  port (
    clk_sys_i     : in std_logic;   --sys / sampling clock
    rst_sys_n_i   : in std_logic;   --sys rst
    irig_i        : in std_logic;   --irig data in

    secs_o        : out std_logic_vector(7 downto 0);  --seconds
    mins_o        : out std_logic_vector(8 downto 0);  --minutes
    hrs_o         : out std_logic_vector(8 downto 0);  --hours
    days_o        : out std_logic_vector(10 downto 0); --day of year
    year_o        : out std_logic_vector(8 downto 0);  --year
    ctrl0_o       : out std_logic_vector(8 downto 0);  --control function 0
    ctrl1_o       : out std_logic_vector(8 downto 0);  --contorl function 1
    sbs_o         : out std_logic_vector(17 downto 0); --straight binary seconds
    valid_o       : out std_logic;                     --output valid
    pps_o         : out std_logic  --pps out, generated from start of irig frame
  );
end entity wr_irig_slave;

architecture rtl of wr_irig_slave is

  constant c_MARKER_MS : integer := 8;
  constant c_HIGH_MS   : integer := 5;
  constant c_LOW_MS    : integer := 2;
  constant c_NUM_FIELDS : integer := 10;
  constant c_MARKER_CNT_SOF : integer := 2;
  constant c_TICK_CNT_FRAME : integer := 1000;
  constant c_TICK_CNT_PREF  : integer := 991;

  type t_irig_frame_state is
  (
    S_IDLE,
    S_PARSE8B,
    S_WAITMARKER,
    S_PARSE9B
  );

  type t_irig_smpl_state is
  (
    S_IDLE,
    S_SMPL
  );

  signal irig_frame_state : t_irig_frame_state := S_IDLE;
  signal irig_smpl_state  : t_irig_smpl_state  := S_IDLE;

  type t_field_array is array (0 to c_NUM_FIELDS-1) of std_logic_vector(8 downto 0);

  signal field : t_field_array;
  signal d_synced, d_rising, d_falling  : std_logic;
  signal sreg : std_logic_vector(8 downto 0);
  signal high_cnt, field_cnt, marker_cnt : unsigned(3 downto 0);
  signal tick : std_logic;
  signal clk_cnt  : unsigned(15 downto 0);
  signal bit_count : unsigned(3 downto 0);
  signal tick_cnt : unsigned(9 downto 0);

  signal sof, synced : std_logic;
  signal marker, irig_data : std_logic;
  signal d_valid, smpl_err : std_logic;
  signal pps_int : std_logic;

begin

  U_sync_data: gc_sync_ffs
  port map
  (
    clk_i    => clk_sys_i,
    rst_n_i  => '1',
    data_i   => irig_i,
    synced_o => d_synced,
    npulse_o => d_falling,
    ppulse_o => d_rising
  );

  p_tick_gen: process(clk_sys_i) is 
  begin 
    if rising_edge(clk_sys_i) then 
      if (rst_sys_n_i = '0' or d_rising = '1') then 
        clk_cnt <= to_unsigned(g_clks_per_ms/2, clk_cnt'length);
      else 
        clk_cnt <= clk_cnt+1;
        if(clk_cnt >=(g_clks_per_ms)-1) then
          clk_cnt <= (others => '0');
        end if;
      end if;
    end if;
  end process;

  --sample tick
  tick <= '1' when (clk_cnt = g_clks_per_ms-1) else '0';

  p_smpl_sm: process(clk_sys_i) is 
  begin 
    if rising_edge(clk_sys_i) then 
      if (rst_sys_n_i = '0') then 
        irig_smpl_state <= S_IDLE;
        marker_cnt <= (others => '0');
      else 

        irig_data <= '0';
        d_valid   <= '0';
        marker    <= '0';
        sof       <= '0';
        smpl_err  <= '0';

        case irig_smpl_state is

          when S_IDLE =>  high_cnt <= (others => '0');
                          if(tick = '1') then 
                            if(d_synced = '1') then 
                              high_cnt <= high_cnt+1;
                              irig_smpl_state <= S_SMPL;
                            end if;
                          end if;

          when S_SMPL =>  if(tick = '1') then 
                            if(d_synced = '1') then
                              high_cnt <= high_cnt+1;
                            else
                              marker_cnt <= (others => '0');
                              irig_smpl_state <= S_IDLE;
                              if(high_cnt = c_HIGH_MS) then
                                irig_data <= '1';
                                d_valid   <= '1';
                              elsif(high_cnt = c_LOW_MS) then
                                irig_data <= '0';
                                d_valid   <= '1';
                              elsif(high_cnt = c_MARKER_MS) then
                                marker    <= '1';
                                marker_cnt <= marker_cnt + 1;
                                if(marker_cnt >= c_MARKER_CNT_SOF-1) then
                                  sof <= '1';
                                  marker_cnt <= (others => '0');
                                end if;
                              else
                                smpl_err <= '1';  --something gone wrong
                              end if;
                            end if;
                          end if;

          when others => irig_smpl_state <= S_IDLE;

        end case;
      end if;
    end if;
  end process;

  --counter synced to sof
  p_tick_cnt: process(clk_sys_i) is
  begin
    if rising_edge(clk_sys_i) then
      if (rst_sys_n_i = '0' or sof = '1') then
        tick_cnt <= (others => '0');
      else
          if(tick = '1') then
            tick_cnt <= tick_cnt + 1;
          end if;
        end if;
    end if;
  end process;

  --check got sof when expected, used to generate valid and gate pps
  p_sync_gen: process(clk_sys_i) is
  begin
    if rising_edge(clk_sys_i) then
      if(rst_sys_n_i = '0') then
        synced <= '0';
      else
        if (tick_cnt >= c_TICK_CNT_FRAME) then
          synced <= '0';
          if(sof = '1') then
            synced <= '1';
          end if;
        end if;
      end if;
    end if;
  end process;

  --parse the frame
  p_frame_sm: process(clk_sys_i) is 
  begin
    if rising_edge(clk_sys_i) then 
      if(rst_sys_n_i = '0') then 
        field <= (others => (others => '0'));
        irig_frame_state <= S_IDLE;
      else

        case irig_frame_state is

          when S_IDLE =>  field_cnt <= (others => '0');
                          if(sof = '1') then
                            irig_frame_state <= S_PARSE8B;  --first field (seconds) is 8 bits, others are 9
                          end if;

          when S_PARSE8B => if(tick = '1') then
                              if (bit_count >= 8) then
                                irig_frame_state <= S_WAITMARKER;
                              end if;
                            elsif(smpl_err = '1' or marker = '1') then
                              irig_frame_state <= S_IDLE;
                            end if;

          when S_WAITMARKER =>  if(marker = '1') then
                                  irig_frame_state <= S_PARSE9B;
                                  field(to_integer(field_cnt)) <= sreg;
                                  field_cnt <= field_cnt + 1;
                                  if(field_cnt >= c_NUM_FIELDS-1) then
                                    irig_frame_state <= S_IDLE;
                                  end if;
                                elsif(smpl_err = '1' or d_valid = '1') then
                                    irig_frame_state <= S_IDLE;
                                end if;

          when S_PARSE9B => if(tick = '1') then
                              if (bit_count >= 9) then
                                irig_frame_state <= S_WAITMARKER;
                              end if;
                            elsif(smpl_err = '1' or marker = '1') then
                              irig_frame_state <= S_IDLE;
                            end if;

          when others => irig_frame_state <= S_IDLE;

        end case;
      end if;
    end if;
  end process;

  --generate pps on first edge of p0 marker
  pps_int <= d_rising when tick_cnt = c_TICK_CNT_PREF and synced = '1' else '0';

  p_bit_count: process(clk_sys_i) is 
  begin 
    if rising_edge(clk_sys_i) then
      if(rst_sys_n_i='0' or marker='1') then
        bit_count <= (others => '0');
      elsif (d_valid = '1') then
        bit_count <= bit_count+1;
      end if;      
    end if;
  end process;

  p_shift_reg: process(clk_sys_i) is
  begin 
    if rising_edge(clk_sys_i) then 
      if (marker = '1') then
        sreg <= (others => '0');
      elsif (d_valid = '1') then
        sreg <= irig_data & sreg(sreg'length-1 downto 1);
      end if;        
    end if;
  end process;

  p_outputs: process(clk_sys_i) is
  begin
    if rising_edge(clk_sys_i) then
      if(rst_sys_n_i = '0') then
        secs_o  <= (others => '0');
        mins_o  <= (others => '0');
        hrs_o   <= (others => '0');
        days_o  <= (others => '0');
        year_o  <= (others => '0');
        ctrl0_o <= (others => '0');
        ctrl1_o <= (others => '0');
        sbs_o   <= (others => '0');
      else
        if(pps_int = '1') then
          secs_o  <= field(0)(8 downto 1);
          mins_o  <= field(1);
          hrs_o   <= field(2);
          days_o  <= field(4)(1 downto 0) & field(3);
          year_o  <= field(5);
          ctrl0_o <= field(6);
          ctrl1_o <= field(7);
          sbs_o   <= field(9) & field(8);
        end if;
      end if;
    end if;
  end process;

  valid_o <= synced;
  pps_o   <= pps_int;

end architecture;
