--! @file eca_scan.vhd
--! @brief Scanner, looking for actions to put into calendar
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2016 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This entity receives deadlines as input and outputs them later. 
--! Assuming time_i increases 2**multiplier ticks/cycle, it outputs stored
--! items one cycle before time_i is in (deadline-period, deadline].  If
--! time_i jumps, eca_scan outputs items exactly once within one second.
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

library work;
use work.eca_internals_pkg.all;

entity eca_scan is
  generic(
    g_ext_size       : natural; -- extra bits tracked by scanner
    g_log_size       : natural; -- 2**g_log_size       = buffer entries
    g_log_multiplier : natural; -- 2**g_log_multiplier = ticks per cycle
    g_log_max_delay  : natural; -- 2**g_log_max_delay  = maximum delay before executed as early
    g_log_latency    : natural);-- 2**g_log_latency    = ticks of calendar delay
  port(
    clk_i        : in  std_logic;
    rst_n_i      : in  std_logic;
    -- Current time, minus some coefficient
    time_i       : in  t_time;
    -- Write port
    wen_i        : in  std_logic;
    stall_o      : out std_logic;
    deadline_i   : in  t_time;
    idx_i        : in  std_logic_vector(g_log_size-1 downto 0);
    ext_i        : in  std_logic_vector(g_ext_size-1 downto 0);
    -- Scan reports ready for calendar (each op is held for two cycles)
    scan_stb_o   : out std_logic;
    scan_late_o  : out std_logic;
    scan_early_o : out std_logic;
    scan_low_o   : out std_logic_vector(g_log_latency-1 downto 0);
    scan_idx_o   : out std_logic_vector(g_log_size   -1 downto 0);
    scan_ext_o   : out std_logic_vector(g_ext_size   -1 downto 0));
end eca_scan;

architecture rtl of eca_scan is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_log_count : natural := g_log_max_delay - g_log_latency;
  constant c_multiplier1_bits : std_logic_vector(g_log_multiplier downto 0) := (others => '0');
  
  -- Fields in the table
  constant c_valid : natural := 0;
  constant c_late  : natural := 1;
  constant c_early : natural := 2;
  -- subtype  c_ext is natural range x downto x;
  constant c_ext   : std_logic_vector(g_ext_size   +c_early    downto c_early   +1) := (others => '0');
  constant c_low   : std_logic_vector(g_log_latency+c_ext'high downto c_ext'high+1) := (others => '0');
  constant c_count : std_logic_vector(c_log_count  +c_low'high downto c_low'high+1) := (others => '0');
  constant c_data_len : natural := c_count'high+1;
  
  signal s_ai : std_logic_vector(c_data_len-1 downto 0);
  signal s_bi : std_logic_vector(c_data_len-1 downto 0);
  signal s_bo : std_logic_vector(c_data_len-1 downto 0);
  
  -- The main challenge is to calculate how many passes through the table it will
  -- take until the element should be output in order to fall in the target range.
  -- An action is output from this entity when it's counter reaches zero and the
  -- current low(time_i) = the idx_i where the deadline was written.
  
  -- Formally, 
  --   Let deadline be the recevied deadline
  --   Let input    be the time we receive the item
  --   Let output   be the time we output the item
  --   Let count    be the number of times we must decrement the index
  --   Let idx      be the index in the table where we store the item
  --
  --   We require output in (deadline-period, deadline]
  --   Since we index the table using low(time_i), we have low(output)=idx
  --   Thus, we want output = high(deadline-idx*2*multiplier) & idx & '0'*(log_multiplier+1)
  --      This is essentially floor(deadline-idx*2*multiplier) + idx*2*multiplier.
  --      idx is within period/multiplier/2, so this output time is correct
  -- 
  --  Between input and output, how many times is idx decremented?
  --  count = (output-input)/period
  --
  --  output = high(deadline - idx * 2 * M) & idx & '0'*(logM + 1);
  --  count  = high(output - input + period)
  --
  --  Note: we want count to cap out at all '1's or =1.
  
  signal r_time3     : t_time := (others => '0');
  signal r_time2     : t_time := (others => '0');
  signal r_time1     : t_time := (others => '0');
  
  signal r_wen1      : std_logic := '0';
  signal r_wen2      : std_logic := '0';
  signal r_wen3      : std_logic := '0';
  
  signal s_ready     : std_logic;
  signal r_reset     : std_logic_vector(g_log_size downto 0) := (others => '0');
  
  signal r_deadline1 : std_logic_vector(t_time'length downto 0);
  signal r_output2   : std_logic_vector(t_time'length downto 0);
  signal r_count3    : std_logic_vector(t_time'length downto 0);
  signal s_count3    : std_logic_vector(c_log_count-1 downto 0);
  signal s_late3     : std_logic;
  signal s_early3    : std_logic;
  
  signal r_idx1      : std_logic_vector(g_log_size-1 downto 0);
  signal r_idx2      : std_logic_vector(g_log_size-1 downto 0);
  signal r_idx3      : std_logic_vector(g_log_size-1 downto 0);
  signal r_ext1      : std_logic_vector(g_ext_size-1 downto 0);
  signal r_ext2      : std_logic_vector(g_ext_size-1 downto 0);
  signal r_ext3      : std_logic_vector(g_ext_size-1 downto 0);
  signal r_low2      : std_logic_vector(g_log_latency-1 downto 0);
  signal r_low3      : std_logic_vector(g_log_latency-1 downto 0);
  
  signal r_bypass    : std_logic;
  signal r_ai        : std_logic_vector(c_data_len-1 downto 0);
  signal s_bo_raw    : std_logic_vector(c_data_len-1 downto 0);
  
  signal r_stb       : std_logic := '0';
  signal s_weno      : std_logic;
  signal s_b_wen     : std_logic;
  signal s_b_addr    : std_logic_vector(g_log_size-1 downto 0);
  signal s_idxo      : std_logic_vector(g_log_size-1 downto 0);
  signal r_idxo      : std_logic_vector(g_log_size-1 downto 0) := (others => '0');
  signal s_valido    : std_logic;
  signal s_lateo     : std_logic;
  signal s_earlyo    : std_logic;
  signal s_exto      : std_logic_vector(g_ext_size   -1 downto 0);
  signal s_lowo      : std_logic_vector(g_log_latency-1 downto 0);
  signal s_counto    : std_logic_vector(c_log_count  -1 downto 0);
  signal s_not_zero  : std_logic;

begin

  sane :
    assert g_log_size > 0
    report "g_log_size must be positive"
    severity failure;

  correct :
    assert g_log_size + 1 = g_log_latency - g_log_multiplier
    report "g_log_size does not match target latency"
    severity failure;
  
  -- Reset wipes memory
  s_ready <= r_reset(r_reset'high);
  reset : process(rst_n_i, clk_i) is
  begin
    if rst_n_i = '0' then
      r_reset <= (others => '0');
    elsif rising_edge(clk_i) then
      r_reset <= f_eca_add(r_reset, not s_ready);
    end if;
  end process;
  
  -- Compensate for pipeline delay with an adjusted clock
  time : process(rst_n_i, clk_i) is
  begin
    if rst_n_i = '0' then
      r_time3 <= (others => '0');
      r_time2 <= (others => '0');
      r_time1 <= (others => '0');
    elsif rising_edge(clk_i) then
      r_time3 <= f_eca_add(time_i, 5*2**g_log_multiplier);
      r_time2 <= r_time3;
      r_time1 <= r_time2;
    end if;
  end process;
  
  -- Clear the write-enable registers
  input_wen : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_wen1 <= '0';
      r_wen2 <= '0';
      r_wen3 <= '0';
    elsif rising_edge(clk_i) then
      r_wen1 <= wen_i and s_ready;
      r_wen2 <= r_wen1;
      r_wen3 <= r_wen2;
    end if;
  end process;
  
  -- Calculate number of times to count-down until we output the value
  input_deadline : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_deadline1 <= '0' & deadline_i;
      r_idx1      <= idx_i;
      r_ext1      <= ext_i;
      
      r_output2   <= std_logic_vector(unsigned(r_deadline1) - (unsigned(r_idx1) & unsigned(c_multiplier1_bits)));
      r_output2(r_idx1'high+c_multiplier1_bits'length downto r_idx1'low+c_multiplier1_bits'length) <= r_idx1;
      r_output2(c_multiplier1_bits'range) <= c_multiplier1_bits;
      r_idx2      <= r_idx1;
      r_ext2      <= r_ext1;
      r_low2      <= r_deadline1(g_log_latency-1 downto 0);
      
      r_count3    <= std_logic_vector(unsigned(r_output2) - unsigned(r_time3));
      r_idx3      <= r_idx2;
      r_ext3      <= r_ext2;
      r_low3      <= r_low2;
    end if;
  end process;
  
  s_late3  <= r_count3(time_i'length);
  s_early3 <= f_eca_or(r_count3(time_i'high downto g_log_max_delay)) and not s_late3;
  
  -- Saturate the counter calculation
  s_count3 <=
    (others => '0') when s_late3 ='1' else
    (others => '1') when s_early3='1' else
    r_count3(g_log_max_delay-1 downto g_log_latency);

  -- Write-write race is prevented by never writing-back something not valid
  -- ... and we know that scan is never asked to write to something already valid
  table : eca_tdp
    generic map(
      g_addr_bits => g_log_size,
      g_data_bits => c_data_len)
    port map(
      clk_i    => clk_i,
      a_wen_i  => r_wen3,
      a_addr_i => r_idx3,
      a_data_i => s_ai,
      a_data_o => open,
      b_wen_i  => s_b_wen,
      b_addr_i => s_b_addr,
      b_data_i => s_bi,
      b_data_o => s_bo_raw);

  -- Inject writes during reset
  s_b_wen  <= not s_ready or s_weno;
  s_b_addr <= f_eca_mux(s_ready, s_idxo, r_reset(g_log_size-1 downto 0));
  
  s_ai(c_valid)       <= '1';
  s_ai(c_late)        <= s_late3;
  s_ai(c_early)       <= s_early3;
  s_ai(c_ext'range)   <= r_ext3;
  s_ai(c_low'range)   <= r_low3;
  s_ai(c_count'range) <= s_count3;
  
  -- Resolve write during read to newest data
  bypass : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      if s_ready = '1' then
        r_bypass <= r_wen3 and f_eca_eq(r_idx3, s_idxo);
        r_ai     <= s_ai;
      else
        r_bypass <= '1';
        r_ai     <= (others => '0');
      end if;
    end if;
  end process;
  s_bo <= f_eca_mux(r_bypass, r_ai, s_bo_raw);
  
  s_valido <= s_bo(c_valid);
  s_lateo  <= s_bo(c_late);
  s_earlyo <= s_bo(c_early);
  s_exto   <= s_bo(c_ext'range);
  s_lowo   <= s_bo(c_low'range);
  s_counto <= s_bo(c_count'range);
  
  s_not_zero <= f_eca_or(s_counto);
  s_bi(c_valid)       <= s_not_zero;
  s_bi(c_late)        <= s_lateo;
  s_bi(c_early)       <= s_earlyo;
  s_bi(c_ext'range)   <= s_exto;
  s_bi(c_low'range)   <= s_lowo;
  s_bi(c_count'range) <= f_eca_add(s_counto, -1);
  
  -- Write on every second cycle, holding write index fixed to last read index
  s_weno <= not r_time1(g_log_multiplier) and r_time2(g_log_multiplier) and s_valido;
  s_idxo <= f_eca_mux(r_time2(g_log_multiplier), r_idxo, r_time2(g_log_latency-1 downto g_log_multiplier+1));
  
  scan : process(rst_n_i, clk_i) is
  begin
    if rst_n_i = '0' then
      r_idxo <= (others => '0');
      r_stb  <= '0';
    elsif rising_edge(clk_i) then
      r_idxo <= s_idxo;
      r_stb  <= not s_not_zero and s_weno;
    end if;
  end process;
  
  scan_stb_o <= r_stb;
  output : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      if s_weno = '1' then
        scan_late_o  <= s_lateo;
        scan_early_o <= s_earlyo;
        scan_low_o   <= s_lowo;
        scan_idx_o   <= r_idxo;
        scan_ext_o   <= s_exto;
      end if;
    end if;
  end process;
  
  stall_o <= not s_ready;
  
end rtl;
