--! @file eca_piso_fifo.vhd
--! @brief ECA PISO FIFO
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! A simple single-clock parallel-in serial-out FIFO
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

entity eca_piso_fifo is
  generic(
    g_log_size  : natural; -- g_log_size > g_log_ports
    g_log_ports : natural;
    g_width     : natural);
  port(
    clk_i   : in  std_logic;
    rst_n_i : in  std_logic;
    push_i  : in  std_logic_vector(2**g_log_ports-1 downto 0);
    data_i  : in  t_eca_matrix(2**g_log_ports-1 downto 0, g_width-1 downto 0);
    pop_i   : in  std_logic;
    valid_o : out std_logic;
    fresh_o : out std_logic; -- If the record was not delayed in the FIFO
    data_o  : out std_logic_vector(g_width-1 downto 0));
end eca_piso_fifo;

architecture rtl of eca_piso_fifo is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  -- Deal with a fifo smaller than it is wide by rounding up
  function f_sub_min1(x, y : natural) return natural is
  begin
    if x > y then return x - y; else return 1; end if;
  end f_sub_min1;

  constant c_log_mem  : natural := f_sub_min1(g_log_size, g_log_ports);
  constant c_log_size : natural := c_log_mem + g_log_ports;
  constant c_ports    : natural := 2**g_log_ports;
  constant c_undef    : std_logic_vector(g_width-1 downto 0) := (others => 'X');
  
  function f_idx(r, c : natural) return natural is
  begin
    return r*2*c_ports + c;
  end f_idx;
  
  function f_sub1(x : natural) return natural is
  begin
    if x = 0 then return 0; else return x-1; end if;
  end f_sub1;

  signal s_ina     : t_eca_matrix(g_log_ports downto 0, c_ports-1 downto 0);
  signal s_inb     : t_eca_matrix(g_log_ports downto 0, c_ports-1 downto 0);
  signal s_xor     : t_eca_matrix(g_log_ports downto 0, c_ports-1 downto 0);
  signal s_and     : t_eca_matrix(g_log_ports downto 0, c_ports-1 downto 0);
  signal s_last    : unsigned(g_log_ports downto 0);
  
  signal r_holes   : unsigned(g_log_ports downto 0) := to_unsigned(c_ports, g_log_ports+1);
  signal s_inc     : unsigned(g_log_ports downto 0) := (others => '1');
  signal s_holes   : unsigned(g_log_ports downto 0);
  signal r_inc     : std_logic := '0';
  
  type t_holes is array(c_ports downto 0) of unsigned(g_log_ports downto 0);
  signal s_subs : t_holes;
  
  signal r_control : t_eca_matrix(g_log_ports downto 0, 2*c_ports-1 downto 0);
  signal r_data    : t_eca_matrix(2**g_log_ports-1 downto 0, g_width-1 downto 0);
  
  type t_data is array(natural range <>) of std_logic_vector(g_width-1 downto 0);
  signal s_muxes : t_data((g_log_ports+2)*2*c_ports-1 downto 0);
  signal s_data  : t_data(c_ports-1 downto 0);
  
  signal r_ge_write : std_logic_vector(c_ports-1 downto 0) := (others => '0');
  signal s_lt_idle  : std_logic_vector(c_ports-1 downto 0);
  signal s_lt_idle2 : std_logic_vector(c_ports-1 downto 0);
  signal s_wen      : std_logic_vector(c_ports-1 downto 0);
  
  signal r_waddr  : unsigned(c_log_size downto 0) := (others => '0');
  signal rr_waddr : unsigned(c_log_size downto 0) := (others => '0');
  signal s_waddr  : unsigned(c_log_size downto 0);
  signal r_raddr  : unsigned(c_log_size downto 0) := (others => '0');
  signal s_raddr  : unsigned(c_log_size downto 0);
  
  type t_addr_high is array(c_ports-1 downto 0) of unsigned(c_log_mem-1 downto 0);
  signal s_waddrs_high : t_addr_high;
  signal s_raddr_high  : unsigned(c_log_mem-1 downto 0);
  signal s_waddr_high  : unsigned(c_log_mem-1 downto 0);
  
  signal r_valid : std_logic := '0';
  signal r_fresh : std_logic := '1';
  signal s_pop   : std_logic;
  
begin

  -- Network to calculate the mux control lines; is prefix-sum of # of holes
  -- If this becomes the critical timing path (>32 ports), convert this network
  -- into a multi-staged network like eca_compact:prefix_xor OR increase channel latency.
  ctl_rows : for r in 0 to g_log_ports generate
    ctl_cols : for c in 0 to c_ports-1 generate
      s_ina(r,c) <= s_xor(r,f_sub1(c)) when c>0 else r_holes(r);
      s_inb(r,c) <= s_and(f_sub1(r),c) when r>0 else not push_i(c);
      s_and(r,c) <= s_ina(r,c) and s_inb(r,c);
      s_xor(r,c) <= s_ina(r,c) xor s_inb(r,c);
    end generate;
    -- The last column carries over into the first column, next cycle
    s_last(r) <= s_xor(r,c_ports-1);
  end generate;
  
  -- If there are <= c_ports holes, we must start filling next record
  s_inc(s_inc'high) <= f_eca_active_high(s_last-1 <= c_ports-1) when f_eca_safe(std_logic_vector(s_last))='1' else 'X';
  s_holes <= s_last xnor s_inc;
  
  -- Calculate all the differences from r_holes
  subs : for i in 0 to c_ports generate
    s_subs(i) <= r_holes - i;
  end generate;
  
  -- Mux control also needs the low half to be computed
  main : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_data <= data_i;
      for c in 0 to c_ports-1 loop
        if g_log_ports > 0 then
          for r in 0 to g_log_ports-1 loop
            r_control(r,c) <= s_subs(c_ports-1-c)(r);
            r_control(r,c+c_ports) <= s_xor(r,c);
          end loop;
        end if;
        -- Last mux layer is a bit different due to wrap-around
        r_control(g_log_ports,c) <= '1';
        r_control(g_log_ports,c+c_ports) <= s_xor(g_log_ports,c) or s_subs(c_ports-1-c)(g_log_ports);
        -- I is W from last cycle, so flip bits of delayed result
        r_ge_write <= not s_lt_idle;
      end loop;
    end if;
  end process;
  
  -- Fill in the initial data for the columns
  cols : for c in 0 to c_ports-1 generate
    s_muxes(f_idx(0,c)) <= (others => 'X');
    bits : for b in 0 to g_width-1 generate
      s_muxes(f_idx(0,c_ports+c))(b) <= r_data(c,b);
    end generate;
  end generate;

  -- Perform the muxing of the data into position
  rows : for r in 0 to g_log_ports generate
    cols : for c in 0 to 2*c_ports-2**r-1 generate
      s_muxes(f_idx(r+1,c)) <= 
        f_eca_mux(r_control(r,c+2**r-1), s_muxes(f_idx(r,c+2**r)), s_muxes(f_idx(r,c)));
    end generate;
    cuts : for c in 2*c_ports-2**r to 2*c_ports-1 generate
      s_muxes(f_idx(r+1,c)) <= 
        f_eca_mux(r_control(r,2*c_ports-1), c_undef, s_muxes(f_idx(r,c)));
    end generate;
  end generate;
  
  -- Bank addressing uses only the high address bits
  s_raddr_high <= s_raddr(c_log_size-1 downto g_log_ports);
  s_waddr_high <= r_waddr(c_log_size-1 downto g_log_ports);
  
  banks : for i in 0 to c_ports-1 generate
    -- FIFO bank control lines fall into one of two cases:
    --   r_inc=0 => [...idle         ] W[write] I[idle]
    --   r_inc=1 => [...write] I[idle] W[write        ]
    -- Note: I is the last W
    
    s_lt_idle(i) <= s_subs(c_ports-i)(g_log_ports);
    
    with r_inc select
    s_wen(i) <= 
      r_ge_write(i) and s_lt_idle(i)  when '0',
      r_ge_write(i) or  s_lt_idle(i)  when '1',
      'X'                             when others;
    
    -- Increment the write index for both [...write] and [idle]
    s_waddrs_high(i) <= s_waddr_high + ("" & not r_ge_write(i));

    bank : eca_sdp
      generic map(
        g_addr_bits  => c_log_mem,
        g_data_bits  => g_width,
        g_bypass     => false,
        g_dual_clock => false)
      port map(
        r_clk_i  => clk_i,
        r_addr_i => std_logic_vector(s_raddr_high),
        r_data_o => s_data(i),
        w_clk_i  => clk_i,
        w_en_i   => s_wen(i),
        w_addr_i => std_logic_vector(s_waddrs_high(i)),
        w_data_i => s_muxes(f_idx(g_log_ports+1, i)));
  
  end generate;
  
  control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_inc   <= '0';
      r_waddr <= (others => '0');
      rr_waddr <= (others => '0');
      r_raddr <= (others => '0');
      r_holes <= to_unsigned(c_ports, g_log_ports+1);
      r_valid <= '0';
      r_fresh <= '1';
    elsif rising_edge(clk_i) then
      r_inc   <= s_inc(s_inc'high);
      r_waddr <= s_waddr;
      rr_waddr <= r_waddr;
      r_raddr <= s_raddr;
      r_holes <= s_holes;
      r_valid <= f_eca_active_high(s_raddr /= r_waddr);
      r_fresh <= f_eca_active_high(s_raddr = rr_waddr);
    end if;
  end process;
  
  s_pop  <= r_valid and pop_i;
  s_raddr <= r_raddr + (""&s_pop);
  
  -- The write address is derived on the high end by th bank counter
  -- and on the low-end by the inverse of the number of free holes
  s_waddr(c_log_size downto g_log_ports) <= r_waddr(c_log_size downto g_log_ports) + (""&r_inc);
  moreports : if g_log_ports > 0 generate
    s_waddr(g_log_ports-1 downto 0) <= c_ports-1 - (r_holes(g_log_ports-1 downto 0)-1);
    data_o <= s_data(to_integer(r_raddr(g_log_ports-1 downto 0)));
  end generate;
  oneport : if g_log_ports = 0 generate
    data_o <= s_data(0);
  end generate;
  
  valid_o <= r_valid;
  fresh_o <= r_fresh;
  
end rtl;
