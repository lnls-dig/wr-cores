library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.eca_auto_pkg.all;
use work.eca_internals_pkg.all;

entity eca is
  generic(
    g_channel_types  : t_nat_array;
    g_channel_nums   : t_nat_array; -- Anything not explicitly set is 1
    g_num_streams    : natural :=  1; -- Number of input streams
    g_num_ios        : natural :=  8; -- Number of gpios
    g_log_table_size : natural :=  8; -- 2**g_log_table_size = maximum number of conditions
    g_log_queue_size : natural :=  8; -- 2**g_log_size       = maximum number of pending actions
    g_log_multiplier : natural :=  3; -- 2**g_log_multiplier = ticks per cycle
    g_log_max_delay  : natural := 32; -- 2**g_log_max_delay  = maximum delay before executed as early
    g_log_latency    : natural := 12; -- 2**g_log_latency    = ticks of calendar delay
    g_log_counter    : natural := 20);-- number of bits in the counters reported
  port(
    -- ECA control registers
    c_clk_i     : in  std_logic;
    c_rst_n_i   : in  std_logic;
    c_slave_i   : in  t_wishbone_slave_in;
    c_slave_o   : out t_wishbone_slave_out;
    -- Action clock domain
    a_clk_i     : in  std_logic;
    a_rst_n_i   : in  std_logic;
    a_time_i    : in  t_time;
    -- Input streams (lower index has priority)
    a_stream_i  : in  t_stream_array(g_num_streams-1 downto 0);
    a_stall_o   : out std_logic_vector(g_num_streams-1 downto 0);
    -- Output actions
    a_stall_i   : in  std_logic_vector(g_channel_types'range);
    a_channel_o : out t_channel_array(g_channel_types'range);
    a_io_o      : out t_eca_matrix(g_num_ios-1 downto 0, 2**g_log_multiplier-1 downto 0);
    -- Interrupts that report failure conditions
    i_clk_i     : in  std_logic;
    i_rst_n_i   : in  std_logic;
    i_master_i  : in  t_wishbone_master_in;
    i_master_o  : out t_wishbone_master_out);
end eca;

architecture rtl of eca is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_num_channels : natural := g_channel_types'length;
  constant c_channel_bits : natural := f_eca_log2(c_num_channels+1);
  
  function f_i(x : natural) return natural is 
  begin
    return x + g_channel_types'low;
  end function;
  
  function f_num(x : natural) return natural is
  begin
    if x >= g_channel_nums'length then
      return 1;
    else
      return g_channel_nums(x + g_channel_nums'low);
    end if;
  end function;
  
  type t_type_table is array(c_num_channels downto 0) of std_logic_vector(31 downto 0);
  function f_type_table return t_type_table is
    variable result : t_type_table;
  begin
    result(0) := (others => '0');
    for i in 1 to c_num_channels loop
      result(i) := std_logic_vector(to_unsigned(g_channel_types(i-1 + g_channel_types'low), 32));
    end loop;
    return result;
  end function;
  constant c_type_table : t_type_table := f_type_table;
  
  type t_num_table is array(c_num_channels downto 0) of std_logic_vector(7 downto 0);
  function f_num_table return t_num_table is
    variable result : t_num_table;
  begin
    result(0) := std_logic_vector(to_unsigned(g_num_ios, 8));
    for i in 1 to c_num_channels loop
      result(i) := std_logic_vector(to_unsigned(f_num(i-1), 8));
    end loop;
    return result;
  end function;
  constant c_num_table : t_num_table := f_num_table;

  signal s_slave_stall_i                       : std_logic_vector(1-1 downto 0);   -- 
  signal s_slave_flip_active_o                 : std_logic_vector(1-1 downto 0);   -- 
  signal s_slave_search_select_WR_o            : std_logic_vector(1-1 downto 0);   -- Write enable flag
  signal s_slave_search_select_o               : std_logic_vector(16-1 downto 0);  -- 
  signal s_slave_search_ro_first_i             : std_logic_vector(16-1 downto 0);  -- 
  signal s_slave_search_ro_event_hi_i          : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_search_ro_event_lo_i          : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_search_write_o                : std_logic_vector(1-1 downto 0);   -- 
  signal s_slave_search_rw_first_o             : std_logic_vector(16-1 downto 0);  -- 
  signal s_slave_search_rw_event_hi_o          : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_search_rw_event_lo_o          : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_walker_select_WR_o            : std_logic_vector(1-1 downto 0);   -- Write enable flag
  signal s_slave_walker_select_o               : std_logic_vector(16-1 downto 0);  -- 
  signal s_slave_walker_ro_next_i              : std_logic_vector(16-1 downto 0);  -- 
  signal s_slave_walker_ro_offset_hi_i         : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_walker_ro_offset_lo_i         : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_walker_ro_tag_i               : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_walker_ro_flags_i             : std_logic_vector(4-1 downto 0);   -- 
  signal s_slave_walker_ro_channel_i           : std_logic_vector(8-1 downto 0);   -- 
  signal s_slave_walker_ro_num_i               : std_logic_vector(8-1 downto 0);   -- 
  signal s_slave_walker_write_o                : std_logic_vector(1-1 downto 0);   -- 
  signal s_slave_walker_rw_next_o              : std_logic_vector(16-1 downto 0);  -- 
  signal s_slave_walker_rw_offset_hi_o         : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_walker_rw_offset_lo_o         : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_walker_rw_tag_o               : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_walker_rw_flags_o             : std_logic_vector(4-1 downto 0);   -- 
  signal s_slave_walker_rw_channel_o           : std_logic_vector(8-1 downto 0);   -- 
  signal s_slave_walker_rw_num_o               : std_logic_vector(8-1 downto 0);   -- 
  signal s_slave_channel_select_WR_o           : std_logic_vector(1-1 downto 0);   -- Write enable flag
  signal s_slave_channel_select_o              : std_logic_vector(8-1 downto 0);   -- 
  signal s_slave_channel_num_select_o          : std_logic_vector(8-1 downto 0);   -- 
  signal s_slave_channel_code_select_o         : std_logic_vector(2-1 downto 0);   -- 
  signal s_slave_channel_type_i                : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_channel_max_num_i             : std_logic_vector(8-1 downto 0);   -- 
  signal s_slave_channel_capacity_i            : std_logic_vector(16-1 downto 0);  -- 
  signal s_slave_channel_msi_set_enable_WR_o   : std_logic_vector(1-1 downto 0);   -- Write enable flag
  signal s_slave_channel_msi_set_enable_o      : std_logic_vector(1-1 downto 0);   -- 
  signal s_slave_channel_msi_get_enable_i      : std_logic_vector(1-1 downto 0);   -- 
  signal s_slave_channel_msi_set_target_WR_o   : std_logic_vector(1-1 downto 0);   -- Write enable flag
  signal s_slave_channel_msi_set_target_o      : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_channel_msi_get_target_i      : std_logic_vector(32-1 downto 0);  -- 
  signal s_slave_channel_overflow_count_RD_o   : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_mostfull_ack_RD_o     : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_mostfull_clear_RD_o   : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_valid_count_RD_o      : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_failed_count_RD_o     : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_event_id_hi_RD_o      : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_event_id_lo_RD_o      : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_param_hi_RD_o         : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_param_lo_RD_o         : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_tag_RD_o              : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_tef_RD_o              : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_deadline_hi_RD_o      : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_deadline_lo_RD_o      : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_executed_hi_RD_o      : std_logic_vector(1-1 downto 0);   -- Read enable flag
  signal s_slave_channel_executed_lo_RD_o      : std_logic_vector(1-1 downto 0);   -- Read enable flag
  
  signal sa_streams : t_stream_array(g_num_streams downto 0);
  signal sa_stalls  : std_logic_vector(g_num_streams downto 0);
  
  signal rc_page         : std_logic := '0';
  signal ra_page         : std_logic_vector(3 downto 0);
  signal r_bad_ack       : std_logic := '0';
  signal r_search_valid  : std_logic_vector(3 downto 0) := (others => '0');
  signal r_walker_valid  : std_logic_vector(3 downto 0) := (others => '0');
  signal r_channel_valid : std_logic_vector(3 downto 0) := (others => '0');
  
  signal s_sw_stb      : std_logic;
  signal s_ws_stall    : std_logic;
  signal s_sw_page     : std_logic;
  signal s_sw_first    : std_logic_vector(g_log_table_size-1 downto 0);
  signal s_sw_event    : t_event;
  signal s_sw_param    : t_param;
  signal s_sw_tef      : t_tef;
  signal s_sw_time     : t_time;
  signal s_s_rw_valid  : std_logic;
  signal s_s_ro_valid  : std_logic;
  signal s_w_rw_valid  : std_logic;
  signal s_w_ro_valid  : std_logic;
  signal s_wc_channels : t_channel_array(c_num_channels downto 0);
  
  type t_words is array(natural range <>) of std_logic_vector(31 downto 0);
  
  signal s_req_fields : std_logic_vector(15 downto 0);
  signal s_req_field  : std_logic_vector( 3 downto 0);
  signal s_req_stb    : std_logic;
  signal s_req_stbs   : std_logic_vector(c_num_channels downto 0);
  signal s_req_acks   : std_logic_vector(c_num_channels downto 0);
  signal s_req_ack    : std_logic;
  signal s_req_dats   : t_words(c_num_channels downto 0);
  signal s_req_dat    : std_logic_vector(31 downto 0);
  
  signal s_msi_acks   : std_logic_vector(c_num_channels downto 0);
  signal s_msi_stbs   : std_logic_vector(c_num_channels downto 0);
  signal s_msi_codes  : t_code_array(c_num_channels downto 0);
  signal s_msi_nums   : t_num_array(c_num_channels downto 0);
  
  signal ra_time       : t_time;
  signal ra_time_gray  : t_time;
  signal rc_time_gray0 : t_time;
  signal rc_time_gray1 : t_time;
  signal rc_time       : t_time;
  
begin

  INST_eca_auto : eca_auto
    generic map(
      g_channels        => c_num_channels+1,
      g_search_capacity => 2**(g_log_table_size+1),
      g_walker_capacity => 2**g_log_table_size,
      g_latency         => 2**g_log_latency,
      g_offset_bits     => g_log_max_delay)
    port map (
      clk_sys_i                     => c_clk_i,
      rst_sys_n_i                   => c_rst_n_i,
      stall_i                       => s_slave_stall_i,
      error_i                       => "0",
      flip_active_o                 => s_slave_flip_active_o,
      time_hi_V_i                   => "1",
      time_hi_i                     => rc_time(63 downto 32),
      time_lo_V_i                   => "1",
      time_lo_i                     => rc_time(31 downto 0),
      search_select_WR_o            => s_slave_search_select_WR_o,
      search_select_RD_o            => open,
      search_select_o               => s_slave_search_select_o,
      search_ro_first_V_i           => r_search_valid(0 downto 0),
      search_ro_first_i             => s_slave_search_ro_first_i,
      search_ro_event_hi_V_i        => r_search_valid(0 downto 0),
      search_ro_event_hi_i          => s_slave_search_ro_event_hi_i,
      search_ro_event_lo_V_i        => r_search_valid(0 downto 0),
      search_ro_event_lo_i          => s_slave_search_ro_event_lo_i,
      search_write_o                => s_slave_search_write_o,
      search_rw_first_o             => s_slave_search_rw_first_o,
      search_rw_event_hi_o          => s_slave_search_rw_event_hi_o,
      search_rw_event_lo_o          => s_slave_search_rw_event_lo_o,
      walker_select_WR_o            => s_slave_walker_select_WR_o,
      walker_select_RD_o            => open,
      walker_select_o               => s_slave_walker_select_o,
      walker_ro_next_V_i            => r_walker_valid(0 downto 0),  
      walker_ro_next_i              => s_slave_walker_ro_next_i,
      walker_ro_offset_hi_V_i       => r_walker_valid(0 downto 0),
      walker_ro_offset_hi_i         => s_slave_walker_ro_offset_hi_i,
      walker_ro_offset_lo_V_i       => r_walker_valid(0 downto 0),
      walker_ro_offset_lo_i         => s_slave_walker_ro_offset_lo_i,
      walker_ro_tag_V_i             => r_walker_valid(0 downto 0),
      walker_ro_tag_i               => s_slave_walker_ro_tag_i,
      walker_ro_flags_V_i           => r_walker_valid(0 downto 0),
      walker_ro_flags_i             => s_slave_walker_ro_flags_i,
      walker_ro_channel_V_i         => r_walker_valid(0 downto 0),
      walker_ro_channel_i           => s_slave_walker_ro_channel_i,
      walker_ro_num_V_i             => r_walker_valid(0 downto 0),
      walker_ro_num_i               => s_slave_walker_ro_num_i,
      walker_write_o                => s_slave_walker_write_o,
      walker_rw_next_o              => s_slave_walker_rw_next_o,
      walker_rw_offset_hi_o         => s_slave_walker_rw_offset_hi_o,
      walker_rw_offset_lo_o         => s_slave_walker_rw_offset_lo_o,
      walker_rw_tag_o               => s_slave_walker_rw_tag_o,
      walker_rw_flags_o             => s_slave_walker_rw_flags_o,
      walker_rw_channel_o           => s_slave_walker_rw_channel_o,
      walker_rw_num_o               => s_slave_walker_rw_num_o,
      channel_select_WR_o           => s_slave_channel_select_WR_o,
      channel_select_RD_o           => open,
      channel_select_o              => s_slave_channel_select_o,
      channel_num_select_o          => s_slave_channel_num_select_o,
      channel_code_select_o         => s_slave_channel_code_select_o,
      channel_type_V_i              => r_channel_valid(0 downto 0),
      channel_type_i                => s_slave_channel_type_i,
      channel_max_num_V_i           => r_channel_valid(0 downto 0),
      channel_max_num_i             => s_slave_channel_max_num_i,
      channel_capacity_V_i          => r_channel_valid(0 downto 0),
      channel_capacity_i            => s_slave_channel_capacity_i,
      channel_msi_set_enable_WR_o   => s_slave_channel_msi_set_enable_WR_o,
      channel_msi_set_enable_o      => s_slave_channel_msi_set_enable_o,
      channel_msi_get_enable_V_i    => r_channel_valid(0 downto 0),
      channel_msi_get_enable_i      => s_slave_channel_msi_get_enable_i,
      channel_msi_set_target_WR_o   => s_slave_channel_msi_set_target_WR_o,
      channel_msi_set_target_o      => s_slave_channel_msi_set_target_o,
      channel_msi_get_target_V_i    => r_channel_valid(0 downto 0),
      channel_msi_get_target_i      => s_slave_channel_msi_get_target_i,
      channel_overflow_count_RD_o   => s_slave_channel_overflow_count_RD_o,
      channel_overflow_count_V_i(0) => s_req_ack,
      channel_overflow_count_i      => s_req_dat,
      channel_mostfull_ack_RD_o     => s_slave_channel_mostfull_ack_RD_o,
      channel_mostfull_ack_V_i(0)   => s_req_ack,
      channel_mostfull_ack_i        => s_req_dat,
      channel_mostfull_clear_RD_o   => s_slave_channel_mostfull_clear_RD_o,
      channel_mostfull_clear_V_i(0) => s_req_ack,
      channel_mostfull_clear_i      => s_req_dat,
      channel_valid_count_RD_o      => s_slave_channel_valid_count_RD_o,
      channel_valid_count_V_i(0)    => s_req_ack,
      channel_valid_count_i         => s_req_dat,
      channel_failed_count_RD_o     => s_slave_channel_failed_count_RD_o,
      channel_failed_count_V_i(0)   => s_req_ack,
      channel_failed_count_i        => s_req_dat,
      channel_event_id_hi_RD_o      => s_slave_channel_event_id_hi_RD_o,
      channel_event_id_hi_V_i(0)    => s_req_ack,
      channel_event_id_hi_i         => s_req_dat,
      channel_event_id_lo_RD_o      => s_slave_channel_event_id_lo_RD_o,
      channel_event_id_lo_V_i(0)    => s_req_ack,
      channel_event_id_lo_i         => s_req_dat,
      channel_param_hi_RD_o         => s_slave_channel_param_hi_RD_o,
      channel_param_hi_V_i(0)       => s_req_ack,
      channel_param_hi_i            => s_req_dat,
      channel_param_lo_RD_o         => s_slave_channel_param_lo_RD_o,
      channel_param_lo_V_i(0)       => s_req_ack,
      channel_param_lo_i            => s_req_dat,
      channel_tag_RD_o              => s_slave_channel_tag_RD_o,
      channel_tag_V_i(0)            => s_req_ack,
      channel_tag_i                 => s_req_dat,
      channel_tef_RD_o              => s_slave_channel_tef_RD_o,
      channel_tef_V_i(0)            => s_req_ack,
      channel_tef_i                 => s_req_dat,
      channel_deadline_hi_RD_o      => s_slave_channel_deadline_hi_RD_o,
      channel_deadline_hi_V_i(0)    => s_req_ack,
      channel_deadline_hi_i         => s_req_dat,
      channel_deadline_lo_RD_o      => s_slave_channel_deadline_lo_RD_o,
      channel_deadline_lo_V_i(0)    => s_req_ack,
      channel_deadline_lo_i         => s_req_dat,
      channel_executed_hi_RD_o      => s_slave_channel_executed_hi_RD_o,
      channel_executed_hi_V_i(0)    => s_req_ack,
      channel_executed_hi_i         => s_req_dat,
      channel_executed_lo_RD_o      => s_slave_channel_executed_lo_RD_o,
      channel_executed_lo_V_i(0)    => s_req_ack,
      channel_executed_lo_i         => s_req_dat,
      slave_i                       => c_slave_i,
      slave_o                       => c_slave_o);
  
  -- Simple fields
  s_slave_channel_capacity_i <= std_logic_vector(to_unsigned(2**g_log_queue_size, 16));
  
  s_slave_channel_type_i <=
    c_type_table(to_integer(unsigned(s_slave_channel_select_o)))
    when f_eca_safe(s_slave_channel_select_o) = '1' else
    (others => 'X');
    
  s_slave_channel_max_num_i <=
    c_num_table(to_integer(unsigned(s_slave_channel_select_o)))
    when f_eca_safe(s_slave_channel_select_o) = '1' else
    (others => 'X');
  
  -- Arbitrate inputs, priority access goes to #0
  sa_streams(g_num_streams).stb   <= '0';
  sa_streams(g_num_streams).event <= (others => '-');
  sa_streams(g_num_streams).param <= (others => '-');
  sa_streams(g_num_streams).tef   <= (others => '-');
  sa_streams(g_num_streams).time  <= (others => '-');
  Sx : for s in 0 to g_num_streams-1 generate
    sa_stalls(s+1)      <= a_stream_i(s).stb   or                              sa_stalls(s);
    sa_streams(s).stb   <= a_stream_i(s).stb   or                              sa_streams(s+1).stb;
    sa_streams(s).event <= a_stream_i(s).event when a_stream_i(s).stb='1' else sa_streams(s+1).event;
    sa_streams(s).param <= a_stream_i(s).param when a_stream_i(s).stb='1' else sa_streams(s+1).param;
    sa_streams(s).tef   <= a_stream_i(s).tef   when a_stream_i(s).stb='1' else sa_streams(s+1).tef;
    sa_streams(s).time  <= a_stream_i(s).time  when a_stream_i(s).stb='1' else sa_streams(s+1).time;
  end generate;
  a_stall_o <= sa_stalls(a_stall_o'range);
  
  search : eca_search
    generic map(
      g_log_table_size => g_log_table_size)
    port map(
      clk_i      => a_clk_i,
      rst_n_i    => a_rst_n_i,
      e_stb_i    => sa_streams(0).stb,
      e_stall_o  => sa_stalls(0),
      e_page_i   => "not"(ra_page(0)),
      e_event_i  => sa_streams(0).event,
      e_param_i  => sa_streams(0).param,
      e_tef_i    => sa_streams(0).tef,
      e_time_i   => sa_streams(0).time,
      w_stb_o    => s_sw_stb,
      w_stall_i  => s_ws_stall,
      w_page_o   => s_sw_page,
      w_first_o  => s_sw_first,
      w1_event_o => s_sw_event,
      w1_param_o => s_sw_param,
      w1_tef_o   => s_sw_tef,
      w1_time_o  => s_sw_time,
      t_clk_i    => c_clk_i,
      t_page_i   => rc_page,
      t_addr_i   => s_slave_search_select_o(g_log_table_size downto 0),
      tw_en_i    => s_slave_search_write_o(0),
      tw_valid_i => s_s_rw_valid,
      tw_first_i => s_slave_search_rw_first_o(g_log_table_size-1 downto 0),
      tw_event_i(63 downto 32) => s_slave_search_rw_event_hi_o,
      tw_event_i(31 downto  0) => s_slave_search_rw_event_lo_o,
      tr_valid_o => s_s_ro_valid,
      tr_first_o => s_slave_search_ro_first_i(g_log_table_size-1 downto 0),
      tr_event_o(63 downto 32) => s_slave_search_ro_event_hi_i,
      tr_event_o(31 downto  0) => s_slave_search_ro_event_lo_i);
  
  s_s_rw_valid <= not s_slave_search_rw_first_o(s_slave_search_rw_first_o'high);
  s_slave_search_ro_first_i(s_slave_search_ro_first_i'high downto g_log_table_size) <= (others => not s_s_ro_valid);
  
  walker : eca_walker
    generic map(
      g_log_table_size => g_log_table_size,
      g_num_channels   => c_num_channels+1)
    port map(
      clk_i         => a_clk_i,
      rst_n_i       => a_rst_n_i,
      b_stb_i       => s_sw_stb,
      b_stall_o     => s_ws_stall,
      b_page_i      => s_sw_page,
      b_first_i     => s_sw_first,
      b1_event_i    => s_sw_event,
      b1_param_i    => s_sw_param,
      b1_tef_i      => s_sw_tef,
      b1_time_i     => s_sw_time,
      q_channel_o   => s_wc_channels,
      t_clk_i       => c_clk_i,
      t_page_i      => rc_page,
      t_addr_i      => s_slave_walker_select_o(g_log_table_size-1 downto 0),
      tw_en_i       => s_slave_walker_write_o(0),
      tw_valid_i    => s_w_rw_valid,
      tw_delayed_i  => s_slave_walker_rw_flags_o(3),
      tw_conflict_i => s_slave_walker_rw_flags_o(2),
      tw_late_i     => s_slave_walker_rw_flags_o(0),
      tw_early_i    => s_slave_walker_rw_flags_o(1),
      tw_next_i     => s_slave_walker_rw_next_o(g_log_table_size-1 downto 0),
      tw_time_i(63 downto 32) => s_slave_walker_rw_offset_hi_o,
      tw_time_i(31 downto  0) => s_slave_walker_rw_offset_lo_o,
      tw_tag_i      => s_slave_walker_rw_tag_o,
      tw_num_i      => s_slave_walker_rw_num_o,
      tw_channel_i  => s_slave_walker_rw_channel_o(c_channel_bits-1 downto 0),
      tr_valid_o    => s_w_ro_valid,
      tr_delayed_o  => s_slave_walker_ro_flags_i(3),
      tr_conflict_o => s_slave_walker_ro_flags_i(2),
      tr_late_o     => s_slave_walker_ro_flags_i(0),
      tr_early_o    => s_slave_walker_ro_flags_i(1),
      tr_next_o     => s_slave_walker_ro_next_i(g_log_table_size-1 downto 0),
      tr_time_o(63 downto 32) => s_slave_walker_ro_offset_hi_i,
      tr_time_o(31 downto  0) => s_slave_walker_ro_offset_lo_i,
      tr_tag_o      => s_slave_walker_ro_tag_i,
      tr_num_o      => s_slave_walker_ro_num_i,
      tr_channel_o  => s_slave_walker_ro_channel_i(c_channel_bits-1 downto 0));

  s_w_rw_valid <= not s_slave_walker_rw_next_o(s_slave_walker_rw_next_o'high);
  s_slave_walker_ro_next_i(s_slave_walker_ro_next_i'high downto g_log_table_size) <= (others => not s_w_ro_valid);
  s_slave_walker_ro_channel_i(s_slave_walker_ro_channel_i'high downto c_channel_bits) <= (others => '0');
  
  -- Mapping corresponds to eca_channel.vhd description
  s_req_fields( 0) <= s_slave_channel_event_id_hi_RD_o(0);
  s_req_fields( 1) <= s_slave_channel_event_id_lo_RD_o(0);
  s_req_fields( 2) <= s_slave_channel_param_hi_RD_o(0);
  s_req_fields( 3) <= s_slave_channel_param_lo_RD_o(0);
  s_req_fields( 4) <= s_slave_channel_tag_RD_o(0);
  s_req_fields( 5) <= s_slave_channel_tef_RD_o(0);
  s_req_fields( 6) <= s_slave_channel_deadline_hi_RD_o(0);
  s_req_fields( 7) <= s_slave_channel_deadline_lo_RD_o(0);
  s_req_fields( 8) <= s_slave_channel_executed_hi_RD_o(0);
  s_req_fields( 9) <= s_slave_channel_executed_lo_RD_o(0);
  s_req_fields(10) <= '0'; -- reserved
  s_req_fields(11) <= s_slave_channel_failed_count_RD_o(0);
  s_req_fields(12) <= s_slave_channel_valid_count_RD_o(0);
  s_req_fields(13) <= s_slave_channel_overflow_count_RD_o(0);
  s_req_fields(14) <= s_slave_channel_mostfull_ack_RD_o(0);
  s_req_fields(15) <= s_slave_channel_mostfull_clear_RD_o(0);
  
  -- Did a channel request arrive channel?
  s_req_field  <= f_eca_1hot_decode(s_req_fields);
  s_req_stb    <= f_eca_or(s_req_fields);
  
  -- Combine results
  s_req_ack <= f_eca_or(s_req_acks) or r_bad_ack;
  s_req_dat <= s_req_dats(to_integer(unsigned(s_slave_channel_select_o))) when s_req_stb='1' else (others => 'X');
  
  -- Select correct channel
  chan_select : for i in 0 to c_num_channels generate
    s_req_stbs(i) <= f_eca_active_high(unsigned(s_slave_channel_select_o) = i) when s_req_stb='1' else '0';
  end generate;
  
  io_channel : eca_channel
    generic map(
      g_support_io     => true,
      g_never_delayed  => true,
      g_num_channels   => g_num_ios,
      g_log_size       => g_log_queue_size,
      g_log_multiplier => g_log_multiplier,
      g_log_max_delay  => g_log_max_delay,
      g_log_latency    => g_log_latency,
      g_log_counter    => g_log_counter)
    port map(
      clk_i       => a_clk_i,
      rst_n_i     => a_rst_n_i,
      time_i      => a_time_i,
      overflow_o  => open,
      channel_i   => s_wc_channels(0),
      clr_i       => s_wc_channels(0).tag(0),
      set_i       => s_wc_channels(0).tag(1),
      stall_i     => '0',
      channel_o   => open,
      io_o        => a_io_o,
      req_clk_i   => c_clk_i,
      req_rst_n_i => c_rst_n_i,
      req_stb_i   => s_req_stbs(0),
      req_num_i   => s_slave_channel_num_select_o,
      req_type_i  => s_slave_channel_code_select_o,
      req_field_i => s_req_field,
      req_valid_o => s_req_acks(0),
      req_data_o  => s_req_dats(0),
      msi_clk_i   => i_clk_i,
      msi_rst_n_i => i_rst_n_i,
      msi_ack_i   => s_msi_acks(0),
      msi_stb_o   => s_msi_stbs(0),
      msi_code_o  => s_msi_codes(0),
      msi_num_o   => s_msi_nums(0));
  
  channels : for i in 1 to c_num_channels generate
    tag_channel : eca_channel
      generic map(
        g_support_io     => false,
        g_never_delayed  => false,
        g_num_channels   => f_num(i-1),
        g_log_size       => g_log_queue_size,
        g_log_multiplier => g_log_multiplier,
        g_log_max_delay  => g_log_max_delay,
        g_log_latency    => g_log_latency,
        g_log_counter    => g_log_counter)
      port map(
        clk_i       => a_clk_i,
        rst_n_i     => a_rst_n_i,
        time_i      => a_time_i,
        overflow_o  => open,
        channel_i   => s_wc_channels(i),
        clr_i       => '0',
        set_i       => '0',
        stall_i     => a_stall_i  (f_i(i-1)),
        channel_o   => a_channel_o(f_i(i-1)),
        io_o        => open,
        req_clk_i   => c_clk_i,
        req_rst_n_i => c_rst_n_i,
        req_stb_i   => s_req_stbs(i),
        req_num_i   => s_slave_channel_num_select_o,
        req_type_i  => s_slave_channel_code_select_o,
        req_field_i => s_req_field,
        req_valid_o => s_req_acks(i),
        req_data_o  => s_req_dats(i),
        msi_clk_i   => i_clk_i,
        msi_rst_n_i => i_rst_n_i,
        msi_ack_i   => s_msi_acks(i),
        msi_stb_o   => s_msi_stbs(i),
        msi_code_o  => s_msi_codes(i),
        msi_num_o   => s_msi_nums(i));
  end generate;
  
  msi : eca_msi
    generic map(
      g_num_channels => c_num_channels+1)
    port map(
      c_clk_i        => c_clk_i,
      c_rst_n_i      => c_rst_n_i,
      c_chan_i       => s_slave_channel_select_o,
      c_enable_stb_i => s_slave_channel_msi_set_enable_WR_o(0),
      c_enable_i     => s_slave_channel_msi_set_enable_o(0),
      c_enable_o     => s_slave_channel_msi_get_enable_i(0),
      c_target_stb_i => s_slave_channel_msi_set_target_WR_o(0),
      c_target_i     => s_slave_channel_msi_set_target_o,
      c_target_o     => s_slave_channel_msi_get_target_i,
      c_stall_o      => s_slave_stall_i(0),
      i_clk_i        => i_clk_i,
      i_rst_n_i      => i_rst_n_i,
      i_ack_o        => s_msi_acks,
      i_stb_i        => s_msi_stbs,
      i_code_i       => s_msi_codes,
      i_num_i        => s_msi_nums,
      i_master_i     => i_master_i,
      i_master_o     => i_master_o);
  
  main : process(c_clk_i, c_rst_n_i) is
  begin
    if c_rst_n_i = '0' then
      rc_page   <= '0';
      r_bad_ack <= '0';
      r_search_valid  <= (others => '0');
      r_walker_valid  <= (others => '0');
      r_channel_valid <= (others => '0');
    elsif rising_edge(c_clk_i) then
      rc_page   <= rc_page xor s_slave_flip_active_o(0);
      r_bad_ack <= s_req_stb and f_eca_active_high(unsigned(s_slave_channel_select_o) > c_num_channels);
      
      -- Delay visibility of search_ro_* fields
      if (s_slave_flip_active_o or s_slave_search_write_o or s_slave_search_select_WR_o) = "1" then
        r_search_valid <= (others => '0');
      else
        r_search_valid <= '1' & r_search_valid(r_search_valid'high downto 1);
      end if;
    
      -- Delay visibility of walker_ro_* fields
      if (s_slave_flip_active_o or s_slave_walker_write_o or s_slave_walker_select_WR_o) = "1" then
        r_walker_valid <= (others => '0');
      else
        r_walker_valid <= '1' & r_walker_valid(r_walker_valid'high downto 1);
      end if;
      
      -- Delay visibility of channel status fields
      if s_slave_channel_select_WR_o = "1" then
        r_channel_valid <= (others => '0');
      else
        r_channel_valid <= '1' & r_channel_valid(r_channel_valid'high downto 1);
      end if;
    end if;
  end process;
  
  bulk : process(c_clk_i) is
  begin
    if rising_edge(c_clk_i) then
      rc_time_gray0 <= ra_time_gray;
      rc_time_gray1 <= rc_time_gray0;
      rc_time       <= f_eca_gray_decode(rc_time_gray1, 1);
    end if;
  end process;
  
  time : process(a_clk_i) is
  begin
    if rising_edge(a_clk_i) then
      ra_time      <= a_time_i;
      ra_time_gray <= f_eca_gray_encode(ra_time);
      ra_page      <= rc_page & ra_page(ra_page'high downto 1);
    end if;
  end process;

end rtl;
