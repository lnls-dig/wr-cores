library ieee;
use ieee.std_logic_1164.all;
use work.wr_fabric_pkg.all;
use work.wrcore_pkg.all;
use work.wishbone_pkg.all;  -- needed for t_wishbone_slave_in, etc

package streamers_pkg is

  component xtx_streamer
    generic (
      g_data_width             : integer := 32;
      g_tx_threshold           : integer := 16;
      g_tx_max_words_per_frame : integer := 128;
      g_tx_timeout             : integer := 128;
      g_escape_code_disable    : boolean := FALSE);
    port (
      clk_sys_i        : in  std_logic;
      rst_n_i          : in  std_logic;
      src_i            : in  t_wrf_source_in;
      src_o            : out t_wrf_source_out;
      clk_ref_i        : in  std_logic                     := '0';
      tm_time_valid_i  : in  std_logic                     := '0';
      tm_tai_i         : in  std_logic_vector(39 downto 0) := x"0000000000";
      tm_cycles_i      : in  std_logic_vector(27 downto 0) := x"0000000";
      tx_data_i        : in  std_logic_vector(g_data_width-1 downto 0);
      tx_valid_i       : in  std_logic;
      tx_dreq_o        : out std_logic;
      tx_last_p1_i     : in  std_logic                     := '1';
      tx_flush_p1_i    : in  std_logic                     := '0';
      tx_reset_seq_i   : in  std_logic                     := '0';
      tx_frame_p1_o    : out std_logic;
      cfg_mac_local_i  : in  std_logic_vector(47 downto 0) := x"000000000000";
      cfg_mac_target_i : in  std_logic_vector(47 downto 0);
      cfg_ethertype_i  : in  std_logic_vector(15 downto 0) := x"dbff");
  end component;
  
  component xrx_streamer
    generic (
      g_data_width        : integer := 32;
      g_buffer_size       : integer := 16;
      g_filter_remote_mac : boolean := false;
      g_escape_code_disable : boolean := FALSE;
      g_expected_words_number : integer := 0);
    port (
      clk_sys_i               : in  std_logic;
      rst_n_i                 : in  std_logic;
      snk_i                   : in  t_wrf_sink_in;
      snk_o                   : out t_wrf_sink_out;
      clk_ref_i               : in  std_logic                     := '0';
      tm_time_valid_i         : in  std_logic                     := '0';
      tm_tai_i                : in  std_logic_vector(39 downto 0) := x"0000000000";
      tm_cycles_i             : in  std_logic_vector(27 downto 0) := x"0000000";
      rx_first_p1_o           : out std_logic;
      rx_last_p1_o            : out std_logic;
      rx_data_o               : out std_logic_vector(g_data_width-1 downto 0);
      rx_valid_o              : out std_logic;
      rx_dreq_i               : in  std_logic;
      rx_lost_p1_o            : out std_logic := '0';
      rx_lost_blocks_p1_o     : out std_logic := '0';
      rx_lost_frames_p1_o     : out std_logic := '0';
      rx_lost_frames_cnt_o    : out std_logic_vector(14 downto 0);
      rx_latency_o            : out std_logic_vector(27 downto 0);
      rx_latency_valid_o      : out std_logic;
      rx_frame_p1_o           : out std_logic;
      cfg_mac_local_i         : in  std_logic_vector(47 downto 0) := x"000000000000";
      cfg_mac_remote_i        : in  std_logic_vector(47 downto 0) := x"000000000000";
      cfg_ethertype_i         : in  std_logic_vector(15 downto 0) := x"dbff";
      cfg_accept_broadcasts_i : in  std_logic                     := '1');
  end component;

  constant c_STREAMERS_ARR_SIZE_OUT : integer := 14;
  constant c_STREAMERS_ARR_SIZE_IN  : integer := 1;

  component xrtx_streamers_stats is
    generic (
      g_cnt_width            : integer := 32;
      g_acc_width            : integer := 64
      );
    port (
      clk_i                  : in std_logic;
      rst_n_i                : in std_logic;
      sent_frame_i           : in std_logic;
      rcvd_frame_i           : in std_logic;
      lost_block_i           : in std_logic;
      lost_frame_i           : in std_logic;
      lost_frames_cnt_i      : in std_logic_vector(14 downto 0);
      rcvd_latency_i         : in  std_logic_vector(27 downto 0);
      rcvd_latency_valid_i   : in  std_logic;
      clk_ref_i              : in std_logic;
      tm_time_valid_i        : in std_logic := '0';
      tm_tai_i               : in std_logic_vector(39 downto 0) := x"0000000000";
      tm_cycles_i            : in std_logic_vector(27 downto 0) := x"0000000";
      reset_stats_i          : in std_logic;
      snapshot_ena_i         : in std_logic := '0';
      reset_time_tai_o       : out std_logic_vector(39 downto 0) := x"0000000000";
      reset_time_cycles_o    : out std_logic_vector(27 downto 0) := x"0000000";
      sent_frame_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
      rcvd_frame_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
      lost_frame_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
      lost_block_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
      latency_cnt_o          : out std_logic_vector(g_cnt_width-1 downto 0);
      latency_acc_overflow_o : out std_logic;
      latency_acc_o          : out std_logic_vector(g_acc_width-1  downto 0);
      latency_max_o          : out std_logic_vector(27  downto 0);
      latency_min_o          : out std_logic_vector(27  downto 0);
      snmp_array_o           : out t_generic_word_array(c_STREAMERS_ARR_SIZE_OUT-1 downto 0);
      snmp_array_i           : in  t_generic_word_array(c_STREAMERS_ARR_SIZE_IN -1 downto 0) := (others => (others=>'0'))
      );
  end component;

  constant c_WR_TRANS_ARR_SIZE_OUT : integer := c_STREAMERS_ARR_SIZE_OUT+3;
  constant c_WR_TRANS_ARR_SIZE_IN  : integer := c_STREAMERS_ARR_SIZE_IN;

  component xwr_transmission is
    generic (
      g_tx_data_width            : integer := 32;
      g_rx_data_width            : integer := 32
      );
    port (
      clk_sys_i : in std_logic;
      rst_n_i   : in std_logic;
      src_i : in  t_wrf_source_in;
      src_o : out t_wrf_source_out;
      snk_i : in  t_wrf_sink_in;
      snk_o : out t_wrf_sink_out;
      tx_data_i : in std_logic_vector(g_tx_data_width-1 downto 0);
      tx_valid_i : in std_logic;
      tx_dreq_o : out std_logic;
      tx_last_p1_i : in std_logic := '1';
      tx_flush_p1_i : in std_logic := '0';
      rx_first_p1_o         : out std_logic;
      rx_last_p1_o          : out std_logic;
      rx_data_o          : out std_logic_vector(g_rx_data_width-1 downto 0);
      rx_valid_o         : out std_logic;
      rx_dreq_i          : in  std_logic;
      clk_ref_i : in std_logic := '0';
      tm_time_valid_i : in std_logic := '0';
      tm_tai_i : in std_logic_vector(39 downto 0) := x"0000000000";
      tm_cycles_i : in std_logic_vector(27 downto 0) := x"0000000";
      wb_slave_i               : in  t_wishbone_slave_in := cc_dummy_slave_in;
      wb_slave_o               : out t_wishbone_slave_out;
      snmp_array_o           : out t_generic_word_array(c_WR_TRANS_ARR_SIZE_OUT-1 downto 0);
      snmp_array_i           : in  t_generic_word_array(c_WR_TRANS_ARR_SIZE_IN -1 downto 0)
      );
  end component;

end streamers_pkg;