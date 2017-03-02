-- see xrx_streamer.vhd for port documentation

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;
use work.streamers_pkg.all;

entity rx_streamer is
  
  generic (
    g_data_width            : integer := 32;
    g_buffer_size           : integer := 128;
    g_escape_code_disable   : boolean := FALSE;
    g_expected_words_number : integer := 0
    );

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    -- Endpoint/WRC interface 
    snk_dat_i   : in  std_logic_vector(15 downto 0);
    snk_adr_i   : in  std_logic_vector(1 downto 0);
    snk_sel_i   : in  std_logic_vector(1 downto 0);
    snk_cyc_i   : in  std_logic;
    snk_stb_i   : in  std_logic;
    snk_we_i    : in  std_logic;
    snk_stall_o : out std_logic;
    snk_ack_o   : out std_logic;
    snk_err_o   : out std_logic;
    snk_rty_o   : out std_logic;

    clk_ref_i       : in std_logic                     := '0';
    tm_time_valid_i : in std_logic                     := '0';
    tm_tai_i        : in std_logic_vector(39 downto 0) := x"0000000000";
    tm_cycles_i     : in std_logic_vector(27 downto 0) := x"0000000";

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

    cfg_mac_local_i         : in std_logic_vector(47 downto 0);
    cfg_mac_remote_i        : in std_logic_vector(47 downto 0);
    cfg_ethertype_i         : in std_logic_vector(15 downto 0) := x"dbff";
    cfg_accept_broadcasts_i : in std_logic                     := '1';
    cfg_filter_remote_i     : in std_logic                     := '0';
    cfg_fixed_latency_i     : in std_logic_vector(27 downto 0) := x"0000000"
    );

end rx_streamer;

architecture wrapper of rx_streamer is


  signal snk_in  : t_wrf_sink_in;
  signal snk_out : t_wrf_sink_out;

begin  -- rtl

  U_Wrapped_Streamer : xrx_streamer
    generic map (
      g_data_width            => g_data_width,
      g_buffer_size           => g_buffer_size,
      g_escape_code_disable   => g_escape_code_disable,
      g_expected_words_number => g_expected_words_number)
    port map (
      clk_sys_i               => clk_sys_i,
      clk_ref_i               => clk_ref_i,
      tm_tai_i                => tm_tai_i,
      tm_time_valid_i         => tm_time_valid_i,
      tm_cycles_i             => tm_cycles_i,
      rst_n_i                 => rst_n_i,
      snk_i                   => snk_in,
      snk_o                   => snk_out,
      rx_first_p1_o           => rx_first_p1_o,
      rx_last_p1_o            => rx_last_p1_o,
      rx_data_o               => rx_data_o,
      rx_valid_o              => rx_valid_o,
      rx_dreq_i               => rx_dreq_i,
      rx_lost_p1_o            => rx_lost_p1_o,
      rx_lost_blocks_p1_o     => rx_lost_blocks_p1_o,
      rx_lost_frames_p1_o     => rx_lost_frames_p1_o,
      rx_lost_frames_cnt_o    => rx_lost_frames_cnt_o,
      rx_latency_valid_o      => rx_latency_valid_o,
      rx_latency_o            => rx_latency_o,
      rx_frame_p1_o           => rx_frame_p1_o,
      cfg_mac_local_i         => cfg_mac_local_i,
      cfg_mac_remote_i        => cfg_mac_remote_i,
      cfg_ethertype_i         => cfg_ethertype_i,
      cfg_accept_broadcasts_i => cfg_accept_broadcasts_i,
      cfg_filter_remote_i     => cfg_filter_remote_i,
      cfg_fixed_latency_i     => cfg_fixed_latency_i);

  snk_in.dat  <= snk_dat_i;
  snk_in.adr  <= snk_adr_i;
  snk_in.sel  <= snk_sel_i;
  snk_in.cyc  <= snk_cyc_i;
  snk_in.stb  <= snk_stb_i;
  snk_in.we   <= snk_we_i;
  snk_stall_o <= snk_out.stall;
  snk_ack_o   <= snk_out.ack;
  snk_err_o   <= snk_out.err;
  snk_rty_o   <= snk_out.rty;

end wrapper;
