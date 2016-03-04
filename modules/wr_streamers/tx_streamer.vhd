-- see xtx_streamer.vhd for comments

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;
use work.streamers_pkg.all;

entity tx_streamer is
  
  generic (
    g_data_width             : integer := 32;
    g_tx_threshold           : integer := 16;
    g_tx_max_words_per_frame : integer := 128;
    g_tx_timeout             : integer := 1024
    );

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    -- Endpoint/WRC interface 
    src_dat_o   : out std_logic_vector(15 downto 0);
    src_adr_o   : out std_logic_vector(1 downto 0);
    src_sel_o   : out std_logic_vector(1 downto 0);
    src_cyc_o   : out std_logic;
    src_stb_o   : out std_logic;
    src_we_o    : out std_logic;
    src_stall_i : in  std_logic;
    src_ack_i   : in  std_logic;
    src_err_i   : in  std_logic;

    clk_ref_i       : in std_logic                     := '0';
    tm_time_valid_i : in std_logic                     := '0';
    tm_tai_i        : in std_logic_vector(39 downto 0) := x"0000000000";
    tm_cycles_i     : in std_logic_vector(27 downto 0) := x"0000000";

    tx_flush_i     : in  std_logic := '0';
    tx_last_i      : in  std_logic := '1';
    tx_data_i      : in  std_logic_vector(g_data_width-1 downto 0);
    tx_reset_seq_i : in  std_logic := '0';
    tx_valid_i     : in  std_logic;
    tx_dreq_o      : out std_logic;

    -- MAC address
    cfg_mac_local_i  : in std_logic_vector(47 downto 0) := x"000000000000";
    cfg_mac_target_i : in std_logic_vector(47 downto 0) := x"ffffffffffff";
    cfg_ethertype_i  : in std_logic_vector(15 downto 0) := x"dbff"
    );

end tx_streamer;

architecture rtl of tx_streamer is

  signal src_in  : t_wrf_source_in;
  signal src_out : t_wrf_source_out;
  
begin  -- rtl
  
  U_Wrapped_Streamer : xtx_streamer
    generic map (
      g_data_width             => g_data_width,
      g_tx_threshold           => g_tx_threshold,
      g_tx_max_words_per_frame => g_tx_max_words_per_frame,
      g_tx_timeout             => g_tx_timeout)
    port map (
      clk_sys_i        => clk_sys_i,
      rst_n_i          => rst_n_i,
      src_i            => src_in,
      src_o            => src_out,
      tx_last_i        => tx_last_i,
      tx_data_i        => tx_data_i,
      tx_reset_seq_i   => tx_reset_seq_i,
      tx_valid_i       => tx_valid_i,
      tx_dreq_o        => tx_dreq_o,
      tx_flush_i       => tx_flush_i,
      clk_ref_i        => clk_ref_i,
      tm_time_valid_i  => tm_time_valid_i,
      tm_tai_i         => tm_tai_i,
      tm_cycles_i      => tm_cycles_i,
      cfg_mac_local_i  => cfg_mac_local_i,
      cfg_mac_target_i => cfg_mac_target_i,
      cfg_ethertype_i  => cfg_ethertype_i);

  src_adr_o    <= src_out.adr;
  src_dat_o    <= src_out.dat;
  src_sel_o    <= src_out.sel;
  src_stb_o    <= src_out.stb;
  src_we_o     <= src_out.we;
  src_cyc_o    <= src_out.cyc;
  src_in.ack   <= src_ack_i;
  src_in.stall <= src_stall_i;
  src_in.err   <= src_err_i;
  
end rtl;
