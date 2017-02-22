library ieee;
use ieee.std_logic_1164.all;

library work;
use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;
use work.endpoint_pkg.all;
use work.wrcore_pkg.all;

package wr_board_pkg is

  type t_board_fabric_iface is (PLAIN, STREAMERS, ETHERBONE, always_last_invalid);

  procedure f_check_fabric_iface_type (
    constant iface_type : in t_board_fabric_iface);

  function f_str2iface_type (
    constant iface_str : string(1 to 9))
    return t_board_fabric_iface;

  component xwrc_board_common is
    generic (
      g_simulation                : integer;
      g_with_external_clock_input : boolean;
      g_phys_uart                 : boolean;
      g_virtual_uart              : boolean;
      g_aux_clks                  : integer;
      g_ep_rxbuf_size             : integer;
      g_tx_runt_padding           : boolean;
      g_dpram_initf               : string;
      g_dpram_size                : integer;
      g_interface_mode            : t_wishbone_interface_mode;
      g_address_granularity       : t_wishbone_address_granularity;
      g_aux_sdb                   : t_sdb_device;
      g_softpll_enable_debugger   : boolean;
      g_vuart_fifo_size           : integer;
      g_pcs_16bit                 : boolean;
      g_diag_id                   : integer;
      g_diag_ver                  : integer;
      g_diag_ro_size              : integer;
      g_diag_rw_size              : integer;
      g_streamer_width            : integer;
      g_fabric_iface              : t_board_fabric_iface);
    port (
      clk_sys_i            : in  std_logic;
      clk_dmtd_i           : in  std_logic;
      clk_ref_i            : in  std_logic;
      clk_aux_i            : in  std_logic_vector(g_aux_clks-1 downto 0)       := (others => '0');
      clk_ext_i            : in  std_logic                                     := '0';
      clk_ext_mul_i        : in  std_logic                                     := '0';
      clk_ext_mul_locked_i : in  std_logic                                     := '1';
      clk_ext_stopped_i    : in  std_logic                                     := '0';
      clk_ext_rst_o        : out std_logic;
      pps_ext_i            : in  std_logic                                     := '0';
      rst_n_i              : in  std_logic;
      dac_hpll_load_p1_o   : out std_logic;
      dac_hpll_data_o      : out std_logic_vector(15 downto 0);
      dac_dpll_load_p1_o   : out std_logic;
      dac_dpll_data_o      : out std_logic_vector(15 downto 0);
      phy8_o               : out t_phy_8bits_from_wrc;
      phy8_i               : in  t_phy_8bits_to_wrc                            := c_dummy_phy8_to_wrc;
      phy16_o              : out t_phy_16bits_from_wrc;
      phy16_i              : in  t_phy_16bits_to_wrc                           := c_dummy_phy16_to_wrc;
      led_act_o            : out std_logic;
      led_link_o           : out std_logic;
      scl_o                : out std_logic;
      scl_i                : in  std_logic                                     := '1';
      sda_o                : out std_logic;
      sda_i                : in  std_logic                                     := '1';
      sfp_scl_o            : out std_logic;
      sfp_scl_i            : in  std_logic                                     := '1';
      sfp_sda_o            : out std_logic;
      sfp_sda_i            : in  std_logic                                     := '1';
      sfp_det_i            : in  std_logic;
      btn1_i               : in  std_logic                                     := '1';
      btn2_i               : in  std_logic                                     := '1';
      spi_sclk_o           : out std_logic;
      spi_ncs_o            : out std_logic;
      spi_mosi_o           : out std_logic;
      spi_miso_i           : in  std_logic                                     := '0';
      uart_rxd_i           : in  std_logic                                     := '0';
      uart_txd_o           : out std_logic;
      owr_pwren_o          : out std_logic_vector(1 downto 0);
      owr_en_o             : out std_logic_vector(1 downto 0);
      owr_i                : in  std_logic_vector(1 downto 0)                  := (others => '1');
      slave_i              : in  t_wishbone_slave_in                           := cc_dummy_slave_in;
      slave_o              : out t_wishbone_slave_out;
      aux_master_o         : out t_wishbone_master_out;
      aux_master_i         : in  t_wishbone_master_in                          := cc_dummy_master_in;
      wrf_src_o            : out t_wrf_source_out;
      wrf_src_i            : in  t_wrf_source_in                               := c_dummy_src_in;
      wrf_snk_o            : out t_wrf_sink_out;
      wrf_snk_i            : in  t_wrf_sink_in                                 := c_dummy_snk_in;
      wrs_tx_data_i        : in  std_logic_vector(g_streamer_width-1 downto 0) := (others => '0');
      wrs_tx_valid_i       : in  std_logic                                     := '0';
      wrs_tx_dreq_o        : out std_logic;
      wrs_tx_last_i        : in  std_logic                                     := '1';
      wrs_tx_flush_i       : in  std_logic                                     := '0';
      wrs_rx_first_o       : out std_logic;
      wrs_rx_last_o        : out std_logic;
      wrs_rx_data_o        : out std_logic_vector(g_streamer_width-1 downto 0);
      wrs_rx_valid_o       : out std_logic;
      wrs_rx_dreq_i        : in  std_logic                                     := '0';
      wb_eth_master_o      : out t_wishbone_master_out;
      wb_eth_master_i      : in  t_wishbone_master_in                          := cc_dummy_master_in;
      timestamps_o         : out t_txtsu_timestamp;
      timestamps_ack_i     : in  std_logic                                     := '1';
      fc_tx_pause_req_i    : in  std_logic                                     := '0';
      fc_tx_pause_delay_i  : in  std_logic_vector(15 downto 0)                 := x"0000";
      fc_tx_pause_ready_o  : out std_logic;
      tm_link_up_o         : out std_logic;
      tm_dac_value_o       : out std_logic_vector(23 downto 0);
      tm_dac_wr_o          : out std_logic_vector(g_aux_clks-1 downto 0);
      tm_clk_aux_lock_en_i : in  std_logic_vector(g_aux_clks-1 downto 0)       := (others => '0');
      tm_clk_aux_locked_o  : out std_logic_vector(g_aux_clks-1 downto 0);
      tm_time_valid_o      : out std_logic;
      tm_tai_o             : out std_logic_vector(39 downto 0);
      tm_cycles_o          : out std_logic_vector(27 downto 0);
      pps_p_o              : out std_logic;
      pps_led_o            : out std_logic;
      link_ok_o            : out std_logic);
  end component xwrc_board_common;

end wr_board_pkg;

package body wr_board_pkg is

  procedure f_check_fabric_iface_type (
    constant iface_type : in t_board_fabric_iface) is
  begin
    if iface_type >= always_last_invalid then
      assert FALSE
        report "WR PTP core fabric interface [" & t_board_fabric_iface'image(iface_type) & "] is not supported"
        severity ERROR;
    end if;
  end procedure f_check_fabric_iface_type;

  function f_str2iface_type (
    constant iface_str : string(1 to 9))
    return t_board_fabric_iface is
    variable result : t_board_fabric_iface;
  begin
    case iface_str is
      when "PLAINFBRC" => result := PLAIN;
      when "STREAMERS" => result := STREAMERS;
      when "ETHERBONE" => result := ETHERBONE;
      when others      => result := always_last_invalid;
    end case;
    return result;
  end function f_str2iface_type;

end package body wr_board_pkg;
