
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;
use work.wrcore_pkg.all;

library unisim;
use unisim.VCOMPONENTS.all;


entity kintex7_top is
  generic(
    g_simulation  : integer := 0;
    g_dpram_size  : integer := 131072/4;
    g_dpram_initf : string := ""
    );
  port (
    -- Global ports
    clk_20m_vcxo_i : in std_logic;    -- 20MHz VCXO clock

    fpga_pll_ref_clk_101_p_i : in std_logic;  -- 125 MHz PLL reference as well as dedicated
    fpga_pll_ref_clk_101_n_i : in std_logic;  --   clock for Xilinx GTX transceiver

    -- No Genum PCI Express interface included in the design

    -- No General Purpose Interface included in the design

    -- LEDs
    LED_ACT                  : out    std_logic;
    LED_LINK                 : out    std_logic;

    dac_sclk_o               : out    std_logic;
    dac_din_o                : out    std_logic;
    --dac_clr_n_o              : out std_logic;
    dac_cs1_n_o              : out    std_logic;
    dac_cs2_n_o              : out    std_logic;

    fpga_scl_b               : inout  std_logic;
    fpga_sda_b               : inout  std_logic;

    button1_i                : in     std_logic := 'H';
    button2_i                : in     std_logic := 'H';

    --spi_sclk_o               : out std_logic;
    --spi_ncs_o                : out std_logic;
    --spi_mosi_o               : out std_logic;
    --spi_miso_i               : in  std_logic := 'L';

    thermo_id                : inout  std_logic;      -- 1-Wire interface to DS18B20

    -------------------------------------------------------------------------
    -- SFP pins
    -------------------------------------------------------------------------
    sfp_txp_o                : out    std_logic;
    sfp_txn_o                : out    std_logic;

    sfp_rxp_i                : in     std_logic;
    sfp_rxn_i                : in     std_logic;

    sfp_mod_def0_b           : in     std_logic;  -- sfp detect
    sfp_mod_def1_b           : inout  std_logic;  -- scl
    sfp_mod_def2_b           : inout  std_logic;  -- sda
    sfp_rate_select_b        : inout  std_logic;
    sfp_tx_fault_i           : in     std_logic;
    sfp_tx_disable_o         : out    std_logic;
    sfp_los_i                : in     std_logic;
    
    -------------------------------------------------------------------------
    -- Digital I/O FMC Pins
    -------------------------------------------------------------------------
    dio_clk_p_i              : in     std_logic;
    dio_clk_n_i              : in     std_logic;

    --dio_n_i : in std_logic_vector(4 downto 0);
    --dio_p_i : in std_logic_vector(4 downto 0);

    --dio_n_o : out std_logic_vector(4 downto 0);
    --dio_p_o : out std_logic_vector(4 downto 0);

    --dio_oe_n_o    : out std_logic_vector(4 downto 0);
    --dio_term_en_o : out std_logic_vector(4 downto 0);

    dio_onewire_b  : inout std_logic;
    --dio_sdn_n_o    : out   std_logic;
    --dio_sdn_ck_n_o : out   std_logic;
    
    dio_led_top_o            : out    std_logic;
    dio_led_bot_o            : out    std_logic;
    
    -----------------------------------------
    --UART
    -----------------------------------------
    uart_rxd_i               : in     std_logic;
    uart_txd_o               : out    std_logic;

    -----------------------------------------
    --Test pins
    -----------------------------------------
    PLL_OE_OUT_B             : out    std_logic;
    pps_ext_i                : in     std_logic := '0';
    pps_n_o                  : out    std_logic;
    pps_p_o                  : out    std_logic);
end entity kintex7_top;

architecture structure of kintex7_top is

  ------------------------------------------------------------------------------
  -- Components declaration
  ------------------------------------------------------------------------------
  
  component spec_reset_gen
    port (
      clk_sys_i        : in     std_logic;
      rst_pcie_n_a_i   : in     std_logic;
      rst_button_n_a_i : in     std_logic;
      rst_n_o          : out    std_logic);
  end component spec_reset_gen;

  component ext_pll_10_to_125m
    port (
      clk_ext_i     : in     std_logic;
      clk_ext_mul_o : out    std_logic;
      rst_a_i       : in     std_logic;
      locked_o      : out    std_logic);
  end component ext_pll_10_to_125m;

  component wr_gtx_phy_kintex7
    generic(
      g_simulation : integer := 0);
    port (
      clk_gtx_i      : in     std_logic;
      tx_data_i      : in     std_logic_vector(15 downto 0);
      tx_k_i         : in     std_logic_vector(1 downto 0);
      tx_disparity_o : out    std_logic;
      tx_enc_err_o   : out    std_logic;
      rx_rbclk_o     : out    std_logic;
      rx_data_o      : out    std_logic_vector(15 downto 0);
      rx_k_o         : out    std_logic_vector(1 downto 0);
      rx_enc_err_o   : out    std_logic;
      rx_bitslide_o  : out    std_logic_vector(4 downto 0);
      rst_i          : in     std_logic;
      loopen_i       : in     std_logic_vector(2 downto 0);
      pad_txn_o      : out    std_logic;
      pad_txp_o      : out    std_logic;
      pad_rxn_i      : in     std_logic := '0';
      pad_rxp_i      : in     std_logic := '0';
      tx_out_clk_o   : out    std_logic;
      tx_locked_o    : out    std_logic;
      tx_prbs_sel_i  : in     std_logic_vector(2 downto 0);
      rdy_o          : out    std_logic);
  end component wr_gtx_phy_kintex7;

  ------------------------------------------------------------------------------
  -- Constants declaration
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- Signals declaration
  ------------------------------------------------------------------------------

  -- Dedicated clock for GTP transceiver
  signal clk_gtx              : std_logic;
  
  -- Reset

  -- DMA wishbone bus

  -- SPI

  signal pllout_clk_sys       : std_logic;
  signal pllout_clk_dmtd      : std_logic;
  signal pllout_clk_fb_pllref : std_logic;
  signal pllout_clk_fb_dmtd   : std_logic;

  signal clk_20m_vcxo_buf   : std_logic;
  signal clk_125m_pllref    : std_logic;
  signal clk_sys            : std_logic;
  signal clk_dmtd           : std_logic;
  signal clk_125m_ref      : std_logic;

  signal wrc_scl_o          : std_logic;
  signal wrc_scl_i          : std_logic;
  signal wrc_sda_o          : std_logic;
  signal wrc_sda_i          : std_logic;
  signal sfp_scl_o          : std_logic;
  signal sfp_scl_i          : std_logic;
  signal sfp_sda_o          : std_logic;
  signal sfp_sda_i          : std_logic;
  
  signal dac_hpll_load_p1_o : std_logic;
  signal dac_dpll_load_p1_o : std_logic;
  signal dac_hpll_data_o    : std_logic_vector(15 downto 0);
  signal dac_dpll_data_o    : std_logic_vector(15 downto 0);
  
  signal pps                : std_logic;
  signal pps_led            : std_logic;
  
  signal phy_tx_data      : std_logic_vector(15 downto 0);
  signal phy_tx_k         : std_logic_vector(1 downto 0);
  signal phy_tx_disparity : std_logic;
  signal phy_tx_enc_err   : std_logic;
  signal phy_rx_data          : std_logic_vector(15 downto 0);
  signal phy_rx_rbclk       : std_logic;
  signal phy_rx_k             : std_logic_vector(1 downto 0);
  signal phy_rx_enc_err   : std_logic;
  signal phy_rx_bitslide       : std_logic_vector(4 downto 0);
  signal phy_rst          : std_logic;
  signal phy_loopen       : std_logic;
  --loopen_i determines (7 Series Transceiver User Guide(UG476) Figure 2-23 and Table 2-37):
  --'0' => gtx_loopback = "000" => normal operation
  --'1' => gtx_loopback = "100" => Far-end PMA Loopback
  signal phy_loopen_vec   : std_logic_vector(2 downto 0);
  signal phy_prbs_sel        : std_logic_vector(2 downto 0);
  signal phy_rdy              : std_logic;

  --signal dio_in  : std_logic_vector(4 downto 0);
  --signal dio_out : std_logic_vector(4 downto 0);
  --signal dio_clk : std_logic;
  
  signal local_reset_n      : std_logic;

  --signal genum_wb_out    : t_wishbone_master_out;
  --signal genum_wb_in     : t_wishbone_master_in;
  --signal genum_csr_ack_i : std_logic;

  --signal wrc_slave_i : t_wishbone_slave_in;
  --signal wrc_slave_o : t_wishbone_slave_out;

  --signal etherbone_rst_n   : std_logic;
  --signal etherbone_src_out : t_wrf_source_out;
  --signal etherbone_src_in  : t_wrf_source_in;
  --signal etherbone_snk_out : t_wrf_sink_out;
  --signal etherbone_snk_in  : t_wrf_sink_in;
  --signal etherbone_wb_out  : t_wishbone_master_out;
  --signal etherbone_wb_in   : t_wishbone_master_in;
  --signal etherbone_cfg_in  : t_wishbone_slave_in;
  --signal etherbone_cfg_out : t_wishbone_slave_out;

  
  signal owr_en             : std_logic_vector(1 downto 0);
  signal owr_i              : std_logic_vector(1 downto 0);

  signal local_reset        : std_logic;
  signal ext_pll_reset      : std_logic;
  signal clk_ext            : std_ulogic;
  signal clk_ext_mul        : std_logic;
  signal clk_ext_mul_locked           : std_logic;
  --signal clk_ref_div2               : std_logic;

  signal dac_cs_n_o         : std_logic_vector(1 downto 0);
  signal button1_n_i        : std_logic;

begin

  local_reset <= not local_reset_n;
  button1_n_i <= not button1_i;

  U_Ext_PLL: ext_pll_10_to_125m
    port map(
      clk_ext_i     => clk_ext,
      clk_ext_mul_o => clk_ext_mul,
      rst_a_i       => ext_pll_reset,
      locked_o      => clk_ext_mul_locked);

  U_Extend_EXT_Reset: gc_extend_pulse
    generic map(
      g_width => 1000)
    port map(
      clk_i      => clk_sys,
      rst_n_i    => local_reset_n,
      pulse_i    => local_reset,
      extended_o => ext_pll_reset);

  cmp_sys_clk_pll: MMCME2_ADV
    generic map(
      BANDWIDTH            => "OPTIMIZED",
      CLKFBOUT_MULT_F      => 8.000,
      CLKFBOUT_PHASE       => 0.000,
      CLKFBOUT_USE_FINE_PS => FALSE,
      CLKIN1_PERIOD        => 8.00,
      CLKIN2_PERIOD        => 0.0,
      CLKOUT0_DIVIDE_F     => 16.000,         -- 62.5 MHz
      CLKOUT0_DUTY_CYCLE   => 0.500,
      CLKOUT0_PHASE        => 0.000,
      CLKOUT0_USE_FINE_PS  => FALSE,
      CLKOUT1_DIVIDE       => 8,              -- 125 MHz
      CLKOUT1_DUTY_CYCLE   => 0.500,
      CLKOUT1_PHASE        => 0.000,
      CLKOUT1_USE_FINE_PS  => FALSE,
      CLKOUT2_DIVIDE       => 1,
      CLKOUT2_DUTY_CYCLE   => 0.5,
      CLKOUT2_PHASE        => 0.0,
      CLKOUT2_USE_FINE_PS  => FALSE,
      CLKOUT3_DIVIDE       => 1,
      CLKOUT3_DUTY_CYCLE   => 0.5,
      CLKOUT3_PHASE        => 0.0,
      CLKOUT3_USE_FINE_PS  => FALSE,
      CLKOUT4_CASCADE      => FALSE,
      CLKOUT4_DIVIDE       => 1,
      CLKOUT4_DUTY_CYCLE   => 0.5,
      CLKOUT4_PHASE        => 0.0,
      CLKOUT4_USE_FINE_PS  => FALSE,
      CLKOUT5_DIVIDE       => 1,
      CLKOUT5_DUTY_CYCLE   => 0.5,
      CLKOUT5_PHASE        => 0.0,
      CLKOUT5_USE_FINE_PS  => FALSE,
      CLKOUT6_DIVIDE       => 1,
      CLKOUT6_DUTY_CYCLE   => 0.5,
      CLKOUT6_PHASE        => 0.0,
      CLKOUT6_USE_FINE_PS  => FALSE,
      COMPENSATION         => "ZHOLD",
      DIVCLK_DIVIDE        => 1,
      REF_JITTER1          => 0.010,
      REF_JITTER2          => 0.0,
      SS_EN                => "FALSE",
      SS_MODE              => "CENTER_HIGH",
      SS_MOD_PERIOD        => 10000,
      STARTUP_WAIT         => FALSE)
    port map(
      CLKFBOUT     => pllout_clk_fb_pllref,
      CLKFBOUTB    => open,
      CLKFBSTOPPED => open,
      CLKINSTOPPED => open,
      CLKOUT0      => pllout_clk_sys,
      CLKOUT0B     => open,
      CLKOUT1      => open,
      CLKOUT1B     => open,
      CLKOUT2      => open,
      CLKOUT2B     => open,
      CLKOUT3      => open,
      CLKOUT3B     => open,
      CLKOUT4      => open,
      CLKOUT5      => open,
      CLKOUT6      => open,
      DO           => open,
      DRDY         => open,
      LOCKED       => open,
      PSDONE       => open,
      CLKFBIN      => pllout_clk_fb_pllref,
      CLKIN1       => clk_125m_pllref,
      CLKIN2       => '0',
      CLKINSEL     => '1',
      DADDR        => (others => '0'),
      DCLK         => '0',
      DEN          => '0',
      DI           => (others => '0'),
      DWE          => '0',
      PSCLK        => '0',
      PSEN         => '0',
      PSINCDEC     => '0',
      PWRDWN       => '0',
      RST          => '0');

  cmp_dmtd_clk_pll: MMCME2_ADV
    generic map(
      BANDWIDTH            => "OPTIMIZED",
      CLKFBOUT_MULT_F      => 50.000,
      CLKFBOUT_PHASE       => 0.000,
      CLKFBOUT_USE_FINE_PS => FALSE,
      CLKIN1_PERIOD        => 50.000,
      CLKIN2_PERIOD        => 0.0,
      CLKOUT0_DIVIDE_F     => 16.000,         -- 62.5 MHz
      CLKOUT0_DUTY_CYCLE   => 0.500,
      CLKOUT0_PHASE        => 0.0,
      CLKOUT0_USE_FINE_PS  => FALSE,
      CLKOUT1_DIVIDE       => 16,             -- 62.5 MHz
      CLKOUT1_DUTY_CYCLE   => 0.500,
      CLKOUT1_PHASE        => 0.00,
      CLKOUT1_USE_FINE_PS  => FALSE,
      CLKOUT2_DIVIDE       => 1,
      CLKOUT2_DUTY_CYCLE   => 0.5,
      CLKOUT2_PHASE        => 0.0,
      CLKOUT2_USE_FINE_PS  => FALSE,
      CLKOUT3_DIVIDE       => 1,
      CLKOUT3_DUTY_CYCLE   => 0.5,
      CLKOUT3_PHASE        => 0.0,
      CLKOUT3_USE_FINE_PS  => FALSE,
      CLKOUT4_CASCADE      => FALSE,
      CLKOUT4_DIVIDE       => 1,
      CLKOUT4_DUTY_CYCLE   => 0.5,
      CLKOUT4_PHASE        => 0.0,
      CLKOUT4_USE_FINE_PS  => FALSE,
      CLKOUT5_DIVIDE       => 1,
      CLKOUT5_DUTY_CYCLE   => 0.5,
      CLKOUT5_PHASE        => 0.0,
      CLKOUT5_USE_FINE_PS  => FALSE,
      CLKOUT6_DIVIDE       => 1,
      CLKOUT6_DUTY_CYCLE   => 0.5,
      CLKOUT6_PHASE        => 0.0,
      CLKOUT6_USE_FINE_PS  => FALSE,
      COMPENSATION         => "ZHOLD",
      DIVCLK_DIVIDE        => 1,
      REF_JITTER1          => 0.010,
      REF_JITTER2          => 0.0,
      SS_EN                => "FALSE",
      SS_MODE              => "CENTER_HIGH",
      SS_MOD_PERIOD        => 10000,
      STARTUP_WAIT         => FALSE)
    port map(
      CLKFBOUT     => pllout_clk_fb_dmtd,
      CLKFBOUTB    => open,
      CLKFBSTOPPED => open,
      CLKINSTOPPED => open,
      CLKOUT0      => pllout_clk_dmtd,
      CLKOUT0B     => open,
      CLKOUT1      => open,
      CLKOUT1B     => open,
      CLKOUT2      => open,
      CLKOUT2B     => open,
      CLKOUT3      => open,
      CLKOUT3B     => open,
      CLKOUT4      => open,
      CLKOUT5      => open,
      CLKOUT6      => open,
      DO           => open,
      DRDY         => open,
      LOCKED       => open,
      PSDONE       => open,
      CLKFBIN      => pllout_clk_fb_dmtd,
      CLKIN1       => clk_20m_vcxo_buf,
      CLKIN2       => '0',
      CLKINSEL     => '1',
      DADDR        => (others => '0'),
      DCLK         => '0',
      DEN          => '0',
      DI           => (others => '0'),
      DWE          => '0',
      PSCLK        => '0',
      PSEN         => '0',
      PSINCDEC     => '0',
      PWRDWN       => '0',
      RST          => '0');

  U_Reset_Gen: spec_reset_gen
    port map(
      clk_sys_i        => clk_sys,
      rst_pcie_n_a_i   => '1',
      rst_button_n_a_i => button1_n_i,
      rst_n_o          => local_reset_n);

  cmp_clk_sys_buf: BUFG
    port map(
      O => clk_sys,
      I => pllout_clk_sys);

  cmp_clk_dmtd_buf: BUFG
    port map(
      O => clk_dmtd,
      I => pllout_clk_dmtd);

  cmp_clk_vcxo: IBUFG
    port map(
      O => clk_20m_vcxo_buf,
      I => clk_20m_vcxo_i);

  cmp_clk_125m_pllref_buf: BUFG
    port map(
      O => clk_125m_pllref,
      I => clk_gtx);

  ------------------------------------------------------------------------------
  -- Dedicated clock for GTP
  ------------------------------------------------------------------------------
      
  cmp_gtx_clk_buf: IBUFDS_GTE2
    generic map(
      CLKCM_CFG    => TRUE,
      CLKRCV_TRST  => TRUE,
      CLKSWING_CFG => "11")
    port map(
      O     => clk_gtx,
      ODIV2 => open,
      CEB   => '0',
      I     => fpga_pll_ref_clk_101_p_i,
      IB    => fpga_pll_ref_clk_101_n_i);

    fpga_scl_b <= '0' when wrc_scl_o = '0' else 'Z';
    fpga_sda_b <= '0' when wrc_sda_o = '0' else 'Z';
    wrc_scl_i  <= fpga_scl_b;
    wrc_sda_i  <= fpga_sda_b;

    sfp_mod_def1_b <= '0' when sfp_scl_o = '0' else 'Z';
    sfp_mod_def2_b <= '0' when sfp_sda_o = '0' else 'Z';
    sfp_scl_i      <= sfp_mod_def1_b;
    sfp_sda_i      <= sfp_mod_def2_b;
      
  U_WR_CORE: xwr_core
    generic map(
      g_simulation                => 0,
      g_with_external_clock_input => true,
      --
      g_phys_uart                 => true,
      g_virtual_uart              => true,
      g_aux_clks                  => 0,
      g_ep_rxbuf_size             => 1024,
      g_tx_runt_padding           => true,
      g_pcs_16bit                 => true,
      g_dpram_initf               => "",
      g_dpram_size                => 131072/4,
      g_interface_mode            => PIPELINED,
      g_address_granularity       => BYTE,
      g_aux_sdb                   => c_wrc_periph3_sdb,
      g_softpll_enable_debugger   => false,
      g_vuart_fifo_size           => 1024)
    port map(
      clk_sys_i            => clk_sys,
      clk_dmtd_i           => clk_dmtd,
      clk_ref_i            => clk_125m_ref,
      clk_aux_i            => (others => '0'),
      clk_ext_mul_i        => clk_ext_mul,
      clk_ext_mul_locked_i => clk_ext_mul_locked,
      clk_ext_i            => clk_ext,
      pps_ext_i            => pps_ext_i,
      rst_n_i              => local_reset_n,
      dac_hpll_load_p1_o   => dac_hpll_load_p1_o,
      dac_hpll_data_o      => dac_hpll_data_o,
      dac_dpll_load_p1_o   => dac_dpll_load_p1_o,
      dac_dpll_data_o      => dac_dpll_data_o,
      phy_ref_clk_i        => clk_125m_ref,
      phy_tx_data_o        => phy_tx_data,
      phy_tx_k_o           => phy_tx_k,
      phy_tx_disparity_i   => phy_tx_disparity,
      phy_tx_enc_err_i     => phy_tx_enc_err,
      phy_rx_data_i        => phy_rx_data,
      phy_rx_rbclk_i       => phy_rx_rbclk,
      phy_rx_k_i           => phy_rx_k,
      phy_rx_enc_err_i     => phy_rx_enc_err,
      phy_rx_bitslide_i    => phy_rx_bitslide,
      phy_rst_o            => phy_rst,
      phy_rdy_i            => phy_rdy,
      phy_loopen_o         => open,
      phy_loopen_vec_o     => phy_loopen_vec,
      phy_tx_prbs_sel_o    => phy_prbs_sel,
      phy_sfp_tx_fault_i   => sfp_tx_fault_i,
      phy_sfp_los_i        => sfp_los_i,
      phy_sfp_tx_disable_o => sfp_tx_disable_o,
      led_act_o            => LED_ACT,
      led_link_o           => LED_LINK,
      scl_o                => wrc_scl_o,
      scl_i                => wrc_scl_i,
      sda_o                => wrc_sda_o,
      sda_i                => wrc_sda_i,
      sfp_scl_o            => sfp_scl_o,
      sfp_scl_i            => sfp_scl_i,
      sfp_sda_o            => sfp_sda_o,
      sfp_sda_i            => sfp_sda_i,
      sfp_det_i            => sfp_mod_def0_b,
      btn1_i               => button1_n_i,
      btn2_i               => button2_i,
      spi_sclk_o           => open,
      spi_ncs_o            => open,
      spi_mosi_o           => open,
      spi_miso_i           => '0',
      uart_rxd_i           => uart_rxd_i,
      uart_txd_o           => uart_txd_o,
      owr_pwren_o          => open,
      owr_en_o             => owr_en,
      owr_i                => owr_i,
      slave_i              => cc_dummy_slave_in,
      slave_o              => open,
      aux_master_o         => open,
      aux_master_i         => cc_dummy_master_in,
      wrf_src_o            => open,
      wrf_src_i            => c_dummy_src_in,
      wrf_snk_o            => open,
      wrf_snk_i            => c_dummy_snk_in,
      timestamps_o         => open,
      timestamps_ack_i     => '1',
      fc_tx_pause_req_i    => '0',
      fc_tx_pause_delay_i  => x"0000",
      fc_tx_pause_ready_o  => open,
      tm_link_up_o         => open,
      tm_dac_value_o       => open,
      tm_dac_wr_o          => open,
      tm_clk_aux_lock_en_i => (others => '0'),
      tm_clk_aux_locked_o  => open,
      tm_time_valid_o      => open,
      tm_tai_o             => open,
      tm_cycles_o          => open,
      pps_p_o              => pps,
      pps_led_o            => pps_led,
      dio_o                => open,
      rst_aux_n_o          => open,
      link_ok_o            => open);
      
  U_GTP: wr_gtx_phy_kintex7
    generic map(
      g_simulation => g_simulation)
    port map(
      clk_gtx_i      => clk_gtx,
      tx_data_i      => phy_tx_data,
      tx_k_i         => phy_tx_k,
      tx_disparity_o => phy_tx_disparity,
      tx_enc_err_o   => phy_tx_enc_err,
      rx_rbclk_o     => phy_rx_rbclk,
      rx_data_o      => phy_rx_data,
      rx_k_o         => phy_rx_k,
      rx_enc_err_o   => phy_rx_enc_err,
      rx_bitslide_o  => phy_rx_bitslide,
      rst_i          => phy_rst,
      loopen_i       => phy_loopen_vec,
      pad_txn_o      => sfp_txn_o,
      pad_txp_o      => sfp_txp_o,
      pad_rxn_i      => sfp_rxn_i,
      pad_rxp_i      => sfp_rxp_i,
      tx_out_clk_o   => clk_125m_ref,
      tx_locked_o    => open,
      tx_prbs_sel_i  => phy_prbs_sel,
      rdy_o          => phy_rdy);
      
  U_DAC_ARB: spec_serial_dac_arb
    generic map(
      g_invert_sclk    => FALSE,
      g_num_extra_bits => 8)
    port map(
      clk_i       => clk_sys,
      rst_n_i     => local_reset_n,
      val1_i      => dac_dpll_data_o,
      load1_i     => dac_dpll_load_p1_o,
      val2_i      => dac_hpll_data_o,
      load2_i     => dac_hpll_load_p1_o,
      dac_cs_n_o  => dac_cs_n_o,
      dac_clr_n_o => open,
      dac_sclk_o  => dac_sclk_o,
      dac_din_o   => dac_din_o);
      
  U_Extend_PPS : gc_extend_pulse
    generic map (
      g_width => 10000000)
    port map (
      clk_i      => clk_125m_ref,
      rst_n_i    => local_reset_n,
      pulse_i    => pps_led,
      extended_o => dio_led_top_o);

--  gen_dio_iobufs : for i in 0 to 4 generate
--    U_ibuf : IBUFDS
--      generic map (
--        DIFF_TERM => true)
--      port map (
--        O  => dio_in(i),
--        I  => dio_p_i(i),
--        IB => dio_n_i(i)
--        );
--
--    U_obuf : OBUFDS
--      port map (
--        I  => dio_out(i),
--        O  => dio_p_o(i),
--        OB => dio_n_o(i)
--        );
--  end generate gen_dio_iobufs;

  U_input_buffer : IBUFGDS
    generic map (
      DIFF_TERM => true)
    port map (
      O  => clk_ext,
      I  => dio_clk_p_i,
      IB => dio_clk_n_i);

  dio_led_bot_o <= '0';

  U_PPS_buffer: OBUFDS
    generic map(
      CAPACITANCE => "DONT_CARE",
      IOSTANDARD  => "DEFAULT",
      SLEW        => "SLOW")
    port map(
      O  => pps_p_o,
      OB => pps_n_o,
      I  => pps);

  dac_cs1_n_o <= dac_cs_n_o(0);
  dac_cs2_n_o <= dac_cs_n_o(1);

  thermo_id <= '0' when owr_en(0) = '1' else 'Z';
  owr_i(0)  <= thermo_id;

  dio_onewire_b <= '0' when owr_en(1) = '1' else 'Z';
  owr_i(1)      <= dio_onewire_b;

   ------------------------------------------------------------------------------
  -- OE test output (KM3NeT CLB specific)
  ------------------------------------------------------------------------------
  PLL_OE_OUT_B <= '0';

end architecture structure ; -- of kintex7_top

