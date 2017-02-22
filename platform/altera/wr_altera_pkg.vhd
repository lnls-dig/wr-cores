library ieee;
use ieee.std_logic_1164.all;

library work;
use work.endpoint_pkg.all;

package wr_altera_pkg is

  component xwrc_platform_altera is
    generic (
      g_fpga_family               : string  := "arria5";
      g_with_external_clock_input : boolean := FALSE;
      g_use_default_plls          : boolean := TRUE;
      g_pcs_16bit                 : boolean := FALSE);
    port (
      areset_n_i            : in  std_logic             := '1';
      clk_10m_ext_i         : in  std_logic             := '0';
      clk_20m_vcxo_i        : in  std_logic             := '0';
      clk_125m_pllref_i     : in  std_logic             := '0';
      clk_62m5_dmtd_i       : in  std_logic             := '0';
      clk_dmtd_locked_i     : in  std_logic             := '1';
      clk_62m5_sys_i        : in  std_logic             := '0';
      clk_sys_locked_i      : in  std_logic             := '1';
      clk_125m_ref_i        : in  std_logic             := '0';
      clk_125m_ext_i        : in  std_logic             := '0';
      clk_ext_locked_i      : in  std_logic             := '1';
      clk_ext_stopped_i     : in  std_logic             := '0';
      clk_ext_rst_o         : out std_logic;
      sfp_tx_o              : out std_logic;
      sfp_rx_i              : in  std_logic;
      sfp_tx_fault_i        : in  std_logic             := '0';
      sfp_los_i             : in  std_logic             := '0';
      sfp_tx_disable_o      : out std_logic;
      clk_62m5_sys_o        : out std_logic;
      clk_125m_ref_o        : out std_logic;
      clk_62m5_dmtd_o       : out std_logic;
      pll_locked_o          : out std_logic;
      clk_10m_ext_o         : out std_logic;
      phy8_o                : out t_phy_8bits_to_wrc;
      phy8_i                : in  t_phy_8bits_from_wrc  := c_dummy_phy8_from_wrc;
      phy16_o               : out t_phy_16bits_to_wrc;
      phy16_i               : in  t_phy_16bits_from_wrc := c_dummy_phy16_from_wrc;
      ext_ref_mul_o         : out std_logic;
      ext_ref_mul_locked_o  : out std_logic;
      ext_ref_mul_stopped_o : out std_logic;
      ext_ref_rst_i         : in  std_logic             := '0');
  end component xwrc_platform_altera;

  component wr_arria2_phy
    generic (
      g_tx_latch_edge : std_logic := '1';
      g_rx_latch_edge : std_logic := '0');
    port (
      clk_reconf_i   : in  std_logic;
      clk_pll_i      : in  std_logic;
      clk_cru_i      : in  std_logic;
      clk_free_i     : in  std_logic;
      rst_i          : in  std_logic;
      locked_o       : out std_logic;
      loopen_i       : in  std_logic;
      drop_link_i    : in  std_logic;
      tx_clk_i       : in  std_logic;
      tx_data_i      : in  std_logic_vector(7 downto 0);
      tx_k_i         : in  std_logic;
      tx_disparity_o : out std_logic;
      tx_enc_err_o   : out std_logic;
      rx_rbclk_o     : out std_logic;
      rx_data_o      : out std_logic_vector(7 downto 0);
      rx_k_o         : out std_logic;
      rx_enc_err_o   : out std_logic;
      rx_bitslide_o  : out std_logic_vector(3 downto 0);
      pad_txp_o      : out std_logic;
      pad_rxp_i      : in  std_logic := '0');
  end component;

  component wr_arria5_phy is
    generic (
      g_pcs_16bit : boolean := FALSE);
    port (
      clk_reconf_i   : in  std_logic;
      clk_phy_i      : in  std_logic;
      ready_o        : out std_logic;
      loopen_i       : in  std_logic;
      drop_link_i    : in  std_logic;
      tx_clk_o       : out std_logic;
      tx_data_i      : in  std_logic_vector(f_pcs_data_width(g_pcs_16bit)-1 downto 0);
      tx_k_i         : in  std_logic_vector(f_pcs_k_width(g_pcs_16bit)-1 downto 0);
      tx_disparity_o : out std_logic;
      tx_enc_err_o   : out std_logic;
      rx_rbclk_o     : out std_logic;
      rx_data_o      : out std_logic_vector(f_pcs_data_width(g_pcs_16bit)-1 downto 0);
      rx_k_o         : out std_logic_vector(f_pcs_k_width(g_pcs_16bit)-1 downto 0);
      rx_enc_err_o   : out std_logic;
      rx_bitslide_o  : out std_logic_vector(f_pcs_bts_width(g_pcs_16bit)-1 downto 0);
      pad_txp_o      : out std_logic;
      pad_rxp_i      : in  std_logic := '0');
  end component;

  component arria5_phy_reconf is
    port (
      reconfig_busy             : out std_logic;
      mgmt_clk_clk              : in  std_logic                     := '0';
      mgmt_rst_reset            : in  std_logic                     := '0';
      reconfig_mgmt_address     : in  std_logic_vector(6 downto 0)  := (others => '0');
      reconfig_mgmt_read        : in  std_logic                     := '0';
      reconfig_mgmt_readdata    : out std_logic_vector(31 downto 0);
      reconfig_mgmt_waitrequest : out std_logic;
      reconfig_mgmt_write       : in  std_logic                     := '0';
      reconfig_mgmt_writedata   : in  std_logic_vector(31 downto 0) := (others => '0');
      reconfig_to_xcvr          : out std_logic_vector(139 downto 0);
      reconfig_from_xcvr        : in  std_logic_vector(91 downto 0) := (others => '0'));
  end component;

  component arria5_phy8 is
    port (
      phy_mgmt_clk                : in  std_logic                      := '0';
      phy_mgmt_clk_reset          : in  std_logic                      := '0';
      phy_mgmt_address            : in  std_logic_vector(8 downto 0)   := (others => '0');
      phy_mgmt_read               : in  std_logic                      := '0';
      phy_mgmt_readdata           : out std_logic_vector(31 downto 0);
      phy_mgmt_waitrequest        : out std_logic;
      phy_mgmt_write              : in  std_logic                      := '0';
      phy_mgmt_writedata          : in  std_logic_vector(31 downto 0)  := (others => '0');
      tx_ready                    : out std_logic;
      rx_ready                    : out std_logic;
      pll_ref_clk                 : in  std_logic_vector(0 downto 0)   := (others => '0');
      tx_serial_data              : out std_logic_vector(0 downto 0);
      tx_bitslipboundaryselect    : in  std_logic_vector(4 downto 0)   := (others => '0');
      pll_locked                  : out std_logic_vector(0 downto 0);
      rx_serial_data              : in  std_logic_vector(0 downto 0)   := (others => '0');
      rx_runningdisp              : out std_logic_vector(0 downto 0);
      rx_disperr                  : out std_logic_vector(0 downto 0);
      rx_errdetect                : out std_logic_vector(0 downto 0);
      rx_bitslipboundaryselectout : out std_logic_vector(4 downto 0);
      tx_clkout                   : out std_logic_vector(0 downto 0);
      rx_clkout                   : out std_logic_vector(0 downto 0);
      tx_parallel_data            : in  std_logic_vector(7 downto 0)   := (others => '0');
      tx_datak                    : in  std_logic_vector(0 downto 0)   := (others => '0');
      rx_parallel_data            : out std_logic_vector(7 downto 0);
      rx_datak                    : out std_logic_vector(0 downto 0);
      reconfig_from_xcvr          : out std_logic_vector(91 downto 0);
      reconfig_to_xcvr            : in  std_logic_vector(139 downto 0) := (others => '0'));
  end component;

  component arria5_phy16 is
    port (
      phy_mgmt_clk                : in  std_logic                      := '0';
      phy_mgmt_clk_reset          : in  std_logic                      := '0';
      phy_mgmt_address            : in  std_logic_vector(8 downto 0)   := (others => '0');
      phy_mgmt_read               : in  std_logic                      := '0';
      phy_mgmt_readdata           : out std_logic_vector(31 downto 0);
      phy_mgmt_waitrequest        : out std_logic;
      phy_mgmt_write              : in  std_logic                      := '0';
      phy_mgmt_writedata          : in  std_logic_vector(31 downto 0)  := (others => '0');
      tx_ready                    : out std_logic;
      rx_ready                    : out std_logic;
      pll_ref_clk                 : in  std_logic_vector(0 downto 0)   := (others => '0');
      tx_serial_data              : out std_logic_vector(0 downto 0);
      tx_bitslipboundaryselect    : in  std_logic_vector(4 downto 0)   := (others => '0');
      pll_locked                  : out std_logic_vector(0 downto 0);
      rx_serial_data              : in  std_logic_vector(0 downto 0)   := (others => '0');
      rx_runningdisp              : out std_logic_vector(1 downto 0);
      rx_disperr                  : out std_logic_vector(1 downto 0);
      rx_errdetect                : out std_logic_vector(1 downto 0);
      rx_bitslipboundaryselectout : out std_logic_vector(4 downto 0);
      tx_clkout                   : out std_logic_vector(0 downto 0);
      rx_clkout                   : out std_logic_vector(0 downto 0);
      tx_parallel_data            : in  std_logic_vector(15 downto 0)  := (others => '0');
      tx_datak                    : in  std_logic_vector(1 downto 0)   := (others => '0');
      rx_parallel_data            : out std_logic_vector(15 downto 0);
      rx_datak                    : out std_logic_vector(1 downto 0);
      reconfig_from_xcvr          : out std_logic_vector(91 downto 0);
      reconfig_to_xcvr            : in  std_logic_vector(139 downto 0) := (others => '0'));
  end component;

  component arria5_dmtd_pll_default is
    port (
      refclk   : in  std_logic := '0';
      rst      : in  std_logic := '0';
      outclk_0 : out std_logic;
      locked   : out std_logic);
  end component;

  component arria5_sys_pll_default is
    port (
      refclk   : in  std_logic := '0';
      rst      : in  std_logic := '0';
      outclk_0 : out std_logic;
      outclk_1 : out std_logic;
      locked   : out std_logic);
  end component;

  component arria5_ext_ref_pll_default is
    port (
      refclk   : in  std_logic := '0';
      rst      : in  std_logic := '0';
      outclk_0 : out std_logic;
      locked   : out std_logic);
  end component arria5_ext_ref_pll_default;

end wr_altera_pkg;
