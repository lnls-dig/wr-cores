library ieee;
use ieee.std_logic_1164.all;

library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;
use work.sysc_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;
use work.endpoint_pkg.all;

package wr_xilinx_pkg is

  -------------------------------------------------------------------------------------------
  -- records used as interface between WRPC and platform-specific module xwrc_platform_xilinx
  -------------------------------------------------------------------------------------------
  type t_sfp_from_wrc is record
    scl  : std_logic;
    sda  : std_logic;
  end record;

  type t_sfp_to_wrc is record
    scl      : std_logic;
    sda      : std_logic;
    det      : std_logic;
  end record;

  type t_dacs_from_wrc is record
    hpll_load_p1   : std_logic;
    hpll_data      : std_logic_vector(15 downto 0);
    dpll_load_p1   : std_logic;
    dpll_data      : std_logic_vector(15 downto 0);
  end record;

  type t_extref_to_wrc is record
    clk_10m_ref     : std_logic;
    clk_125m_ref    : std_logic;
    locked  : std_logic;
    stopped : std_logic;
    pps     : std_logic;
  end record;

  -------------------------------------------------------------------------------------------
  component xwrc_platform_xilinx
    generic
    (
      g_simulation         :       integer := 0;
      g_family             :       string  := "spartan6";
      g_with_10m_refin     :       integer := 0
    );
    port (
      local_reset_n_i      : in    std_logic;
      clk_20m_vcxo_i       : in    std_logic;                     -- 20MHz VCXO clock
      clk_125m_pllref_p_i  : in    std_logic;                     -- 125 MHz PLL reference
      clk_125m_pllref_n_i  : in    std_logic;
      clk_125m_gtp_n_i     : in    std_logic;                     -- 125 MHz GTP reference
      clk_125m_gtp_p_i     : in    std_logic;                     -- 125 MHz GTP reference
      clk_10m_ref_p_i      : in    std_logic := '0';              -- 10MHz external reference
      clk_10m_ref_n_i      : in    std_logic := '0';              -- 10MHz external reference
      pps_ext_i            : in    std_logic := '0';              -- external 1-PPS from reference
      dac_sclk_o           : out   std_logic;                      -- Serial Clock Line
      dac_din_o            : out   std_logic;                      -- Serial Data Line
      dac_cs1_n_o          : out   std_logic;                      -- Chip Select
      dac_cs2_n_o          : out   std_logic;                      -- Chip Select
      carrier_onewire_b    : inout std_logic := '1';               -- read temperature sensor
      sfp_txp_o            : out   std_logic;
      sfp_txn_o            : out   std_logic;
      sfp_rxp_i            : in    std_logic;
      sfp_rxn_i            : in    std_logic;
      sfp_mod_def0_i       : in    std_logic;                      -- sfp detect
      sfp_mod_def1_b       : inout std_logic;                      -- Config-I2C: Clk  Line
      sfp_mod_def2_b       : inout std_logic;                      -- Config-I2C: Data Line
      sfp_rate_select_b    : inout std_logic;
      sfp_tx_fault_i       : in    std_logic;
      sfp_tx_disable_o     : out   std_logic;
      sfp_los_i            : in    std_logic;
      clk_62m5_sys_o       : out std_logic;
      clk_125m_pllref_o    : out std_logic;
      clk_62m5_dmtd_o      : out std_logic;
      dacs_i               : in  t_dacs_from_wrc;
      phy8_o               : out t_phy_8bits_to_wrc;
      phy8_i               : in  t_phy_8bits_from_wrc := c_dummy_phy8_from_wrc;
      phy16_o              : out t_phy_16bits_to_wrc;
      phy16_i              : in  t_phy_16bits_from_wrc := c_dummy_phy16_from_wrc;
      owr_en_i             : in  std_logic_vector(1 downto 0);
      owr_o                : out std_logic_vector(1 downto 0);
      sfp_config_o         : out t_sfp_to_wrc;
      sfp_config_i         : in  t_sfp_from_wrc;
      ext_ref_o            : out t_extref_to_wrc;
      ext_ref_rst_i        : in  std_logic := '0'
      );
  end component;

  component wr_gtp_phy_spartan6
    generic (
      g_enable_ch0 : integer := 1;
      g_enable_ch1 : integer := 1;
      g_simulation : integer := 0);
    port (
      gtp_clk_i          : in  std_logic;
      ch0_ref_clk_i      : in  std_logic := '0';
      ch0_tx_data_i      : in  std_logic_vector(7 downto 0) := "00000000";
      ch0_tx_k_i         : in  std_logic                    := '0';
      ch0_tx_disparity_o : out std_logic;
      ch0_tx_enc_err_o   : out std_logic;
      ch0_rx_rbclk_o     : out std_logic;
      ch0_rx_data_o      : out std_logic_vector(7 downto 0);
      ch0_rx_k_o         : out std_logic;
      ch0_rx_enc_err_o   : out std_logic;
      ch0_rx_bitslide_o  : out std_logic_vector(3 downto 0);
      ch0_rst_i          : in  std_logic := '0';
      ch0_loopen_i       : in  std_logic := '0';
      ch0_loopen_vec_i   : in  std_logic_vector(2 downto 0) := (others=>'0');
      ch0_tx_prbs_sel_i  : in  std_logic_vector(2 downto 0) := (others=>'0');
      ch0_rdy_o          : out std_logic;
      ch1_ref_clk_i      : in  std_logic;
      ch1_tx_data_i      : in  std_logic_vector(7 downto 0) := "00000000";
      ch1_tx_k_i         : in  std_logic                    := '0';
      ch1_tx_disparity_o : out std_logic;
      ch1_tx_enc_err_o   : out std_logic;
      ch1_rx_data_o      : out std_logic_vector(7 downto 0);
      ch1_rx_rbclk_o     : out std_logic;
      ch1_rx_k_o         : out std_logic;
      ch1_rx_enc_err_o   : out std_logic;
      ch1_rx_bitslide_o  : out std_logic_vector(3 downto 0);
      ch1_rst_i          : in  std_logic                    := '0';
      ch1_loopen_i       : in  std_logic                    := '0';
      ch1_loopen_vec_i   : in  std_logic_vector(2 downto 0) := (others=>'0');
      ch1_tx_prbs_sel_i  : in  std_logic_vector(2 downto 0) := (others=>'0');
      ch1_rdy_o          : out std_logic;
      pad_txn0_o         : out std_logic;
      pad_txp0_o         : out std_logic;
      pad_rxn0_i         : in  std_logic                    := '0';
      pad_rxp0_i         : in  std_logic                    := '0';
      pad_txn1_o         : out std_logic;
      pad_txp1_o         : out std_logic;
      pad_rxn1_i         : in  std_logic                    := '0';
      pad_rxp1_i         : in  std_logic                    := '0');
  end component;

  component wr_gtx_phy_kintex7 is
    generic (
      -- set to non-zero value to speed up the simulation by reducing some delays
      g_simulation : integer := 0);
    port (
      clk_gtx_i      : in  std_logic;
      tx_out_clk_o   : out std_logic;
      tx_data_i      : in  std_logic_vector(15 downto 0);
      tx_k_i         : in  std_logic_vector(1 downto 0);
      tx_disparity_o : out std_logic;
      tx_enc_err_o   : out std_logic;
      rx_rbclk_o     : out std_logic;
      rx_data_o      : out std_logic_vector(15 downto 0);
      rx_k_o         : out std_logic_vector(1 downto 0);
      rx_enc_err_o   : out std_logic;
      rx_bitslide_o  : out std_logic_vector(4 downto 0);
      rst_i          : in  std_logic;
      loopen_i       : in  std_logic_vector(2 downto 0);
      tx_prbs_sel_i  : in  std_logic_vector(2 downto 0);
      pad_txn_o      : out std_logic;
      pad_txp_o      : out std_logic;
      pad_rxn_i      : in  std_logic := '0';
      pad_rxp_i      : in  std_logic := '0');
  end component;

  component ext_pll_10_to_125m
    port (
      clk_ext_i     : in  std_logic;
      clk_ext_mul_o : out std_logic;
      rst_a_i       : in  std_logic;
      clk_in_stopped_o: out  std_logic;
      locked_o      : out std_logic);
  end component ext_pll_10_to_125m;

end wr_xilinx_pkg;
