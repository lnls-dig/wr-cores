
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.gn4124_core_pkg.all;
use work.gencores_pkg.all;
use work.wrcore_pkg.all;
use work.wr_fabric_pkg.all;
use work.wr_xilinx_pkg.all;
use work.etherbone_pkg.all;

library UNISIM;
use UNISIM.vcomponents.all;

library work;
use work.wishbone_pkg.all;


entity spec_top is
  generic
    (
      TAR_ADDR_WDTH : integer := 13     -- not used for this project
      );
  port
    (
      -- Global ports
      clk_20m_vcxo_i : in std_logic;    -- 20MHz VCXO clock

      clk_125m_pllref_p_i : in std_logic;  -- 125 MHz PLL reference
      clk_125m_pllref_n_i : in std_logic;

      clk_125m_gtp_p_i : in std_logic;  -- Dedicated clock for Xilinx GTP transceiver
      clk_125m_gtp_n_i : in std_logic;

      -- From GN4124 Local bus
      L_CLKp : in std_logic;  -- Local bus clock (frequency set in GN4124 config registers)
      L_CLKn : in std_logic;  -- Local bus clock (frequency set in GN4124 config registers)

      L_RST_N : in std_logic;           -- Reset from GN4124 (RSTOUT18_N)

      -- General Purpose Interface
      GPIO : inout std_logic_vector(1 downto 0);  -- GPIO[0] -> GN4124 GPIO8
                                                  -- GPIO[1] -> GN4124 GPIO9

      -- PCIe to Local [Inbound Data] - RX
      P2L_RDY    : out std_logic;       -- Rx Buffer Full Flag
      P2L_CLKn   : in  std_logic;       -- Receiver Source Synchronous Clock-
      P2L_CLKp   : in  std_logic;       -- Receiver Source Synchronous Clock+
      P2L_DATA   : in  std_logic_vector(15 downto 0);  -- Parallel receive data
      P2L_DFRAME : in  std_logic;       -- Receive Frame
      P2L_VALID  : in  std_logic;       -- Receive Data Valid

      -- Inbound Buffer Request/Status
      P_WR_REQ : in  std_logic_vector(1 downto 0);  -- PCIe Write Request
      P_WR_RDY : out std_logic_vector(1 downto 0);  -- PCIe Write Ready
      RX_ERROR : out std_logic;                     -- Receive Error

      -- Local to Parallel [Outbound Data] - TX
      L2P_DATA   : out std_logic_vector(15 downto 0);  -- Parallel transmit data
      L2P_DFRAME : out std_logic;       -- Transmit Data Frame
      L2P_VALID  : out std_logic;       -- Transmit Data Valid
      L2P_CLKn   : out std_logic;  -- Transmitter Source Synchronous Clock-
      L2P_CLKp   : out std_logic;  -- Transmitter Source Synchronous Clock+
      L2P_EDB    : out std_logic;       -- Packet termination and discard

      -- Outbound Buffer Status
      L2P_RDY    : in std_logic;        -- Tx Buffer Full Flag
      L_WR_RDY   : in std_logic_vector(1 downto 0);  -- Local-to-PCIe Write
      P_RD_D_RDY : in std_logic_vector(1 downto 0);  -- PCIe-to-Local Read Response Data Ready
      TX_ERROR   : in std_logic;        -- Transmit Error
      VC_RDY     : in std_logic_vector(1 downto 0);  -- Channel ready

      -- Font panel LEDs
      LED_RED   : out std_logic;
      LED_GREEN : out std_logic;

      dac_sclk_o  : out std_logic;
      dac_din_o   : out std_logic;
      dac_clr_n_o : out std_logic;
      dac_cs1_n_o : out std_logic;
      dac_cs2_n_o : out std_logic;

      fpga_scl_b : inout std_logic;
      fpga_sda_b : inout std_logic;

      button1_i : in std_logic := 'H';
      button2_i : in std_logic := 'H';
      
      spi_sclk_o : out std_logic;
      spi_ncs_o  : out std_logic;
      spi_mosi_o : out std_logic;
      spi_miso_i : in  std_logic := 'L';

      thermo_id : inout std_logic;      -- 1-Wire interface to DS18B20

      -------------------------------------------------------------------------
      -- SFP pins
      -------------------------------------------------------------------------

      sfp_txp_o : out std_logic;
      sfp_txn_o : out std_logic;

      sfp_rxp_i : in std_logic;
      sfp_rxn_i : in std_logic;

      sfp_mod_def0_i    : in    std_logic;  -- sfp detect
      sfp_mod_def1_b    : inout std_logic;  -- scl
      sfp_mod_def2_b    : inout std_logic;  -- sda
      sfp_rate_select_b : inout std_logic;
      sfp_tx_fault_i    : in    std_logic;
      sfp_tx_disable_o  : out   std_logic;
      sfp_los_i         : in    std_logic;


      -------------------------------------------------------------------------
      -- Digital I/O FMC Pins
      -------------------------------------------------------------------------

      dio_clk_p_i : in std_logic;
      dio_clk_n_i : in std_logic;

      dio_n_i : in std_logic_vector(4 downto 0);
      dio_p_i : in std_logic_vector(4 downto 0);

      dio_n_o : out std_logic_vector(4 downto 0);
      dio_p_o : out std_logic_vector(4 downto 0);

      dio_oe_n_o    : out std_logic_vector(4 downto 0);
      dio_term_en_o : out std_logic_vector(4 downto 0);

      dio_onewire_b  : inout std_logic;
      dio_sdn_n_o    : out   std_logic;
      dio_sdn_ck_n_o : out   std_logic;

      dio_led_top_o : out std_logic;
      dio_led_bot_o : out std_logic;

      -----------------------------------------
      --UART
      -----------------------------------------
      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic
      );

end spec_top;

architecture rtl of spec_top is

  ------------------------------------------------------------------------------
  -- Components declaration
  ------------------------------------------------------------------------------
  component spec_reset_gen
    port (
      clk_sys_i        : in  std_logic;
      rst_pcie_n_a_i   : in  std_logic;
      rst_button_n_a_i : in  std_logic;
      rst_n_o          : out std_logic);
  end component;

  component ext_pll_10_to_125m
    port (
      clk_ext_i     : in  std_logic;
      clk_ext_mul_o : out std_logic;
      rst_a_i       : in  std_logic;
      clk_in_stopped_o: out  std_logic;
      locked_o      : out std_logic);
  end component;

  --component chipscope_ila
  --  port (
  --    CONTROL : inout std_logic_vector(35 downto 0);
  --    CLK     : in    std_logic;
  --    TRIG0   : in    std_logic_vector(31 downto 0);
  --    TRIG1   : in    std_logic_vector(31 downto 0);
  --    TRIG2   : in    std_logic_vector(31 downto 0);
  --    TRIG3   : in    std_logic_vector(31 downto 0));
  --end component;

  --signal CONTROL : std_logic_vector(35 downto 0);
  --signal CLK     : std_logic;
  --signal TRIG0   : std_logic_vector(31 downto 0);
  --signal TRIG1   : std_logic_vector(31 downto 0);
  --signal TRIG2   : std_logic_vector(31 downto 0);
  --signal TRIG3   : std_logic_vector(31 downto 0);

  --component chipscope_icon
  --  port (
  --    CONTROL0 : inout std_logic_vector (35 downto 0));
  --end component;

  ------------------------------------------------------------------------------
  -- Constants declaration
  ------------------------------------------------------------------------------
  constant c_BAR0_APERTURE     : integer := 20;
  constant c_CSR_WB_SLAVES_NB  : integer := 1;
  constant c_DMA_WB_SLAVES_NB  : integer := 1;
  constant c_DMA_WB_ADDR_WIDTH : integer := 26;

  ------------------------------------------------------------------------------
  -- Signals declaration
  ------------------------------------------------------------------------------

  -- LCLK from GN4124 used as system clock
  signal l_clk : std_logic;

  signal clk_125m_pllref  : std_logic;
  signal clk_62m5_sys          : std_logic;
  signal clk_dmtd         : std_logic;

  signal wrc_scl_o : std_logic;
  signal wrc_scl_i : std_logic;
  signal wrc_sda_o : std_logic;
  signal wrc_sda_i : std_logic;

  signal pps     : std_logic;
  signal pps_led : std_logic;

  signal dio_in  : std_logic_vector(4 downto 0);
  signal dio_out : std_logic_vector(4 downto 0);

  signal local_reset_n  : std_logic;

  signal genum_wb_out    : t_wishbone_master_out;
  signal genum_wb_in     : t_wishbone_master_in;

  signal wrc_slave_i : t_wishbone_slave_in;
  signal wrc_slave_o : t_wishbone_slave_out;

  signal owr_en : std_logic_vector(1 downto 0);
  signal owr_i  : std_logic_vector(1 downto 0);

  signal wb_adr : std_logic_vector(31 downto 0);  --c_BAR0_APERTURE-priv_log2_ceil(c_CSR_WB_SLAVES_NB+1)-1 downto 0);

  signal etherbone_rst_n   : std_logic;
  signal etherbone_src_out : t_wrf_source_out;
  signal etherbone_src_in  : t_wrf_source_in;
  signal etherbone_snk_out : t_wrf_sink_out;
  signal etherbone_snk_in  : t_wrf_sink_in;
  signal etherbone_wb_out  : t_wishbone_master_out;
  signal etherbone_wb_in   : t_wishbone_master_in;
  signal etherbone_cfg_in  : t_wishbone_slave_in;
  signal etherbone_cfg_out : t_wishbone_slave_out;

  signal ext_pll_reset : std_logic;
  signal clk_ext, clk_ext_mul       : std_logic;
  signal clk_ext_mul_locked         : std_logic;
  signal clk_ext_stopped            : std_logic;
  signal clk_ext_rst                : std_logic;
  signal clk_ref_div2               : std_logic;

  -- WRPC <--> WRP (WR PTP Core Platform-dependent peripherals)
  signal wrc_dacs_out  : t_dacs_from_wrc;
  signal wrc_phy_out   : t_phy_8bits_from_wrc;
  signal wrc_phy_in    : t_phy_8bits_to_wrc;
  signal wrc_sfp_out   : t_sfp_from_wrc;
  signal wrc_sfp_in    : t_sfp_to_wrc;
  
begin


  U_Ext_PLL : ext_pll_10_to_125m
    port map (
      clk_ext_i        => clk_ext,
      clk_ext_mul_o    => clk_ext_mul,
      rst_a_i          => ext_pll_reset,
      clk_in_stopped_o => clk_ext_stopped,
      locked_o         => clk_ext_mul_locked);

  U_Extend_EXT_Reset : gc_extend_pulse
    generic map (
      g_width => 1000)
    port map (
      clk_i      => clk_62m5_sys,
      rst_n_i    => local_reset_n,
      pulse_i    => clk_ext_rst,
      extended_o => ext_pll_reset);


  U_Reset_Gen : spec_reset_gen
    port map (
      clk_sys_i        => clk_62m5_sys,
      rst_pcie_n_a_i   => L_RST_N,
      rst_button_n_a_i => button1_i,
      rst_n_o          => local_reset_n);

  ------------------------------------------------------------------------------
  -- Local clock from gennum LCLK
  ------------------------------------------------------------------------------
  cmp_l_clk_buf : IBUFDS
    generic map (
      DIFF_TERM    => false,            -- Differential Termination
      IBUF_LOW_PWR => true,  -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
      IOSTANDARD   => "DEFAULT")
    port map (
      O  => l_clk,                      -- Buffer output
      I  => L_CLKp,  -- Diff_p buffer input (connect directly to top-level port)
      IB => L_CLKn  -- Diff_n buffer input (connect directly to top-level port)
      );

  ------------------------------------------------------------------------------
  -- GN4124 interface
  ------------------------------------------------------------------------------
  cmp_gn4124_core : gn4124_core
    port map
    (
      ---------------------------------------------------------
      -- Control and status
      rst_n_a_i => L_RST_N,
      status_o  => open,

      ---------------------------------------------------------
      -- P2L Direction
      --
      -- Source Sync DDR related signals
      p2l_clk_p_i  => P2L_CLKp,
      p2l_clk_n_i  => P2L_CLKn,
      p2l_data_i   => P2L_DATA,
      p2l_dframe_i => P2L_DFRAME,
      p2l_valid_i  => P2L_VALID,
      -- P2L Control
      p2l_rdy_o    => P2L_RDY,
      p_wr_req_i   => P_WR_REQ,
      p_wr_rdy_o   => P_WR_RDY,
      rx_error_o   => RX_ERROR,
      vc_rdy_i     => VC_RDY,

      ---------------------------------------------------------
      -- L2P Direction
      --
      -- Source Sync DDR related signals
      l2p_clk_p_o  => L2P_CLKp,
      l2p_clk_n_o  => L2P_CLKn,
      l2p_data_o   => L2P_DATA,
      l2p_dframe_o => L2P_DFRAME,
      l2p_valid_o  => L2P_VALID,
      -- L2P Control
      l2p_edb_o    => L2P_EDB,
      l2p_rdy_i    => L2P_RDY,
      l_wr_rdy_i   => L_WR_RDY,
      p_rd_d_rdy_i => P_RD_D_RDY,
      tx_error_i   => TX_ERROR,

      ---------------------------------------------------------
      -- Interrupt interface
      dma_irq_o => open,
      irq_p_i   => '0',
      irq_p_o   => GPIO(0),

      ---------------------------------------------------------
      -- DMA registers wishbone interface (slave classic)
      dma_reg_clk_i => clk_62m5_sys,
      dma_reg_adr_i => (others=>'0'),
      dma_reg_dat_i => (others=>'0'),
      dma_reg_sel_i => (others=>'0'),
      dma_reg_stb_i => '0',
      dma_reg_we_i  => '0',
      dma_reg_cyc_i => '0',

      ---------------------------------------------------------
      -- CSR wishbone interface (master pipelined)
      csr_clk_i   => clk_62m5_sys,
      csr_adr_o   => wb_adr,
      csr_dat_o   => genum_wb_out.dat,
      csr_sel_o   => genum_wb_out.sel,
      csr_stb_o   => genum_wb_out.stb,
      csr_we_o    => genum_wb_out.we,
      csr_cyc_o   => genum_wb_out.cyc,
      csr_dat_i   => genum_wb_in.dat,
      csr_ack_i   => genum_wb_in.ack,
      csr_stall_i => genum_wb_in.stall,
      csr_err_i   => genum_wb_in.err,
      csr_rty_i   => genum_wb_in.rty,
      csr_int_i   => genum_wb_in.int,

      ---------------------------------------------------------
      -- L2P DMA Interface (Pipelined Wishbone master)
      dma_clk_i => clk_62m5_sys,
      dma_dat_i => (others=>'0'),
      dma_ack_i => '1',
      dma_stall_i => '0',
      dma_err_i => '0',
      dma_rty_i => '0',
      dma_int_i => '0');

  genum_wb_out.adr(1 downto 0)   <= (others => '0');
  genum_wb_out.adr(18 downto 2)  <= wb_adr(16 downto 0);
  genum_wb_out.adr(31 downto 19) <= (others => '0');


  fpga_scl_b <= '0' when wrc_scl_o = '0' else 'Z';
  fpga_sda_b <= '0' when wrc_sda_o = '0' else 'Z';
  wrc_scl_i  <= fpga_scl_b;
  wrc_sda_i  <= fpga_sda_b;


  U_WR_CORE : xwr_core
    generic map (
      g_simulation                => 0,
      g_with_external_clock_input => true,
      --
      g_phys_uart                 => true,
      g_virtual_uart              => true,
      g_aux_clks                  => 0,
      g_ep_rxbuf_size             => 1024,
      g_tx_runt_padding           => true,
      g_pcs_16bit                 => false,
      g_dpram_initf               => "none",
      g_aux_sdb                   => c_etherbone_sdb,
      g_dpram_size                => 131072/4,
      g_interface_mode            => PIPELINED,
      g_address_granularity       => BYTE)
    port map (
      clk_sys_i     => clk_62m5_sys,
      clk_dmtd_i    => clk_dmtd,
      clk_ref_i     => clk_125m_pllref,
      clk_aux_i     => (others => '0'),
      clk_ext_i     => clk_ext,
      clk_ext_mul_i => clk_ext_mul,
      clk_ext_mul_locked_i => clk_ext_mul_locked,
      clk_ext_stopped_i    => clk_ext_stopped,
      clk_ext_rst_o        => clk_ext_rst,
      pps_ext_i     => dio_in(3),
      rst_n_i       => local_reset_n,

      dac_hpll_load_p1_o => wrc_dacs_out.hpll_load_p1,
      dac_hpll_data_o    => wrc_dacs_out.hpll_data,
      dac_dpll_load_p1_o => wrc_dacs_out.dpll_load_p1,
      dac_dpll_data_o    => wrc_dacs_out.dpll_data,

      phy_ref_clk_i      => clk_125m_pllref,
      phy_tx_data_o      => wrc_phy_out.tx_data,
      phy_tx_k_o         => wrc_phy_out.tx_k,
      phy_tx_disparity_i => wrc_phy_in.tx_disparity,
      phy_tx_enc_err_i   => wrc_phy_in.tx_enc_err,
      phy_rx_data_i      => wrc_phy_in.rx_data,
      phy_rx_rbclk_i     => wrc_phy_in.rx_clk,
      phy_rx_k_i         => wrc_phy_in.rx_k,
      phy_rx_enc_err_i   => wrc_phy_in.rx_enc_err,
      phy_rx_bitslide_i  => wrc_phy_in.rx_bitslide,
      phy_rst_o          => wrc_phy_out.rst,

      phy_loopen_o       => wrc_phy_out.loopen,
      phy_loopen_vec_o   => wrc_phy_out.loopen_vec,
      phy_rdy_i          => wrc_phy_in.rdy,
      phy_tx_prbs_sel_o  => wrc_phy_out.tx_prbs_sel,
      phy_sfp_tx_fault_i   => wrc_phy_in.sfp_tx_fault,
      phy_sfp_los_i        => wrc_phy_in.sfp_los,
      phy_sfp_tx_disable_o => wrc_phy_out.sfp_tx_disable,

      led_act_o  => LED_RED,
      led_link_o => LED_GREEN,
      scl_o      => wrc_scl_o,
      scl_i      => wrc_scl_i,
      sda_o      => wrc_sda_o,
      sda_i      => wrc_sda_i,
      sfp_scl_o  => wrc_sfp_out.scl,
      sfp_scl_i  => wrc_sfp_in.scl,
      sfp_sda_o  => wrc_sfp_out.sda,
      sfp_sda_i  => wrc_sfp_in.sda,
      sfp_det_i  => wrc_sfp_in.det,
      btn1_i     => button1_i,
      btn2_i     => button2_i,
      spi_sclk_o  => spi_sclk_o,
      spi_ncs_o   => spi_ncs_o,
      spi_mosi_o  => spi_mosi_o,
      spi_miso_i  => spi_miso_i,

      uart_rxd_i => uart_rxd_i,
      uart_txd_o => uart_txd_o,

      owr_en_o => owr_en,
      owr_i    => owr_i,

      slave_i => wrc_slave_i,
      slave_o => wrc_slave_o,

      aux_master_o => etherbone_cfg_in,
      aux_master_i => etherbone_cfg_out,

      wrf_src_o => etherbone_snk_in,
      wrf_src_i => etherbone_snk_out,
      wrf_snk_o => etherbone_src_in,
      wrf_snk_i => etherbone_src_out,

      tm_dac_value_o       => open,
      tm_dac_wr_o          => open,
      tm_clk_aux_lock_en_i => (others => '0'),
      tm_clk_aux_locked_o  => open,
      tm_time_valid_o      => open,
      tm_tai_o             => open,
      tm_cycles_o          => open,
      pps_p_o              => pps,
      pps_led_o            => pps_led,

      rst_aux_n_o => etherbone_rst_n
      );

  WRC_PLATFORM : xwrc_platform_xilinx
    generic map (
      g_simulation => 0, 
      g_family     => "spartan6")
    port map (
      local_reset_n_i      => local_reset_n,
      --------------------------------------------------------------------------
      -- main clocks
      --------------------------------------------------------------------------
      clk_20m_vcxo_i       => clk_20m_vcxo_i,      -- 20MHz VCXO clock
      clk_125m_pllref_p_i  => clk_125m_pllref_p_i, -- 125 MHz PLL reference
      clk_125m_pllref_n_i  => clk_125m_pllref_n_i,
      clk_125m_gtp_p_i     => clk_125m_gtp_p_i,   -- 125 MHz GTP reference
      clk_125m_gtp_n_i     => clk_125m_gtp_n_i,   -- 125 MHz GTP reference
      --------------------------------------------------------------------------
      -- I2C to control DAC
      --------------------------------------------------------------------------
      dac_sclk_o           => dac_sclk_o,
      dac_din_o            => dac_din_o,
      dac_clr_n_o          => dac_clr_n_o,
      dac_cs1_n_o          => dac_cs1_n_o,
      dac_cs2_n_o          => dac_cs2_n_o,
      --------------------------------------------------------------------------
      -- one-wire access to thermometer
      --------------------------------------------------------------------------
      carrier_onewire_b    => thermo_id,
      --------------------------------------------------------------------------
      -- SFP pins
      --------------------------------------------------------------------------
      sfp_txp_o            => sfp_txp_o,
      sfp_txn_o            => sfp_txn_o,
      sfp_rxp_i            => sfp_rxp_i,
      sfp_rxn_i            => sfp_rxn_i,
      sfp_mod_def0_i       => sfp_mod_def0_i,
      sfp_mod_def1_b       => sfp_mod_def1_b,
      sfp_mod_def2_b       => sfp_mod_def2_b,
      sfp_rate_select_b    => sfp_rate_select_b,
      sfp_tx_fault_i       => sfp_tx_fault_i,
      sfp_tx_disable_o     => sfp_tx_disable_o,
      sfp_los_i            => sfp_los_i,
      --------------------------------------------------------------------------
      --Interface to WR PTP Core (WRPC)
      --------------------------------------------------------------------------
      clk_62m5_sys_o       => clk_62m5_sys,
      clk_125m_pllref_o    => clk_125m_pllref,
      clk_62m5_dmtd_o      => clk_dmtd,
      dacs_i               => wrc_dacs_out,
      phy8_o                => wrc_phy_in,
      phy8_i                => wrc_phy_out,
      owr_en_i             => owr_en,
      owr_o                => owr_i,
      sfp_config_o         => wrc_sfp_in,
      sfp_config_i         => wrc_sfp_out);

  Etherbone : eb_slave_core
    generic map (
      g_sdb_address => x"0000000000030000")
    port map (
      clk_i       => clk_62m5_sys,
      nRst_i      => etherbone_rst_n,
      src_o       => etherbone_src_out,
      src_i       => etherbone_src_in,
      snk_o       => etherbone_snk_out,
      snk_i       => etherbone_snk_in,
      cfg_slave_o => etherbone_cfg_out,
      cfg_slave_i => etherbone_cfg_in,
      master_o    => etherbone_wb_out,
      master_i    => etherbone_wb_in);

  ---------------------
  masterbar : xwb_crossbar
    generic map (
      g_num_masters => 2,
      g_num_slaves  => 1,
      g_registered  => false,
      g_address     => (0 => x"00000000"),
      g_mask        => (0 => x"00000000"))
    port map (
      clk_sys_i   => clk_62m5_sys,
      rst_n_i     => local_reset_n,
      slave_i(0)  => genum_wb_out,
      slave_i(1)  => etherbone_wb_out,
      slave_o(0)  => genum_wb_in,
      slave_o(1)  => etherbone_wb_in,
      master_i(0) => wrc_slave_o,
      master_o(0) => wrc_slave_i);

  ---------------------


  U_Extend_PPS : gc_extend_pulse
    generic map (
      g_width => 10000000)
    port map (
      clk_i      => clk_125m_pllref,
      rst_n_i    => local_reset_n,
      pulse_i    => pps_led,
      extended_o => dio_led_top_o);


  gen_dio_iobufs : for i in 0 to 4 generate
    U_ibuf : IBUFDS
      generic map (
        DIFF_TERM => true)
      port map (
        O  => dio_in(i),
        I  => dio_p_i(i),
        IB => dio_n_i(i)
        );

    U_obuf : OBUFDS
      port map (
        I  => dio_out(i),
        O  => dio_p_o(i),
        OB => dio_n_o(i)
        );
  end generate gen_dio_iobufs;

  U_input_buffer : IBUFGDS
    generic map (
      DIFF_TERM => true)
    port map (
      O  => clk_ext,
      I  => dio_clk_p_i,
      IB => dio_clk_n_i
      );

  dio_led_bot_o <= '0';

  process(clk_125m_pllref)
  begin
    if rising_edge(clk_125m_pllref) then
      clk_ref_div2 <= not clk_ref_div2;
    end if;
  end process;
  
  dio_out(0) <= pps;
  dio_out(1) <= clk_ref_div2;

  dio_oe_n_o(0)          <= '0';
  dio_oe_n_o(2 downto 1) <= (others => '0');
  dio_oe_n_o(3)          <= '1';        -- for external 1-PPS
  dio_oe_n_o(4)          <= '1';        -- for external 10MHz clock

  dio_onewire_b <= 'Z';
  dio_term_en_o <= (others => '0');

  dio_sdn_ck_n_o <= '1';
  dio_sdn_n_o    <= '1';

end rtl;


