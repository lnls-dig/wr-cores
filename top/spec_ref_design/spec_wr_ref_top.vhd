-------------------------------------------------------------------------------
-- Title      : WRPC reference design for SPEC
-- Project    : WR PTP Core
-- URL        : http://www.ohwr.org/projects/wr-cores/wiki/Wrpc_core
-------------------------------------------------------------------------------
-- File       : spec_wr_ref_top.vhd
-- Author(s)  : Grzegorz Daniluk <grzegorz.daniluk@cern.ch>
-- Company    : CERN (BE-CO-HT)
-- Created    : 2017-02-20
-- Last update: 2017-03-10
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Top-level file for the WRPC reference design on the SPEC.
--
-- This is a reference top HDL that instanciates the WR PTP Core together with
-- its peripherals to be run on a SPEC card.
--
-- There are two main usecases for this HDL file:
-- * let new users easily synthesize a WR PTP Core bitstream that can be run on
--   reference hardware
-- * provide a reference top HDL file showing how the WRPC can be instantiated
--   in HDL projects.
--
-- SPEC:  http://www.ohwr.org/projects/spec/
--
-------------------------------------------------------------------------------
-- Copyright (c) 2017 CERN
-------------------------------------------------------------------------------
-- GNU LESSER GENERAL PUBLIC LICENSE
--
-- This source file is free software; you can redistribute it
-- and/or modify it under the terms of the GNU Lesser General
-- Public License as published by the Free Software Foundation;
-- either version 2.1 of the License, or (at your option) any
-- later version.
--
-- This source is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE.  See the GNU Lesser General Public License for more
-- details.
--
-- You should have received a copy of the GNU Lesser General
-- Public License along with this source; if not, download it
-- from http://www.gnu.org/licenses/lgpl-2.1.html
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.wishbone_pkg.all;
use work.wr_board_pkg.all;
use work.wr_spec_pkg.all;
use work.gn4124_core_pkg.all;

library unisim;
use unisim.vcomponents.all;

entity spec_wr_ref_top is
  generic (
    g_dpram_initf : string := "../../bin/wrpc/wrc_phy8.bram";
    -- Simulation-mode enable parameter. Set by default (synthesis) to 0, and
    -- changed to non-zero in the instantiation of the top level DUT in the testbench.
    -- Its purpose is to reduce some internal counters/timeouts to speed up simulations.
    g_simulation : integer := 0
  );
  port (
    ---------------------------------------------------------------------------
    -- Clocks/resets
    ---------------------------------------------------------------------------

    -- Local oscillators
    clk_20m_vcxo_i : in std_logic;                -- 20MHz VCXO clock

    clk_125m_pllref_p_i : in std_logic;           -- 125 MHz PLL reference
    clk_125m_pllref_n_i : in std_logic;

    clk_125m_gtp_n_i : in std_logic;              -- 125 MHz GTP reference
    clk_125m_gtp_p_i : in std_logic;

    ---------------------------------------------------------------------------
    -- GN4124 PCIe bridge signals
    ---------------------------------------------------------------------------
    -- From GN4124 Local bus
    gn_rst_n : in std_logic; -- Reset from GN4124 (RSTOUT18_N)
    -- PCIe to Local [Inbound Data] - RX
    gn_p2l_clk_n  : in  std_logic;       -- Receiver Source Synchronous Clock-
    gn_p2l_clk_p  : in  std_logic;       -- Receiver Source Synchronous Clock+
    gn_p2l_rdy    : out std_logic;       -- Rx Buffer Full Flag
    gn_p2l_dframe : in  std_logic;       -- Receive Frame
    gn_p2l_valid  : in  std_logic;       -- Receive Data Valid
    gn_p2l_data   : in  std_logic_vector(15 downto 0);  -- Parallel receive data
    -- Inbound Buffer Request/Status
    gn_p_wr_req   : in  std_logic_vector(1 downto 0);  -- PCIe Write Request
    gn_p_wr_rdy   : out std_logic_vector(1 downto 0);  -- PCIe Write Ready
    gn_rx_error   : out std_logic;                     -- Receive Error
    -- Local to Parallel [Outbound Data] - TX
    gn_l2p_clkn   : out std_logic;       -- Transmitter Source Synchronous Clock-
    gn_l2p_clkp   : out std_logic;       -- Transmitter Source Synchronous Clock+
    gn_l2p_dframe : out std_logic;       -- Transmit Data Frame
    gn_l2p_valid  : out std_logic;       -- Transmit Data Valid
    gn_l2p_edb    : out std_logic;       -- Packet termination and discard
    gn_l2p_data   : out std_logic_vector(15 downto 0);  -- Parallel transmit data
    -- Outbound Buffer Status
    gn_l2p_rdy    : in std_logic;                     -- Tx Buffer Full Flag
    gn_l_wr_rdy   : in std_logic_vector(1 downto 0);  -- Local-to-PCIe Write
    gn_p_rd_d_rdy : in std_logic_vector(1 downto 0);  -- PCIe-to-Local Read Response Data Ready
    gn_tx_error   : in std_logic;                     -- Transmit Error
    gn_vc_rdy     : in std_logic_vector(1 downto 0);  -- Channel ready
    -- General Purpose Interface
    gn_gpio : inout std_logic_vector(1 downto 0);  -- gn_gpio[0] -> GN4124 GPIO8
                                                   -- gn_gpio[1] -> GN4124 GPIO9
    ---------------------------------------------------------------------------
    -- SPI interface to DACs
    ---------------------------------------------------------------------------

    plldac_sclk_o     : out std_logic;
    plldac_din_o      : out std_logic;
    pll25dac_cs_n_o : out std_logic; --cs1
    pll20dac_cs_n_o : out std_logic; --cs2

    ---------------------------------------------------------------------------
    -- SFP I/O for transceiver
    ---------------------------------------------------------------------------

    sfp_txp_o         : out   std_logic;
    sfp_txn_o         : out   std_logic;
    sfp_rxp_i         : in    std_logic;
    sfp_rxn_i         : in    std_logic;
    sfp_mod_def0_i    : in    std_logic;          -- sfp detect
    sfp_mod_def1_b    : inout std_logic;          -- scl
    sfp_mod_def2_b    : inout std_logic;          -- sda
    sfp_rate_select_o : out   std_logic;
    sfp_tx_fault_i    : in    std_logic;
    sfp_tx_disable_o  : out   std_logic;
    sfp_los_i         : in    std_logic;

    ---------------------------------------------------------------------------
    -- Onewire interface
    ---------------------------------------------------------------------------

    onewire_b : inout std_logic;

    ---------------------------------------------------------------------------
    -- UART
    ---------------------------------------------------------------------------

    uart_rxd_i : in  std_logic;
    uart_txd_o : out std_logic;

    ---------------------------------------------------------------------------
    -- Flash memory SPI interface
    ---------------------------------------------------------------------------

    flash_sclk_o : out std_logic;
    flash_ncs_o  : out std_logic;
    flash_mosi_o : out std_logic;
    flash_miso_i : in  std_logic;

    ---------------------------------------------------------------------------
    -- Miscellanous SPEC pins
    ---------------------------------------------------------------------------
    -- Red LED next to the SFP: blinking indicates that packets are being
    -- transferred.
    led_act_o   : out std_logic;
    -- Green LED next to the SFP: indicates if the link is up.
    led_link_o : out std_logic;

    button1_i   : in  std_logic;

    ---------------------------------------------------------------------------
    -- Digital I/O FMC Pins
    -- used in this design to output WR-aligned 1-PPS (in Slave mode) and input
    -- 10MHz & 1-PPS from external reference (in GrandMaster mode).
    ---------------------------------------------------------------------------

    -- Clock input from LEMO 5 on the mezzanine front panel. Used as 10MHz
    -- external reference input.
    dio_clk_p_i : in std_logic;
    dio_clk_n_i : in std_logic;

    -- Differential inputs, dio_p_i(N) inputs the current state of I/O (N+1) on
    -- the mezzanine front panel.
    dio_n_i : in std_logic_vector(4 downto 0);
    dio_p_i : in std_logic_vector(4 downto 0);

    -- Differential outputs. When the I/O (N+1) is configured as output (i.e. when
    -- dio_oe_n_o(N) = 0), the value of dio_p_o(N) determines the logic state
    -- of I/O (N+1) on the front panel of the mezzanine
    dio_n_o : out std_logic_vector(4 downto 0);
    dio_p_o : out std_logic_vector(4 downto 0);

    -- Output enable. When dio_oe_n_o(N) is 0, connector (N+1) on the front
    -- panel is configured as an output.
    dio_oe_n_o    : out std_logic_vector(4 downto 0);

    -- Termination enable. When dio_term_en_o(N) is 1, connector (N+1) on the front
    -- panel is 50-ohm terminated
    dio_term_en_o : out std_logic_vector(4 downto 0);

    -- Two LEDs on the mezzanine panel. Only Top one is currently used - to
    -- blink 1-PPS.
    dio_led_top_o : out std_logic;
    dio_led_bot_o : out std_logic;

    -- I2C interface for accessing FMC EEPROM. Deprecated, was used in
    -- pre-v3.0 releases to store WRPC configuration. Now we use Flash for this.
    dio_scl_b : inout std_logic;
    dio_sda_b : inout std_logic

  );
end entity spec_wr_ref_top;

architecture top of spec_wr_ref_top is

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------

  -- Number of masters on the wishbone crossbar
  constant c_NUM_WB_MASTERS : integer := 2;

  -- Number of slaves on the primary wishbone crossbar
  constant c_NUM_WB_SLAVES : integer := 1;

  -- Primary Wishbone master(s) offsets
  constant c_WB_MASTER_PCIE    : integer := 0;
  constant c_WB_MASTER_ETHBONE : integer := 1;

  -- Primary Wishbone slave(s) offsets
  constant c_WB_SLAVE_WRC : integer := 0;

  -- sdb header address on primary crossbar
  constant c_SDB_ADDRESS : t_wishbone_address := x"00040000";

  -- f_xwb_bridge_manual_sdb(size, sdb_addr)
  -- Note: sdb_addr is the sdb records address relative to the bridge base address
  constant c_wrc_bridge_sdb : t_sdb_bridge :=
    f_xwb_bridge_manual_sdb(x"0003ffff", x"00030000");

  -- Primary wishbone crossbar layout
  constant c_WB_LAYOUT : t_sdb_record_array(c_NUM_WB_SLAVES - 1 downto 0) := (
    c_WB_SLAVE_WRC => f_sdb_embed_bridge(c_wrc_bridge_sdb, x"00000000"));

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  -- Wishbone buse(s) from masters attached to crossbar
  signal cnx_master_out : t_wishbone_master_out_array(c_NUM_WB_MASTERS-1 downto 0);
  signal cnx_master_in  : t_wishbone_master_in_array(c_NUM_WB_MASTERS-1 downto 0);

  -- Wishbone buse(s) to slaves attached to crossbar
  signal cnx_slave_out : t_wishbone_slave_out_array(c_NUM_WB_SLAVES-1 downto 0);
  signal cnx_slave_in  : t_wishbone_slave_in_array(c_NUM_WB_SLAVES-1 downto 0);

  -- Gennum signals
  signal gn_wbadr : std_logic_vector(31 downto 0);

  -- clock and reset
  signal clk_sys_62m5   : std_logic;
  signal rst_sys_62m5_n : std_logic;
  signal rst_ref_125m_n : std_logic;
  signal clk_ref_125m   : std_logic;
  signal clk_ref_div2   : std_logic;
  signal clk_ext_10m    : std_logic;

  -- I2C EEPROM
  signal eeprom_sda_in  : std_logic;
  signal eeprom_sda_out : std_logic;
  signal eeprom_scl_in  : std_logic;
  signal eeprom_scl_out : std_logic;

  -- SFP
  signal sfp_sda_in  : std_logic;
  signal sfp_sda_out : std_logic;
  signal sfp_scl_in  : std_logic;
  signal sfp_scl_out : std_logic;

  -- OneWire
  signal onewire_data : std_logic;
  signal onewire_oe   : std_logic;

  -- LEDs and GPIO
  signal wrc_abscal_txts_out : std_logic;
  signal wrc_abscal_rxts_out : std_logic;
  signal wrc_pps_out : std_logic;
  signal wrc_pps_led : std_logic;
  signal wrc_pps_in  : std_logic;
  signal svec_led    : std_logic_vector(15 downto 0);

  -- DIO Mezzanine
  signal dio_in  : std_logic_vector(4 downto 0);
  signal dio_out : std_logic_vector(4 downto 0);

begin  -- architecture top

  -----------------------------------------------------------------------------
  -- Primary wishbone Crossbar
  -----------------------------------------------------------------------------

  cmp_sdb_crossbar : xwb_sdb_crossbar
    generic map (
      g_num_masters => c_NUM_WB_MASTERS,
      g_num_slaves  => c_NUM_WB_SLAVES,
      g_registered  => TRUE,
      g_wraparound  => TRUE,
      g_layout      => c_WB_LAYOUT,
      g_sdb_addr    => c_SDB_ADDRESS)
    port map (
      clk_sys_i => clk_sys_62m5,
      rst_n_i   => rst_sys_62m5_n,
      slave_i   => cnx_master_out,
      slave_o   => cnx_master_in,
      master_i  => cnx_slave_out,
      master_o  => cnx_slave_in);

  -----------------------------------------------------------------------------
  -- GN4124, PCIe bridge core
  -----------------------------------------------------------------------------
  cmp_gn4124_core : gn4124_core
    port map (
      ---------------------------------------------------------
      -- Control and status
      rst_n_a_i => gn_rst_n,
      status_o  => open,

      ---------------------------------------------------------
      -- P2L Direction
      --
      -- Source Sync DDR related signals
      p2l_clk_p_i  => gn_p2l_clk_p,
      p2l_clk_n_i  => gn_p2l_clk_n,
      p2l_data_i   => gn_p2l_data,
      p2l_dframe_i => gn_p2l_dframe,
      p2l_valid_i  => gn_p2l_valid,
      -- P2L Control
      p2l_rdy_o    => gn_p2l_rdy,
      p_wr_req_i   => gn_p_wr_req,
      p_wr_rdy_o   => gn_p_wr_rdy,
      rx_error_o   => gn_rx_error,
      vc_rdy_i     => gn_vc_rdy,

      ---------------------------------------------------------
      -- L2P Direction
      --
      -- Source Sync DDR related signals
      l2p_clk_p_o  => gn_l2p_clkp,
      l2p_clk_n_o  => gn_l2p_clkn,
      l2p_data_o   => gn_l2p_data,
      l2p_dframe_o => gn_l2p_dframe,
      l2p_valid_o  => gn_l2p_valid,
      -- L2P Control
      l2p_edb_o    => gn_l2p_edb,
      l2p_rdy_i    => gn_l2p_rdy,
      l_wr_rdy_i   => gn_l_wr_rdy,
      p_rd_d_rdy_i => gn_p_rd_d_rdy,
      tx_error_i   => gn_tx_error,

      ---------------------------------------------------------
      -- Interrupt interface
      dma_irq_o => open,
      irq_p_i   => '0',
      irq_p_o   => gn_gpio(0),

      ---------------------------------------------------------
      -- DMA registers wishbone interface (slave classic)
      dma_reg_clk_i => clk_sys_62m5,
      dma_reg_adr_i => (others=>'0'),
      dma_reg_dat_i => (others=>'0'),
      dma_reg_sel_i => (others=>'0'),
      dma_reg_stb_i => '0',
      dma_reg_we_i  => '0',
      dma_reg_cyc_i => '0',

      ---------------------------------------------------------
      -- CSR wishbone interface (master pipelined)
      csr_clk_i   => clk_sys_62m5,
      csr_adr_o   => gn_wbadr,
      csr_dat_o   => cnx_master_out(c_WB_MASTER_PCIE).dat,
      csr_sel_o   => cnx_master_out(c_WB_MASTER_PCIE).sel,
      csr_stb_o   => cnx_master_out(c_WB_MASTER_PCIE).stb,
      csr_we_o    => cnx_master_out(c_WB_MASTER_PCIE).we,
      csr_cyc_o   => cnx_master_out(c_WB_MASTER_PCIE).cyc,
      csr_dat_i   => cnx_master_in(c_WB_MASTER_PCIE).dat,
      csr_ack_i   => cnx_master_in(c_WB_MASTER_PCIE).ack,
      csr_stall_i => cnx_master_in(c_WB_MASTER_PCIE).stall,
      csr_err_i   => cnx_master_in(c_WB_MASTER_PCIE).err,
      csr_rty_i   => cnx_master_in(c_WB_MASTER_PCIE).rty,
      csr_int_i   => cnx_master_in(c_WB_MASTER_PCIE).int,

      ---------------------------------------------------------
      -- L2P DMA Interface (Pipelined Wishbone master)
      dma_clk_i   => clk_sys_62m5,
      dma_dat_i   => (others=>'0'),
      dma_ack_i   => '1',
      dma_stall_i => '0',
      dma_err_i   => '0',
      dma_rty_i   => '0',
      dma_int_i   => '0');

  -- "translating" word addressing of Gennum module into byte addressing
  cnx_master_out(c_WB_MASTER_PCIE).adr(1 downto 0)   <= (others => '0');
  cnx_master_out(c_WB_MASTER_PCIE).adr(18 downto 2)  <= gn_wbadr(16 downto 0);
  cnx_master_out(c_WB_MASTER_PCIE).adr(31 downto 19) <= (others => '0');


  -----------------------------------------------------------------------------
  -- The WR PTP core board package (WB Slave + WB Master #2 (Etherbone))
  -----------------------------------------------------------------------------

  cmp_xwrc_board_spec : xwrc_board_spec
    generic map (
      g_simulation                => g_simulation,
      g_with_external_clock_input => TRUE,
      g_dpram_initf               => g_dpram_initf,
      g_fabric_iface              => ETHERBONE)
    port map (
      areset_n_i          => button1_i,
      areset_edge_n_i     => gn_rst_n,
      clk_20m_vcxo_i      => clk_20m_vcxo_i,
      clk_125m_pllref_p_i => clk_125m_pllref_p_i,
      clk_125m_pllref_n_i => clk_125m_pllref_n_i,
      clk_125m_gtp_n_i    => clk_125m_gtp_n_i,
      clk_125m_gtp_p_i    => clk_125m_gtp_p_i,
      clk_10m_ext_i       => clk_ext_10m,
      clk_sys_62m5_o      => clk_sys_62m5,
      clk_ref_125m_o      => clk_ref_125m,
      rst_sys_62m5_n_o    => rst_sys_62m5_n,
      rst_ref_125m_n_o    => rst_ref_125m_n,

      plldac_sclk_o       => plldac_sclk_o,
      plldac_din_o        => plldac_din_o,
      pll25dac_cs_n_o     => pll25dac_cs_n_o,
      pll20dac_cs_n_o     => pll20dac_cs_n_o,

      sfp_txp_o           => sfp_txp_o,
      sfp_txn_o           => sfp_txn_o,
      sfp_rxp_i           => sfp_rxp_i,
      sfp_rxn_i           => sfp_rxn_i,
      sfp_det_i           => sfp_mod_def0_i,
      sfp_sda_i           => sfp_sda_in,
      sfp_sda_o           => sfp_sda_out,
      sfp_scl_i           => sfp_scl_in,
      sfp_scl_o           => sfp_scl_out,
      sfp_rate_select_o   => sfp_rate_select_o,
      sfp_tx_fault_i      => sfp_tx_fault_i,
      sfp_tx_disable_o    => sfp_tx_disable_o,
      sfp_los_i           => sfp_los_i,

      eeprom_sda_i        => eeprom_sda_in,
      eeprom_sda_o        => eeprom_sda_out,
      eeprom_scl_i        => eeprom_scl_in,
      eeprom_scl_o        => eeprom_scl_out,

      onewire_i           => onewire_data,
      onewire_oen_o       => onewire_oe,
      -- Uart
      uart_rxd_i          => uart_rxd_i,
      uart_txd_o          => uart_txd_o,
      -- SPI Flash
      flash_sclk_o        => flash_sclk_o,
      flash_ncs_o         => flash_ncs_o,
      flash_mosi_o        => flash_mosi_o,
      flash_miso_i        => flash_miso_i,

      wb_slave_o          => cnx_slave_out(c_WB_SLAVE_WRC),
      wb_slave_i          => cnx_slave_in(c_WB_SLAVE_WRC),

      wb_eth_master_o     => cnx_master_out(c_WB_MASTER_ETHBONE),
      wb_eth_master_i     => cnx_master_in(c_WB_MASTER_ETHBONE),
      
      abscal_txts_o       => wrc_abscal_txts_out,
      abscal_rxts_o       => wrc_abscal_rxts_out,

      pps_ext_i           => wrc_pps_in,
      pps_p_o             => wrc_pps_out,
      pps_led_o           => wrc_pps_led,
      led_link_o          => led_link_o,
      led_act_o           => led_act_o);

  -- Tristates for SFP EEPROM
  sfp_mod_def1_b <= '0' when sfp_scl_out = '0' else 'Z';
  sfp_mod_def2_b <= '0' when sfp_sda_out = '0' else 'Z';
  sfp_scl_in     <= sfp_mod_def1_b;
  sfp_sda_in     <= sfp_mod_def2_b;

  -- tri-state onewire access
  onewire_b    <= '0' when (onewire_oe = '1') else 'Z';
  onewire_data <= onewire_b;

  ------------------------------------------------------------------------------
  -- Digital I/O FMC Mezzanine connections
  ------------------------------------------------------------------------------
  gen_dio_iobufs: for I in 0 to 4 generate
    U_ibuf: IBUFDS
      generic map (
        DIFF_TERM => true)
      port map (
        O  => dio_in(i),
        I  => dio_p_i(i),
        IB => dio_n_i(i));

    U_obuf : OBUFDS
      port map (
        I  => dio_out(i),
        O  => dio_p_o(i),
        OB => dio_n_o(i));
  end generate;
  -- Configure Digital I/Os 0 to 3 as outputs
  dio_oe_n_o(2 downto 0) <= (others => '0');
  -- Configure Digital I/Os 3 and 4 as inputs for external reference
  dio_oe_n_o(3)          <= '1';  -- for external 1-PPS
  dio_oe_n_o(4)          <= '1';  -- for external 10MHz clock
  -- All DIO connectors are not terminated
  dio_term_en_o          <= (others => '0');

  -- EEPROM I2C tri-states
  dio_sda_b <= '0' when (eeprom_sda_out = '0') else 'Z';
  eeprom_sda_in <= dio_sda_b;
  dio_scl_b <= '0' when (eeprom_scl_out = '0') else 'Z';
  eeprom_scl_in <= dio_scl_b;

  -- Div by 2 reference clock to LEMO connector
  process(clk_ref_125m)
  begin
    if rising_edge(clk_ref_125m) then
      clk_ref_div2 <= not clk_ref_div2;
    end if;
  end process;

  cmp_ibugds_extref: IBUFGDS
    generic map (
      DIFF_TERM => true)
    port map (
      O  => clk_ext_10m,
      I  => dio_clk_p_i,
      IB => dio_clk_n_i);

  wrc_pps_in    <= dio_in(3);
  dio_out(0)    <= wrc_pps_out;
  dio_out(1)    <= wrc_abscal_rxts_out;
  dio_out(2)    <= wrc_abscal_txts_out;

  -- LEDs
  U_Extend_PPS : gc_extend_pulse
  generic map (
    g_width => 10000000)
  port map (
    clk_i      => clk_ref_125m,
    rst_n_i    => rst_ref_125m_n,
    pulse_i    => wrc_pps_led,
    extended_o => dio_led_top_o);

  dio_led_bot_o <= '0';

end architecture top;
