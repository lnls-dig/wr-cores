-------------------------------------------------------------------------------
-- Title      : WhiteRabbit PTP Core SPEC demo
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : spec_top.vhd
-- Author     : Grzegorz Daniluk
-- Company    : CERN
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- This is a reference top HDL that instanciates WR PTP Core together with its
-- peripherals to be run on the SPEC PCIe card. There are two main usecases for
-- this HDL file:
-- * let new users easily synthesize WR PTP Core bitstream that can be run on a
--   reference hardware (SPEC PCIe card)
-- * provide a reference top HDL file showing how the WRPC can be instantiated
--   in HDL projects.
-- 
-------------------------------------------------------------------------------
--
-- Copyright (c) 2011-2016 CERN
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

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

---------------------------------------------------------------------------
-- Basic packages needed for instantiating the WR PTP Core
---------------------------------------------------------------------------
-- Use the General Cores package (for gc_extend_pulse)
use work.gencores_pkg.all;
-- Use the WR Core package, with xwr_core component defined inside.
use work.wrcore_pkg.all;
-- Use the Xilinx White Rabbit platform-specific package (for xwrc_platform_xilinx)
use work.wr_xilinx_pkg.all;
-- Use the Endpoint package inside the WR PTP Core for definitions of
-- record-based PHY interfaces phy8_o, phy8_i
use work.endpoint_pkg.all;

---------------------------------------------------------------------------
-- Additional packages needed for other HDL modules in this design
---------------------------------------------------------------------------
-- Use the Gennum GN4124 package for PCIe module (gn4124_core)
use work.gn4124_core_pkg.all;
-- Use the package for Etherbone module (eb_slave_core)
use work.etherbone_pkg.all;
-- Use the WR Fabric package for definitions of the WRPC frame interface used
-- for Etherbone
use work.wr_fabric_pkg.all;
-- Use the Wishbone package for definitions of Wishbone interfaces used to
-- control the WRPC from PCIe and Etherbone cores.
use work.wishbone_pkg.all;

---------------------------------------------------------------------------
-- Simulation packages
---------------------------------------------------------------------------
-- Use library UNISIM for PLL_BASE, IBUFGDS and BUFG simulation components.
library UNISIM;
use UNISIM.vcomponents.all;


entity spec_top is
  generic (
    -- Simulation mode enable parameter. Set by default (synthesis) to 0, and
    -- changed to non-zero in the instantiation of the top level DUT in the testbench.
    -- Its purpose is to reduce some internal counters/timeouts to speed up simulations.
    g_simulation : integer := 0
  );
  port (
    ---------------------------------------------------------------------------
    -- Clock signals
    ---------------------------------------------------------------------------
    -- Clock input, used to derive the DDMTD clock (check out the general presentation
    -- of WR for explanation of its purpose). The clock is produced by the
    -- other VCXO, tuned by the second AD5662 DAC, (which is connected to
    -- dac_helper output of the WR Core)
      clk_20m_vcxo_i : in std_logic;

    -- Clock input: 125 MHz LVDS reference clock, coming from the CDCM61004
    -- PLL. The reference oscillator is a 25 MHz VCTCXO (VM53S), tunable by the
    -- DAC connected to CS0 SPI line (dac_main output of the WR Core).
      clk_125m_pllref_p_i : in std_logic;
      clk_125m_pllref_n_i : in std_logic;

    -- Dedicated clock for the Xilinx GTP transceiver. Same physical clock as
    -- clk_125m_pllref, just coming from another output of CDCM61004 PLL.
      clk_125m_gtp_p_i : in std_logic;
      clk_125m_gtp_n_i : in std_logic;

    ---------------------------------------------------------------------------
    -- GN4124 PCIe bridge signals
    ---------------------------------------------------------------------------
    -- From GN4124 Local bus
      L_RST_N : in std_logic; -- Reset from GN4124 (RSTOUT18_N)

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
      L2P_CLKn   : out std_logic;       -- Transmitter Source Synchronous Clock-
      L2P_CLKp   : out std_logic;       -- Transmitter Source Synchronous Clock+
      L2P_EDB    : out std_logic;       -- Packet termination and discard

    -- Outbound Buffer Status
      L2P_RDY    : in std_logic;                     -- Tx Buffer Full Flag
      L_WR_RDY   : in std_logic_vector(1 downto 0);  -- Local-to-PCIe Write
      P_RD_D_RDY : in std_logic_vector(1 downto 0);  -- PCIe-to-Local Read Response Data Ready
      TX_ERROR   : in std_logic;                     -- Transmit Error
      VC_RDY     : in std_logic_vector(1 downto 0);  -- Channel ready

    ---------------------------------------------------------------------------
    -- Oscillator control pins
    ---------------------------------------------------------------------------
    -- A typical SPI bus shared betwen two AD5662 DACs. The first one (CS1) tunes
    -- the clk_ref oscillator, the second (CS2) - the clk_dmtd VCXO.
      dac_sclk_o  : out std_logic;
      dac_din_o   : out std_logic;
      dac_cs1_n_o : out std_logic;
      dac_cs2_n_o : out std_logic;

    ---------------------------------------------------------------------------
    -- Flash memory SPI interface
    ---------------------------------------------------------------------------
    -- Standard SPI interface for M24P32 Flash memory on SPEC. Used for storing
    -- SFP database, configuration of the WRPC, etc.
      spi_sclk_o : out std_logic;
      spi_ncs_o  : out std_logic;
      spi_mosi_o : out std_logic;
      spi_miso_i : in  std_logic := 'L';

    ---------------------------------------------------------------------------
    -- SFP pins
    ---------------------------------------------------------------------------
    -- TX gigabit output
      sfp_txp_o : out std_logic;
      sfp_txn_o : out std_logic;
    -- RX gigabit input
      sfp_rxp_i : in std_logic;
      sfp_rxn_i : in std_logic;

    -- SFP MOD_DEF0 pin (used as a tied-to-ground SFP insertion detect line)
      sfp_mod_def0_i    : in    std_logic;
    -- SFP MOD_DEF1 pin (SCL line of the I2C EEPROM inside the SFP)
      sfp_mod_def1_b    : inout std_logic;
    -- SFP MOD_DEF2 pin (SDA line of the I2C EEPROM inside the SFP)
      sfp_mod_def2_b    : inout std_logic;
    -- SFP RATE_SELECT pin. Unused for most SFPs, in our case tied to 0.
      sfp_rate_select_b : inout std_logic;
    -- SFP laser fault detection pin. Unused in our design.
      sfp_tx_fault_i    : in    std_logic;
    -- SFP laser disable line.
      sfp_tx_disable_o  : out   std_logic;
    -- SFP-provided loss-of-link detection. We don't use it as Ethernet PCS
    -- has its own loss-of-sync detection mechanism.
      sfp_los_i         : in    std_logic;

    ---------------------------------------------------------------------------
    -- Miscellanous WR Core pins
    ---------------------------------------------------------------------------
    -- Red LED next to the SFP: blinking indicates that packets are being
    -- transferred.
      led_red_o   : out std_logic;
    -- Green LED next to the SFP: indicates if the link is up.
      led_green_o : out std_logic;

    -- Buttons on the SPEC card. In our case, button1 is used as an external
    -- reset trigger.
      button1_i : in std_logic := 'H';
      button2_i : in std_logic := 'H';
      
    -- One-wire interface to DS18B20 temperature sensor, which also provides an
    -- unique serial number, that WRPC can use to assign itself a unique MAC address.
      thermo_id : inout std_logic;

    -- UART pins (connected to the mini-USB port)
      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic;

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
      fpga_scl_b : inout std_logic;
      fpga_sda_b : inout std_logic
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
  -- Signals declaration
  ------------------------------------------------------------------------------

  -- WRPC: clock signals
  signal clk_125m_pllref  : std_logic;
  signal clk_62m5_sys          : std_logic;
  signal clk_dmtd         : std_logic;

  -- WRPC: I2C signals for EEPROM. Deprecated, now we use SPI Flash
  signal wrc_scl_out : std_logic;
  signal wrc_scl_in  : std_logic;
  signal wrc_sda_out : std_logic;
  signal wrc_sda_in  : std_logic;

  -- WRPC: 1-PPS signals (for the connector and blinking LED)
  signal pps     : std_logic;
  signal pps_led : std_logic;

  -- Digital I/O mezzanine wiring
  signal dio_in  : std_logic_vector(4 downto 0);
  signal dio_out : std_logic_vector(4 downto 0);

  -- Main system reset signal
  signal local_reset_n  : std_logic;

  -- GN4124: wishbone signals
  signal genum_wb_out    : t_wishbone_master_out;
  signal genum_wb_in     : t_wishbone_master_in;
  signal wb_adr          : std_logic_vector(31 downto 0);

  -- WRPC: wishbone signals
  signal wrc_slave_i : t_wishbone_slave_in;
  signal wrc_slave_o : t_wishbone_slave_out;

  -- WRPC: 1-Wire signals for DS18B20 temperature sensor
  signal wrc_owr_en : std_logic_vector(1 downto 0);
  signal wrc_owr_in : std_logic_vector(1 downto 0);

  -- Etherbone core signals
  signal etherbone_rst_n   : std_logic;
  signal etherbone_src_out : t_wrf_source_out;
  signal etherbone_src_in  : t_wrf_source_in;
  signal etherbone_snk_out : t_wrf_sink_out;
  signal etherbone_snk_in  : t_wrf_sink_in;
  signal etherbone_wb_out  : t_wishbone_master_out;
  signal etherbone_wb_in   : t_wishbone_master_in;
  signal etherbone_cfg_in  : t_wishbone_slave_in;
  signal etherbone_cfg_out : t_wishbone_slave_out;

  -- WRPC <--> WRP (WR PTP Core Platform-dependent peripherals)
  signal wrc_dacs_out  : t_dacs_from_wrc;
  signal wrc_phy_out   : t_phy_8bits_from_wrc;
  signal wrc_phy_in    : t_phy_8bits_to_wrc;
  signal wrc_sfp_out   : t_sfp_from_wrc;
  signal wrc_sfp_in    : t_sfp_to_wrc;
  signal wrc_extref_in : t_extref_to_wrc;
  signal wrc_extref_rst: std_logic;

  -- 125MHz WR clock divided by 2 and output to LEMO connector for debugging
  signal clk_125m_div2               : std_logic;
  
begin

  ------------------------------------------------------------------------------
  -- Reset generation
  ------------------------------------------------------------------------------
  U_Reset_Gen : spec_reset_gen
    port map (
      clk_sys_i        => clk_62m5_sys,
      rst_pcie_n_a_i   => L_RST_N,
      rst_button_n_a_i => button1_i,
      rst_n_o          => local_reset_n);

  ------------------------------------------------------------------------------
  -- GN4124, PCIe bridge core
  ------------------------------------------------------------------------------
  cmp_gn4124_core : gn4124_core
    port map (
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
      dma_clk_i   => clk_62m5_sys,
      dma_dat_i   => (others=>'0'),
      dma_ack_i   => '1',
      dma_stall_i => '0',
      dma_err_i   => '0',
      dma_rty_i   => '0',
      dma_int_i   => '0');

  -- "translating" word addressing of Gennum module into byte addressing
  genum_wb_out.adr(1 downto 0)   <= (others => '0');
  genum_wb_out.adr(18 downto 2)  <= wb_adr(16 downto 0);
  genum_wb_out.adr(31 downto 19) <= (others => '0');


  ------------------------------------------------------------------------------
  -- WR PTP Core - the main module of this demo design.
  ------------------------------------------------------------------------------
  U_WR_CORE : xwr_core
    generic map (
      g_simulation                => g_simulation,
      g_with_external_clock_input => true,
      --
      g_phys_uart                 => true,
      g_virtual_uart              => true,
      g_aux_clks                  => 0,
      g_ep_rxbuf_size             => 1024,
      g_tx_runt_padding           => true,
      g_pcs_16bit                 => false,
      g_records_for_phy           => true,
      g_dpram_initf               => "wrc.bram",
      g_aux_sdb                   => c_etherbone_sdb,
      g_dpram_size                => 131072/4,
      g_interface_mode            => PIPELINED,
      g_address_granularity       => BYTE)
    port map (
      -- Main clocks and reset
      clk_sys_i     => clk_62m5_sys,
      clk_dmtd_i    => clk_dmtd,
      clk_ref_i     => clk_125m_pllref,
      rst_n_i       => local_reset_n,

      -- External reference (10MHz & 1-PPS)
      clk_ext_i            => wrc_extref_in.clk_10m_ref,
      clk_ext_mul_i        => wrc_extref_in.clk_125m_ref,
      clk_ext_mul_locked_i => wrc_extref_in.locked,
      clk_ext_stopped_i    => wrc_extref_in.stopped,
      clk_ext_rst_o        => wrc_extref_rst,
      pps_ext_i            => wrc_extref_in.pps,

      -- Oscillator control DACs connections
      dac_hpll_load_p1_o => wrc_dacs_out.hpll_load_p1,
      dac_hpll_data_o    => wrc_dacs_out.hpll_data,
      dac_dpll_load_p1_o => wrc_dacs_out.dpll_load_p1,
      dac_dpll_data_o    => wrc_dacs_out.dpll_data,

      -- PHY (SerDes) connections
      phy8_o => wrc_phy_out,
      phy8_i => wrc_phy_in,

      -- Timecode & 1-PPS interface
      tm_dac_value_o       => open,
      tm_dac_wr_o          => open,
      tm_clk_aux_lock_en_i => (others => '0'),
      tm_clk_aux_locked_o  => open,
      tm_time_valid_o      => open,
      tm_tai_o             => open,
      tm_cycles_o          => open,
      -- 1-PPS output, one signal goes to the LEMO connector, another to LED
      pps_p_o              => pps,
      pps_led_o            => pps_led,

      -- Miscellanous pins
      --- Link and Activity LEDs
      led_act_o  => led_red_o,
      led_link_o => led_green_o,
      --- UART
      uart_rxd_i => uart_rxd_i,
      uart_txd_o => uart_txd_o,
      --- SFP identification I2C and SFP insertion detect
      sfp_scl_o  => wrc_sfp_out.scl,
      sfp_scl_i  => wrc_sfp_in.scl,
      sfp_sda_o  => wrc_sfp_out.sda,
      sfp_sda_i  => wrc_sfp_in.sda,
      sfp_det_i  => wrc_sfp_in.det,
      --- SPI for external Flash (stores configuration of the core)
      spi_sclk_o => spi_sclk_o,
      spi_ncs_o  => spi_ncs_o,
      spi_mosi_o => spi_mosi_o,
      spi_miso_i => spi_miso_i,
      --- 1-Wire for DS18B20 temperature sensor
      owr_en_o   => wrc_owr_en,
      owr_i      => wrc_owr_in,
      --- Buttons available on SPEC board
      btn1_i     => button1_i,
      btn2_i     => button2_i,
      --- Deprecated I2C for FMC EEPROM
      scl_o      => wrc_scl_out,
      scl_i      => wrc_scl_in,
      sda_o      => wrc_sda_out,
      sda_i      => wrc_sda_in,

      -- Wishbone Slave interface to control WRPC from PCIe and Etherbone
      slave_i => wrc_slave_i,
      slave_o => wrc_slave_o,

      -- WR Fabric interface, connected to Etherbone Core
      wrf_src_o    => etherbone_snk_in,
      wrf_src_i    => etherbone_snk_out,
      wrf_snk_o    => etherbone_src_in,
      wrf_snk_i    => etherbone_src_out,

      -- Wishbone Master interface to configure Etherbone core (e.g. IP address)
      aux_master_o => etherbone_cfg_in,
      aux_master_i => etherbone_cfg_out,
      -- Aux reset signal for Etherbone core
      rst_aux_n_o  => etherbone_rst_n
    );

  ------------------------------------------------------------------------------
  -- WRC_Platform : contains all platform specific modules (SerDes, PLLs,
  -- Buffers) needed for WR PTP Core.
  ------------------------------------------------------------------------------
  WRC_PLATFORM : xwrc_platform_xilinx
    generic map (
      g_simulation     => g_simulation, 
      g_family         => "spartan6",
      g_with_10m_refin => 1)
    port map (
      local_reset_n_i      => local_reset_n,

      -- main clocks
      clk_20m_vcxo_i       => clk_20m_vcxo_i,      -- 20MHz VCXO clock
      clk_125m_pllref_p_i  => clk_125m_pllref_p_i, -- 125 MHz PLL reference
      clk_125m_pllref_n_i  => clk_125m_pllref_n_i,
      clk_125m_gtp_p_i     => clk_125m_gtp_p_i,   -- 125 MHz GTP reference
      clk_125m_gtp_n_i     => clk_125m_gtp_n_i,   -- 125 MHz GTP reference

      clk_10m_ref_p_i      => dio_clk_p_i,
      clk_10m_ref_n_i      => dio_clk_n_i,
      pps_ext_i            => dio_in(3),

      -- I2C to control DAC
      dac_sclk_o           => dac_sclk_o,
      dac_din_o            => dac_din_o,
      dac_cs1_n_o          => dac_cs1_n_o,
      dac_cs2_n_o          => dac_cs2_n_o,

      -- 1-wire access to thermometer
      carrier_onewire_b    => thermo_id,

      -- SFP pins
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

      -- Record-based interface to WR PTP Core (WRPC)
      clk_62m5_sys_o       => clk_62m5_sys,
      clk_125m_pllref_o    => clk_125m_pllref,
      clk_62m5_dmtd_o      => clk_dmtd,
      dacs_i               => wrc_dacs_out,
      phy8_o               => wrc_phy_in,
      phy8_i               => wrc_phy_out,
      owr_en_i             => wrc_owr_en,
      owr_o                => wrc_owr_in,
      sfp_config_o         => wrc_sfp_in,
      sfp_config_i         => wrc_sfp_out,
      ext_ref_o            => wrc_extref_in,
      ext_ref_rst_i        => wrc_extref_rst);

  -----------------------------------------------------------------------------
  -- Etherbone Core
  -----------------------------------------------------------------------------
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

  -----------------------------------------------------------------------------
  -- Wishbone crossbar
  -- connects two WB Masters (GN4124 PCIe and Etherbone) to the WRPC
  -----------------------------------------------------------------------------
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

  -----------------------------------------------------------------------------
  -- LEDs on the Digital I/O Mezzanine
  -----------------------------------------------------------------------------
  U_Extend_PPS : gc_extend_pulse
    generic map (
      g_width => 10000000)
    port map (
      clk_i      => clk_125m_pllref,
      rst_n_i    => local_reset_n,
      pulse_i    => pps_led,
      extended_o => dio_led_top_o);

  dio_led_bot_o <= '0';

  -----------------------------------------------------------------------------
  -- Differential buffers for the Digital I/O Mezzanine
  -----------------------------------------------------------------------------
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


  -- Debug: Reference clock to LEMO connector
  process(clk_125m_pllref)
  begin
    if rising_edge(clk_125m_pllref) then
      clk_125m_div2 <= not clk_125m_div2;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Deprecated I2C interface for accessing FMC EEPROM.
  -----------------------------------------------------------------------------
  -- Leave it here because in software this option can still be selected.
  -- However, the new, preferred method is to use SPI Flash on the carrier (SPEC
  -- board) to store configuration parameters.
  fpga_scl_b <= '0' when wrc_scl_out = '0' else 'Z';
  fpga_sda_b <= '0' when wrc_sda_out = '0' else 'Z';
  wrc_scl_in  <= fpga_scl_b;
  wrc_sda_in  <= fpga_sda_b;
  
  -----------------------------------------------------------------------------
  -- Inputs/Outputs of the Digital I/O Mezzanine
  -----------------------------------------------------------------------------
  -- Connect the PPS output to the I/O 1 of the Digital I/O mezzanine
  dio_out(0) <= pps;
  dio_out(1) <= clk_125m_div2;

  -- Configure Digital I/Os 0 to 3 as outputs
  dio_oe_n_o(2 downto 0) <= (others => '0');
  -- Configure Digital I/Os 3 and 4 as inputs for external reference
  dio_oe_n_o(3)          <= '1';  -- for external 1-PPS
  dio_oe_n_o(4)          <= '1';  -- for external 10MHz clock

  -- All DIO connectors are not terminated
  dio_term_en_o <= (others => '0');

end rtl;
