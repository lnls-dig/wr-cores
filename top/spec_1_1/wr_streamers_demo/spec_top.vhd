--
-- White Rabbit Core Hands-On Course
--
-- Lesson 04a: Trivial streamer demo
--
-- Objectives:
-- - Show how to use streamer cores and WR timing interface.
-- - Measure packet latency
--
-- Brief description:
-- This firmware demonstrates a simple trigger distribution system. A pulse coming
-- to one of the cards is time tagged, the time tag is sent over WR network and
-- used by the receiver to reproduce the pulse with fixed delay. 
-- 
-- DIO Mezzanine connector assignment is:
-- I/O 1 - PPS output
-- I/O 2 - trigger pulse input
-- I/O 3 - recovered pulse output

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Use library UNISIM for PLL_BASE, IBUFGDS and BUFG simulation components.
library UNISIM;
use UNISIM.vcomponents.all;

-- Use the WR Core package, with xwr_core component defined inside.
use work.wrcore_pkg.all;
-- Use the General Cores package (for gc_extend_pulse)
use work.gencores_pkg.all;
-- Use the Xilinx White Rabbit platform-specific package (for wr_gtp_phy_spartan6)
use work.wr_xilinx_pkg.all;
-- Use the WR Fabric interface package For WR Fabric interface type definitions
use work.wr_fabric_pkg.all;
-- Use the streamers package for streamer component declarations
use work.streamers_pkg.all;
-- -- needed for c_etherbone_sdb
-- use work.etherbone_pkg.all;
-- needed for PIPELINED
use work.wishbone_pkg.all;

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

    -- Clock input: 125 MHz LVDS reference clock, coming from the CDCM61004
    -- PLL. The reference oscillator is a 25 MHz VCTCXO (VM53S), tunable by the
    -- DAC connected to CS0 SPI line (dac_main output of the WR Core).
    clk_125m_pllref_p_i : in std_logic;
    clk_125m_pllref_n_i : in std_logic;

    -- Dedicated clock for the Xilinx GTP transceiver. Same physical clock as
    -- clk_125m_pllref, just coming from another output of CDCM61004 PLL.
    fpga_pll_ref_clk_101_p_i : in std_logic;
    fpga_pll_ref_clk_101_n_i : in std_logic;

    -- Clock input, used to derive the DDMTD clock (check out the general presentation
    -- of WR for explanation of its purpose). The clock is produced by the
    -- other VCXO, tuned by the second AD5662 DAC, (which is connected to
    -- dac_helper output of the WR Core)
    clk_20m_vcxo_i : in std_logic;

    -- Reset input, active low. Comes from the Gennum PCI-Express bridge.
    l_rst_n : in std_logic := 'H';

    -- Button 1 on the SPEC card. In our case, used as an external reset trigger.
    button1_n_i : in std_logic := 'H';

    -------------------------------------------------------------------------
    -- SFP pins
    -------------------------------------------------------------------------

    -- TX gigabit output
    sfp_txp_o : out std_logic;
    sfp_txn_o : out std_logic;

    -- RX gigabit input
    sfp_rxp_i : in std_logic;
    sfp_rxn_i : in std_logic;

    -- SFP MOD_DEF0 pin (used as a tied-to-ground SFP insertion detect line)
    sfp_det_i         : in    std_logic;
    -- SFP MOD_DEF1 pin (SCL line of the I2C EEPROM inside the SFP)
    sfp_scl_b         : inout std_logic;
    -- SFP MOD_DEF1 pin (SDA line of the I2C EEPROM inside the SFP)
    sfp_sda_b         : inout std_logic;
    -- SFP RATE_SELECT pin. Unused for most SFPs, in our case tied to 0.
    sfp_rate_select_b : inout std_logic;
    -- SFP laser fault detection pin. Unused in our design.
    sfp_tx_fault_i    : in    std_logic;
    -- SFP laser disable line. In our case, tied to GND.
    sfp_tx_disable_o  : out   std_logic;
    -- SFP-provided loss-of-link detection. We don't use it as Ethernet PCS
    -- has its own loss-of-sync detection mechanism.
    sfp_los_i         : in    std_logic;

    -- Green LED next to the SFP: indicates if the link is up.
    sfp_led_green_o : out std_logic;

    -- Red LED next to the SFP: blinking indicates that packets are being
    -- transferred.
    sfp_led_red_o : out std_logic;

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
    -- Miscellanous WR Core pins
    ---------------------------------------------------------------------------

    -- I2C bus connected to the EEPROM on the DIO mezzanine. This EEPROM is used
    -- for storing WR Core's configuration parameters.
    fmc_scl_b : inout std_logic;
    fmc_sda_b : inout std_logic;

    -- One-wire interface to DS18B20 temperature sensor, which also provides an
    -- unique serial number, that WRPC uses to assign itself a unique MAC address.
    thermo_id_b : inout std_logic;

    -- UART pins (connected to the mini-USB port)
    uart_txd_o : out std_logic;
    uart_rxd_i : in  std_logic;

    -------------------------------------------------------------------------
    -- Necessary Digital I/O mezzanine pins
    -------------------------------------------------------------------------

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
    dio_oe_n_o : out std_logic_vector(4 downto 0);

    -- Termination enable. When dio_term_en_o(N) is 1, connector (N+1) on the front
    -- panel is 50-ohm terminated
    dio_term_en_o : out std_logic_vector(4 downto 0);

    -- Two LEDs on the mezzanine panel
    dio_led_top_o : out std_logic;
    dio_led_bot_o : out std_logic
    );
end spec_top;

architecture rtl of spec_top is

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------

  -- Ethertype we are going to use for the streamer protocol. Value 0xdbff
  -- is default for standard WR Core CPU firmware. Other values need re-configuring
  -- the WR Core packet filter.
  constant c_STREAMER_ETHERTYPE : std_logic_vector(15 downto 0) := x"dbff";

  -- Trigger-to-output value, in 8 ns ticks. Set by default to 20us to work
  -- for 10km+ fibers.
  constant c_PULSE_DELAY : integer := 30000/8;

  -----------------------------------------------------------------------------
  -- Component declarations
  -----------------------------------------------------------------------------

  component spec_reset_gen
    port (
      clk_sys_i        : in  std_logic;
      rst_pcie_n_a_i   : in  std_logic;
      rst_button_n_a_i : in  std_logic;
      rst_n_o          : out std_logic);
  end component;

  component pulse_stamper
    generic (
      g_ref_clk_rate : integer);
    port (
      clk_ref_i       : in  std_logic;
      clk_sys_i       : in  std_logic;
      rst_n_i         : in  std_logic;
      pulse_a_i       : in  std_logic;
      tm_time_valid_i : in  std_logic;
      tm_tai_i        : in  std_logic_vector(39 downto 0);
      tm_cycles_i     : in  std_logic_vector(27 downto 0);
      tag_tai_o       : out std_logic_vector(39 downto 0);
      tag_cycles_o    : out std_logic_vector(27 downto 0);
      tag_valid_o     : out std_logic);
  end component;

  component pulse_gen
    generic (
      g_ref_clk_rate : integer);
    port (
      clk_ref_i       : in  std_logic;
      clk_sys_i       : in  std_logic;
      rst_n_i         : in  std_logic;
      pulse_o         : out std_logic;
      tm_time_valid_i : in  std_logic;
      tm_tai_i        : in  std_logic_vector(39 downto 0);
      tm_cycles_i     : in  std_logic_vector(27 downto 0);
      trig_ready_o    : out std_logic;
      trig_tai_i      : in  std_logic_vector(39 downto 0);
      trig_cycles_i   : in  std_logic_vector(27 downto 0);
      trig_valid_i    : in  std_logic);
  end component;

  component timestamp_adder
    generic (
      g_ref_clk_rate : integer);
    port (
      clk_i      : in  std_logic;
      rst_n_i    : in  std_logic;
      valid_i    : in  std_logic;
      a_tai_i    : in  std_logic_vector(39 downto 0);
      a_cycles_i : in  std_logic_vector(27 downto 0);
      b_tai_i    : in  std_logic_vector(39 downto 0);
      b_cycles_i : in  std_logic_vector(27 downto 0);
      valid_o    : out std_logic;
      q_tai_o    : out std_logic_vector(39 downto 0);
      q_cycles_o : out std_logic_vector(27 downto 0));
  end component;


  -----------------------------------------------------------------------------
  -- Signals declarations
  -----------------------------------------------------------------------------

  -- System reset
  signal rst_n : std_logic;

  -- System clock (62.5 MHz)
  signal clk_sys : std_logic;

  -- White Rabbit reference clock (125 MHz)
  signal clk_ref : std_logic;

  -- White Rabbit DDMTD helper clock (62.5-and-something MHz)
  signal clk_dmtd : std_logic;

  -- 125 MHz GTP clock coming from a dedicated input pin (same as clk_ref)
  signal clk_gtp : std_logic;

  -- PLL & clock buffer wiring
  signal clk_20m_vcxo_buf     : std_logic;
  signal pllout_clk_sys       : std_logic;
  signal pllout_clk_fb_pllref : std_logic;
  signal pllout_clk_dmtd      : std_logic;
  signal pllout_clk_fb_dmtd   : std_logic;

  -- Oscillator control DAC wiring
  signal dac_hpll_load_p1 : std_logic;
  signal dac_dpll_load_p1 : std_logic;
  signal dac_hpll_data    : std_logic_vector(15 downto 0);
  signal dac_dpll_data    : std_logic_vector(15 downto 0);

  -- PHY wiring
  signal phy_tx_data      : std_logic_vector(7 downto 0);
  signal phy_tx_k         : std_logic_vector(0 downto 0);
  signal phy_tx_disparity : std_logic;
  signal phy_tx_enc_err   : std_logic;
  signal phy_rx_data      : std_logic_vector(7 downto 0);
  signal phy_rx_rbclk     : std_logic;
  signal phy_rx_k         : std_logic_vector(0 downto 0);
  signal phy_rx_enc_err   : std_logic;
  signal phy_rx_bitslide  : std_logic_vector(3 downto 0);
  signal phy_rst          : std_logic;
  signal phy_loopen       : std_logic;

  -- Timing interface
  signal tm_time_valid : std_logic;
  signal tm_tai        : std_logic_vector(39 downto 0);
  signal tm_cycles     : std_logic_vector(27 downto 0);

  -- TX streamer signals
  signal tx_tag_tai                    : std_logic_vector(39 downto 0);
  signal tx_tag_cycles                 : std_logic_vector(27 downto 0);
  signal tx_tag_valid                  : std_logic;
  signal tx_data                       : std_logic_vector(79 downto 0);
  signal tx_valid, tx_dreq, tx_dreq_d0 : std_logic;

  -- RX streamer signals
  signal rx_data  : std_logic_vector(79 downto 0);
  signal rx_valid : std_logic;

  -- Trigger timestamp adjusted with delay
  signal adjusted_ts_valid  : std_logic;
  signal adjusted_ts_tai    : std_logic_vector(39 downto 0);
  signal adjusted_ts_cycles : std_logic_vector(27 downto 0);


  -- Digital I/O mezzanine wiring
  signal dio_in  : std_logic_vector(4 downto 0);
  signal dio_out : std_logic_vector(4 downto 0);

  -- Misc signals
  signal pps_p, pps_long : std_logic;

  signal sfp_scl_out, sfp_sda_out   : std_logic;
  signal fmc_scl_out, fmc_sda_out   : std_logic;
  signal owr_enable, owr_in         : std_logic_vector(1 downto 0);
  signal pulse_out, pulse_in_synced : std_logic;


  -- Fabric interface signals, passing packets between the WR Core and the streamers
  signal wrcore_snk_out : t_wrf_sink_out;
  signal wrcore_snk_in  : t_wrf_sink_in;
  signal wrcore_src_out : t_wrf_source_out;
  signal wrcore_src_in  : t_wrf_source_in;


  -- ChipScope for histogram readout/debugging

  component chipscope_icon
    port (
      CONTROL0 : inout std_logic_vector(35 downto 0));
  end component;

  component chipscope_ila
    port (
      CONTROL : inout std_logic_vector(35 downto 0);
      CLK     : in    std_logic;
      TRIG0   : in    std_logic_vector(31 downto 0);
      TRIG1   : in    std_logic_vector(31 downto 0);
      TRIG2   : in    std_logic_vector(31 downto 0);
      TRIG3   : in    std_logic_vector(31 downto 0));
  end component;

  signal control0                   : std_logic_vector(35 downto 0);
  signal trig0, trig1, trig2, trig3 : std_logic_vector(31 downto 0);

  signal rx_latency       : std_logic_vector(27 downto 0);
  signal rx_latency_valid : std_logic;
  
begin

  -----------------------------------------------------------------------------
  -- System/reference clock buffers and PLL
  -----------------------------------------------------------------------------

  -- Input differential buffer on the 125 MHz reference clock
  U_Reference_Clock_Buffer : IBUFGDS
    generic map (
      DIFF_TERM    => true,             -- Differential Termination
      IBUF_LOW_PWR => true,      -- Low power (TRUE) vs. performance (FALSE)
      IOSTANDARD   => "DEFAULT")  -- take the I/O standard from the UCF file
    port map (
      O  => clk_ref,                    -- Buffer output
      I  => clk_125m_pllref_p_i,  -- Diff_p buffer input (connect directly to top-level port)
      IB => clk_125m_pllref_n_i  -- Diff_n buffer input (connect directly to top-level port)
      );

  -- ... and the PLL that derives 62.5 MHz system clock from the 125 MHz reference
  U_System_Clock_PLL : PLL_BASE
    generic map (
      BANDWIDTH          => "OPTIMIZED",
      CLK_FEEDBACK       => "CLKFBOUT",
      COMPENSATION       => "INTERNAL",
      DIVCLK_DIVIDE      => 1,
      CLKFBOUT_MULT      => 8,
      CLKFBOUT_PHASE     => 0.000,
      CLKOUT0_DIVIDE     => 16,  -- Output 0: 125 MHz * 8 / 16 = 62.5 MHz
      CLKOUT0_PHASE      => 0.000,
      CLKOUT0_DUTY_CYCLE => 0.500,
      CLKOUT1_DIVIDE     => 16,
      CLKOUT1_PHASE      => 0.000,
      CLKOUT1_DUTY_CYCLE => 0.500,
      CLKOUT2_DIVIDE     => 16,
      CLKOUT2_PHASE      => 0.000,
      CLKOUT2_DUTY_CYCLE => 0.500,
      CLKIN_PERIOD       => 8.0,
      REF_JITTER         => 0.016)
    port map (
      CLKFBOUT => pllout_clk_fb_pllref,
      CLKOUT0  => pllout_clk_sys,
      CLKOUT1  => open,
      CLKOUT2  => open,
      CLKOUT3  => open,
      CLKOUT4  => open,
      CLKOUT5  => open,
      LOCKED   => open,
      RST      => '0',
      CLKFBIN  => pllout_clk_fb_pllref,
      CLKIN    => clk_ref);

  -- A buffer to drive system clock generated by the PLL above as a global
  -- clock net.
  U_System_Clock_Buffer : BUFG
    port map (
      O => clk_sys,
      I => pllout_clk_sys);

  -----------------------------------------------------------------------------
  -- DMTD clock buffers and PLL
  -----------------------------------------------------------------------------

  -- A global clock buffer to drive the PLL input pin from the 20 MHz VCXO clock
  -- input pin on the FPGA
  U_DMTD_VCXO_Clock_Buffer : BUFG
    port map (
      O => clk_20m_vcxo_buf,
      I => clk_20m_vcxo_i);

  -- The PLL that multiplies the 20 MHz VCXO input to obtain the DDMTD
  -- clock, that is sligthly offset in frequency wrs to the reference 125 MHz clock.
  -- The WR core additionally requires the DDMTD clock frequency to be divided
  -- by 2 (so instead of 125-point-something MHz we get 62.5-point-something
  -- MHz). This is to improve internal DDMTD phase detector timing.
  U_DMTD_Clock_PLL : PLL_BASE
    generic map (
      BANDWIDTH          => "OPTIMIZED",
      CLK_FEEDBACK       => "CLKFBOUT",
      COMPENSATION       => "INTERNAL",
      DIVCLK_DIVIDE      => 1,
      CLKFBOUT_MULT      => 50,
      CLKFBOUT_PHASE     => 0.000,
      CLKOUT0_DIVIDE     => 16,         -- 62.5 MHz
      CLKOUT0_PHASE      => 0.000,
      CLKOUT0_DUTY_CYCLE => 0.500,
      CLKOUT1_DIVIDE     => 16,         -- 62.5 MHz
      CLKOUT1_PHASE      => 0.000,
      CLKOUT1_DUTY_CYCLE => 0.500,
      CLKOUT2_DIVIDE     => 8,
      CLKOUT2_PHASE      => 0.000,
      CLKOUT2_DUTY_CYCLE => 0.500,
      CLKIN_PERIOD       => 50.0,
      REF_JITTER         => 0.016)
    port map (
      CLKFBOUT => pllout_clk_fb_dmtd,
      CLKOUT0  => pllout_clk_dmtd,
      CLKOUT1  => open,
      CLKOUT2  => open,
      CLKOUT3  => open,
      CLKOUT4  => open,
      CLKOUT5  => open,
      LOCKED   => open,
      RST      => '0',
      CLKFBIN  => pllout_clk_fb_dmtd,
      CLKIN    => clk_20m_vcxo_buf);

  -- A buffer to drive system clock generated by the PLL above as a global
  -- clock net.
  U_DMTD_Clock_Buffer : BUFG
    port map (
      O => clk_dmtd,
      I => pllout_clk_dmtd);


  ------------------------------------------------------------------------------
  -- Dedicated clock for GTP
  ------------------------------------------------------------------------------
  U_Dedicated_GTP_Clock_Buffer : IBUFGDS
    generic map(
      DIFF_TERM    => true,
      IBUF_LOW_PWR => true,
      IOSTANDARD   => "DEFAULT")
    port map (
      O  => clk_gtp,
      I  => fpga_pll_ref_clk_101_p_i,
      IB => fpga_pll_ref_clk_101_n_i
      );

  -----------------------------------------------------------------------------
  -- Reset signal generator
  -----------------------------------------------------------------------------

  -- Produces a clean reset signal upon the following
  -- conditions:
  -- - device is powered up
  -- - a PCI-Express bus reset is requested
  -- - button 1 is pressed.
  U_Reset_Gen : spec_reset_gen
    port map (
      clk_sys_i        => clk_sys,
      rst_pcie_n_a_i   => L_RST_N,
      rst_button_n_a_i => button1_n_i,
      rst_n_o          => rst_n);

  -----------------------------------------------------------------------------
  -- The WR Core part. The simplest functional instantiation.
  -----------------------------------------------------------------------------

  U_The_WR_Core : xwr_core
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
      g_dpram_initf               => "../../../bin/wrpc/wrc_phy8_sim.bram",
--       g_aux_sdb                   => c_etherbone_sdb, --ML
      g_dpram_size                => 131072/4,
      g_interface_mode            => PIPELINED,
      g_address_granularity       => BYTE)
    port map (
      -- Clocks & resets connections
      clk_sys_i  => clk_sys,
      clk_ref_i  => clk_ref,
      clk_dmtd_i => clk_dmtd,
      rst_n_i    => rst_n,

      -- Fabric interface pins
      wrf_snk_i => wrcore_snk_in,
      wrf_snk_o => wrcore_snk_out,
      wrf_src_i => wrcore_src_in,
      wrf_src_o => wrcore_src_out,

      -- Timing interface pins
      tm_time_valid_o => tm_time_valid,
      tm_tai_o        => tm_tai,
      tm_cycles_o     => tm_cycles,

      -- PHY connections
      phy_ref_clk_i      => clk_ref,
      phy_tx_data_o      => phy_tx_data,
      phy_tx_k_o         => phy_tx_k,
      phy_tx_disparity_i => phy_tx_disparity,
      phy_tx_enc_err_i   => phy_tx_enc_err,
      phy_rx_data_i      => phy_rx_data,
      phy_rx_rbclk_i     => phy_rx_rbclk,
      phy_rx_k_i         => phy_rx_k,
      phy_rx_enc_err_i   => phy_rx_enc_err,
      phy_rx_bitslide_i  => phy_rx_bitslide,
      phy_rst_o          => phy_rst,
      phy_loopen_o       => phy_loopen,

      -- Oscillator control DACs connections
      dac_hpll_load_p1_o => dac_hpll_load_p1,
      dac_hpll_data_o    => dac_hpll_data,
      dac_dpll_load_p1_o => dac_dpll_load_p1,
      dac_dpll_data_o    => dac_dpll_data,

      -- Miscellanous pins
      uart_rxd_i => uart_rxd_i,
      uart_txd_o => uart_txd_o,

      scl_o => fmc_scl_out,
      scl_i => fmc_scl_b,
      sda_o => fmc_sda_out,
      sda_i => fmc_sda_b,

      sfp_scl_o => sfp_scl_out,
      sfp_scl_i => sfp_scl_b,
      sfp_sda_o => sfp_sda_out,
      sfp_sda_i => sfp_sda_b,

      sfp_det_i => sfp_det_i,

      led_link_o => sfp_led_green_o,
      led_act_o  => sfp_led_red_o,

      owr_en_o => owr_enable,
      owr_i    => owr_in,

      -- The PPS output, which we'll drive to the DIO mezzanine channel 1.
      pps_p_o => pps_p
      );


  -----------------------------------------------------------------------------
  -- Dual channel SPI DAC driver
  -----------------------------------------------------------------------------
  
  U_DAC_ARB : spec_serial_dac_arb
    generic map (
      g_invert_sclk    => false,        -- configured for 2xAD5662. Don't
                                        -- change the parameters.
      g_num_extra_bits => 8)

    port map (
      clk_i   => clk_sys,
      rst_n_i => rst_n,

      -- DAC 1 controls the main (clk_ref) oscillator
      val1_i  => dac_dpll_data,
      load1_i => dac_dpll_load_p1,

      -- DAC 2 controls the helper (clk_ddmtd) oscillator
      val2_i  => dac_hpll_data,
      load2_i => dac_hpll_load_p1,

      dac_cs_n_o(0) => dac_cs1_n_o,
      dac_cs_n_o(1) => dac_cs2_n_o,
      dac_sclk_o    => dac_sclk_o,
      dac_din_o     => dac_din_o);


  -----------------------------------------------------------------------------
  -- Gigabit Ethernet PHY using Spartan-6 GTP transceviver.
  -----------------------------------------------------------------------------
  
  U_GTP : wr_gtp_phy_spartan6
    generic map (
      g_enable_ch0 => 0,
      -- each GTP has two channels, so does the PHY module.
      -- The SFP on the SPEC is connected to the 2nd channel. 
      g_enable_ch1 => 1,
      g_simulation => g_simulation)
    port map (
      gtp_clk_i => clk_gtp,

      ch1_ref_clk_i => clk_ref,

      -- TX code stream
      ch1_tx_data_i      => phy_tx_data,
      -- TX control/data select
      ch1_tx_k_i         => phy_tx_k(0),
      -- TX disparity of the previous symbol
      ch1_tx_disparity_o => phy_tx_disparity,
      -- TX encoding error
      ch1_tx_enc_err_o   => phy_tx_enc_err,

      -- RX recovered byte clock
      ch1_rx_rbclk_o    => phy_rx_rbclk,
      -- RX data stream
      ch1_rx_data_o     => phy_rx_data,
      -- RX control/data select
      ch1_rx_k_o        => phy_rx_k(0),
      -- RX encoding error detection
      ch1_rx_enc_err_o  => phy_rx_enc_err,
      -- RX path comma alignment bit slide delay (crucial for accuracy!)
      ch1_rx_bitslide_o => phy_rx_bitslide,

      -- Channel reset
      ch1_rst_i    => phy_rst,
      -- Loopback mode enable
      ch1_loopen_i => phy_loopen,

      pad_txn1_o => sfp_txn_o,
      pad_txp1_o => sfp_txp_o,
      pad_rxn1_i => sfp_rxn_i,
      pad_rxp1_i => sfp_rxp_i);

  -- pps_p signal from the WR core is 8ns- (single clk_ref cycle) wide. This is
  -- too short to drive outputs such as LEDs. Let's extend its length to some
  -- human-noticeable value
  U_Extend_PPS : gc_extend_pulse
    generic map (
      g_width => 10000000)              -- output length: 10000000x8ns = 80 ms.

    port map (
      clk_i      => clk_ref,
      rst_n_i    => rst_n,
      pulse_i    => pps_p,
      extended_o => pps_long);

  -----------------------------------------------------------------------------
  -- Trigger distribution stuff - timestamping & packet transmission part
  -----------------------------------------------------------------------------
  
  U_Pulse_Stamper : pulse_stamper
    generic map (
      g_ref_clk_rate => 125000000)
    port map (
      clk_ref_i => clk_ref,
      clk_sys_i => clk_sys,
      rst_n_i   => rst_n,
      pulse_a_i => dio_in(1),           -- I/O 2 = our pulse input

      tm_time_valid_i => tm_time_valid,  -- timing ports of the WR Core
      tm_tai_i        => tm_tai,
      tm_cycles_i     => tm_cycles,

      tag_tai_o    => tx_tag_tai,       -- time tag of the latest pulse
      tag_cycles_o => tx_tag_cycles,
      tag_valid_o  => tx_tag_valid);

  -- Streamer instantiation. 
  U_TX_Streamer : xtx_streamer
    generic map (
      -- We send each timestamp (40 TAI bits + 28
      -- cycle bits) as a single parallel data word of 68 bits. Since data width
      -- must be a multiple of 16 bits, we round it up to 80 bits).
      g_data_width => 80,

      -- TX threshold = 4 data words. (it's anyway ignored because of
      -- g_tx_timeout setting  below)
      g_tx_threshold => 4,

      -- minimum timeout: sends packets asap to minimize latency (but it's not
      -- good for large amounts of data due to encapsulation overhead)
      g_tx_timeout => 1)
    port map (
      clk_sys_i => clk_sys,
      rst_n_i   => rst_n,
      -- Wire the packet source of the streamer to the packet sink of the WR Core
      src_i     => wrcore_snk_out,
      src_o     => wrcore_snk_in,

      clk_ref_i => clk_ref,
      tm_time_valid_i => tm_time_valid,
      tm_tai_i        => tm_tai,
      tm_cycles_i     => tm_cycles,

      tx_data_i  => tx_data,
      tx_valid_i => tx_valid,
      tx_dreq_o  => tx_dreq,
      -- every data word we send is the last one, as a single transfer in our
      -- case contains only one 80-bit data word.
      tx_last_p1_i  => '1',

      -- send broadcast packets, so that many receivers can use triggers sent
      -- by us.
      cfg_mac_target_i => x"ffffffffffff",
      cfg_ethertype_i  => c_STREAMER_ETHERTYPE);


  -- Pack the time stamp into a 80-bit data word for the streamer
  tx_data(27 downto 0)       <= tx_tag_cycles;
  tx_data(32 + 39 downto 32) <= tx_tag_tai;
  -- avoid Xes (this may break simulations)
  tx_data(31 downto 28)      <= (others => '0');
  tx_data(79 downto 32+40)   <= (others => '0');

  -- Data valid signal: simply drop the timestamp if the streamer can't accept
  -- data for the moment.
  tx_valid <= tx_dreq_d0 and tx_tag_valid;

  -- tx_dreq_o output of the streamer is asserted one clock cycle in advance,
  -- while the line above drives the valid signal combinatorially. We need a delay.
  process(clk_sys)
  begin
    if rising_edge(clk_sys) then
      tx_dreq_d0 <= tx_dreq;
    end if;
  end process;


  -----------------------------------------------------------------------------
  -- Trigger distribution stuff - packet reception and pulse generation
  -----------------------------------------------------------------------------

  -- Streamer instantiation
  U_RX_Streamer : xrx_streamer
    generic map (
      -- data width must be identical as in the TX streamer - otherwise, we'll be receiving
      -- rubbish
      g_data_width        => 80)
    port map (
      clk_sys_i => clk_sys,
      rst_n_i   => rst_n,

      -- Wire the packet sink of the streamer to the packet source of the WR Core
      snk_i => wrcore_src_out,
      snk_o => wrcore_src_in,

      clk_ref_i => clk_ref,
      tm_time_valid_i => tm_time_valid,
      tm_tai_i        => tm_tai,
      tm_cycles_i     => tm_cycles,

      rx_data_o               => rx_data,
      rx_valid_o              => rx_valid,
      rx_dreq_i               => '1',
      rx_latency_o            => rx_latency,
      rx_latency_valid_o      => rx_latency_valid,
      cfg_ethertype_i         => c_STREAMER_ETHERTYPE,
      cfg_accept_broadcasts_i => '1');

  -- Add a fixed delay to the reveived trigger timestamp
  U_Add_Delay1 : timestamp_adder
    generic map (
      g_ref_clk_rate => 125000000)
    port map (
      clk_i   => clk_sys,
      rst_n_i => rst_n,
      valid_i => rx_valid,

      a_tai_i    => rx_data(32 + 39 downto 32),
      a_cycles_i => rx_data(27 downto 0),

      b_tai_i    => (others => '0'),
      b_cycles_i => std_logic_vector(to_unsigned(c_PULSE_DELAY, 28)),

      valid_o    => adjusted_ts_valid,
      q_tai_o    => adjusted_ts_tai,
      q_cycles_o => adjusted_ts_cycles);

  -- And a pulse generator that produces a pulse at a time received by the
  -- streamer above adjusted with the delay
  U_Pulse_Generator : pulse_gen
    generic map (
      g_ref_clk_rate => 125000000)
    port map (
      clk_ref_i       => clk_ref,
      clk_sys_i       => clk_sys,
      rst_n_i         => rst_n,
      pulse_o         => pulse_out,
      tm_time_valid_i => tm_time_valid,
      tm_tai_i        => tm_tai,
      tm_cycles_i     => tm_cycles,
      trig_tai_i      => adjusted_ts_tai,
      trig_cycles_i   => adjusted_ts_cycles,
      trig_valid_i    => adjusted_ts_valid);

  -- pulse_gen above generates pulses that are single-cycle long. This is too
  -- short to observe on a scope, particularly with slower time base (to see 2
  -- pulses simulatenously). Let's extend it a bit:
  U_Extend_Output_Pulse : gc_extend_pulse
    generic map (
      -- 1000 * 8ns = 8 us
      g_width => 1000)
    port map (
      clk_i      => clk_ref,
      rst_n_i    => rst_n,
      pulse_i    => pulse_out,
      extended_o => dio_out(2));

  -----------------------------------------------------------------------------
  -- Differential buffers for the Digital I/O Mezzanine
  -----------------------------------------------------------------------------
  gen_dio_iobufs : for i in 0 to 4 generate
    U_Input_Buffer : IBUFDS
      generic map (
        DIFF_TERM => true)
      port map (
        O  => dio_in(i),
        I  => dio_p_i(i),
        IB => dio_n_i(i)
        );

    U_Output_Buffer : OBUFDS
      port map (
        I  => dio_out(i),
        O  => dio_p_o(i),
        OB => dio_n_o(i)
        );
  end generate gen_dio_iobufs;

  -----------------------------------------------------------------------------
  -- Combinatorial pins, tristate buffers, etc.
  -----------------------------------------------------------------------------

  -- The SFP is permanently enabled
  sfp_tx_disable_o  <= '0';
  sfp_rate_select_b <= '0';

  -- Open-drain driver for the Onewire bus
  thermo_id_b <= '0' when owr_enable(0) = '1' else 'Z';
  owr_in(0)   <= thermo_id_b;

  -- Open-drain drivers for the I2C busses
  fmc_scl_b <= '0' when fmc_scl_out = '0' else 'Z';
  fmc_sda_b <= '0' when fmc_sda_out = '0' else 'Z';

  sfp_scl_b <= '0' when sfp_scl_out = '0' else 'Z';
  sfp_sda_b <= '0' when sfp_sda_out = '0' else 'Z';

  -- Connect the PPS output to the I/O 1 of the Digital I/O mezzanine
  dio_out(0) <= pps_long;

  -- Drive unused DIO outputs to 0.
  dio_out(4) <= dio_out(2);
  dio_out(3) <= dio_out(2);
  dio_out(1) <= '0';

  -- all DIO connectors except I/O 2 (trigger input) are outputs
  dio_oe_n_o(0)          <= '0';
  dio_oe_n_o(1)          <= '1';
  dio_oe_n_o(4 downto 2) <= (others => '0');

  -- terminate only the trigger input
  dio_oe_n_o(0)          <= '0';
  dio_oe_n_o(1)          <= '1';
  dio_oe_n_o(4 downto 2) <= (others => '0');

  dio_term_en_o(1)          <= '1';
  dio_term_en_o(0)          <= '0';
  dio_term_en_o(4 downto 2) <= (others => '0');


  -- Drive one of the LEDs on the mezzanine with out PPS signal (pps_led is a
  -- longer version that can be used to directly drive a LED)
  dio_led_top_o <= pps_long;

  -- The other LED on the DIO serves as an indicator of incoming trigger pulses.

  U_Sync_Trigger_Pulse : gc_sync_ffs
    port map (
      clk_i    => clk_ref,
      rst_n_i  => rst_n,
      data_i   => dio_in(1),
      synced_o => pulse_in_synced);

  U_Extend_Trigger_Pulse : gc_extend_pulse
    generic map (
      -- 1000 * 8ns = 8 us
      g_width => 1000)
    port map (
      clk_i      => clk_ref,
      rst_n_i    => rst_n,
      pulse_i    => pulse_in_synced,
      extended_o => dio_led_bot_o);

  CS_ICON : chipscope_icon
    port map (
      CONTROL0 => CONTROL0);
  CS_ILA : chipscope_ila
    port map (
      CONTROL => CONTROL0,
      CLK     => clk_sys,
      TRIG0   => TRIG0,
      TRIG1   => TRIG1,
      TRIG2   => TRIG2,
      TRIG3   => TRIG3);

  trig0(27 downto 0) <= rx_latency;
  trig0(31) <= rx_latency_valid;

  trig1(31) <= tm_time_valid;
  trig1(27 downto 0) <= tm_cycles;
  trig1(30 downto 28) <= tm_tai(2 downto 0);

  trig2(31) <= adjusted_ts_valid;
  trig2(27 downto 0) <= adjusted_ts_cycles;
  trig2(30 downto 28) <= adjusted_ts_tai(2 downto 0);

  trig3(31) <= rx_valid;
  trig3(27 downto 0) <= rx_data(27 downto 0);
  trig3(30 downto 28) <= rx_data(32 + 2 downto 32);
  
  
end rtl;
