-------------------------------------------------------------------------------
-- Title      : WRPC reference design for VFC-HD
-- Project    : WR PTP Core
-- URL        : http://www.ohwr.org/projects/wr-cores/wiki/Wrpc_core
-------------------------------------------------------------------------------
-- File       : vfchd_wr_ref_top.vhd
-- Author(s)  : Dimitrios Lampridis  <dimitrios.lampridis@cern.ch>
-- Company    : CERN (BE-CO-HT)
-- Created    : 2017-01-24
-- Last update: 2017-03-10
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Top-level file for the WRPC reference design on the VFC-HD.
--
-- This is a reference top HDL that instanciates the WR PTP Core together with
-- its peripherals to be run on a VFC-HD card.
--
-- There are two main usecases for this HDL file:
-- * let new users easily synthesize a WR PTP Core bitstream that can be run on
--   reference hardware
-- * provide a reference top HDL file showing how the WRPC can be instantiated
--   in HDL projects.
--
-- VFC-HD:  http://www.ohwr.org/projects/vfc-hd/
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
use work.xvme64x_core_pkg.all;
use work.wr_board_pkg.all;
use work.wr_vfchd_pkg.all;
use work.vfchd_i2cmux_pkg.all;

entity vfchd_wr_ref_top is
  generic (
    g_dpram_initf : string := "../../bin/wrpc/wrc_phy8.mif";
    -- Simulation-mode enable parameter. Set by default (synthesis) to 0, and
    -- changed to non-zero in the instantiation of the top level DUT in the testbench.
    -- Its purpose is to reduce some internal counters/timeouts to speed up simulations.
    g_simulation : integer := 0
  );
  port (
    ---------------------------------------------------------------------------
    -- Clocks/resets
    ---------------------------------------------------------------------------

    -- Clock inputs from the board
    clk_board_125m_i : in std_logic;
    clk_board_20m_i  : in std_logic;

    -- Reset input (active low, can be async)
    areset_n_i : in std_logic;

    ---------------------------------------------------------------------------
    -- VME interface
    ---------------------------------------------------------------------------

    vme_write_n_i   : in    std_logic;
    vme_lword_n_b   : inout std_logic;
    vme_iackout_n_o : out   std_logic;
    vme_iackin_n_i  : in    std_logic;
    vme_iack_n_i    : in    std_logic;
    vme_dtack_oe_o  : out   std_logic;
    vme_ds_n_i      : in    std_logic_vector(1 downto 0);
    vme_data_oe_n_o : out   std_logic;
    vme_data_dir_o  : out   std_logic;
    vme_as_n_i      : in    std_logic;
    vme_addr_oe_n_o : out   std_logic;
    vme_addr_dir_o  : out   std_logic;
    vme_irq_n_o     : out   std_logic_vector(7 downto 1);
    vme_data_b      : inout std_logic_vector(31 downto 0);
    vme_am_i        : in    std_logic_vector(5 downto 0);
    vme_addr_b      : inout std_logic_vector(31 downto 1);

    ---------------------------------------------------------------------------
    -- SPI interfaces to DACs
    ---------------------------------------------------------------------------

    dac_ref_sync_n_o  : out std_logic;
    dac_dmtd_sync_n_o : out std_logic;
    dac_din_o         : out std_logic;
    dac_sclk_o        : out std_logic;

    ---------------------------------------------------------------------------
    -- SPI interfaces to VFC Vadj and VADC
    ---------------------------------------------------------------------------

    --vfc_vadj_cs_n_o : out std_logic;
    --vfc_vadj_din_o  : out std_logic;
    --vfc_vadj_sck_o  : out std_logic;

    --vfc_vadc_cs_n_o : out std_logic;
    --vfc_vadc_din_o  : out std_logic;
    --vfc_vadc_dout_i : in  std_logic;
    --vfc_vadc_sck_o  : out std_logic;

    ---------------------------------------------------------------------------
    -- SFP I/O for transceiver
    ---------------------------------------------------------------------------

    sfp_tx_o : out std_logic;
    sfp_rx_i : in  std_logic;

    ---------------------------------------------------------------------------
    -- VFC IO/I2C Mux
    ---------------------------------------------------------------------------

    i2c_mux_sda_b : inout std_logic;
    i2c_mux_scl_b : inout std_logic;

    io_exp_irq_bsteth_n_i : in std_logic;
    io_exp_irq_los_n_i    : in std_logic;

    ---------------------------------------------------------------------------
    -- I2C EEPROM
    ---------------------------------------------------------------------------

    eeprom_sda_b : inout std_logic;
    eeprom_scl_b : inout std_logic;

    ---------------------------------------------------------------------------
    -- Onewire interface
    ---------------------------------------------------------------------------

    onewire_b : inout std_logic;

    ---------------------------------------------------------------------------
    -- FMC DIO mezzannine and VFC GPIOs
    ---------------------------------------------------------------------------

    fmc_enable_n_o : out std_logic;
    dio_led_term_o : out std_logic;
    dio_led_out_o  : out std_logic;
    dio4_i         : in  std_logic;   -- LEMO4 as input for ext PPS in
    dio4_oe_n_o    : out std_logic;   -- LEMO4 output enable control
    dio4_term_en_o : out std_logic;   -- LEMO4 output termination control
    dio5_clk_i     : in  std_logic;   -- LEMO5 clock input for ext 10MHz
    dio5_oe_n_o    : out std_logic;   -- LEMO5 output enable control
    dio5_term_en_o : out std_logic;   -- LEMO5 output termination control
    vfchd_gpio3_o  : out std_logic;   -- VFC GPIO3 for PPS ref output
    vfchd_gpio4_o  : out std_logic);  -- VFC GPIO4 for ref clock output

end entity vfchd_wr_ref_top;

architecture top of vfchd_wr_ref_top is

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------

  -- Number of masters on the primary wishbone crossbar
  constant c_NUM_WB1_MASTERS : integer := 2;

  -- Number of slaves on the primary wishbone crossbar
  constant c_NUM_WB1_SLAVES : integer := 3;

  -- Number of masters on the secondary wishbone crossbar
  constant c_NUM_WB2_MASTERS : integer := 2;

  -- Number of slaves on the secondary wishbone crossbar
  constant c_NUM_WB2_SLAVES : integer := 1;

  -- Primary Wishbone master(s) offsets
  constant c_WB_MASTER_VME     : integer := 0;
  constant c_WB_MASTER_ETHBONE : integer := 1;

  -- Primary Wishbone slave(s) offsets
  constant c_WB_SLAVE_I2CCFG : integer := 0;
  constant c_WB_SLAVE_SECOND : integer := 1;
  constant c_WB_SLAVE_WRC    : integer := 2;

  -- Secondary Wishbone master(s) offsets
  constant c_WB_MASTER_PRIM  : integer := 0;
  constant c_WB_MASTER_SFPID : integer := 1;

  -- Secondary Wishbone slave(s) offsets
  constant c_WB_SLAVE_I2CMUX : integer := 0;

  -- sdb header address on primary crossbar
  constant c_SDB_ADDRESS : t_wishbone_address := x"00000000";

  -- SDB record for IO Exp configuration port
  constant c_xwb_i2ccfg_sdb : t_sdb_device := (
    abi_class     => x"0000",                     -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"1",                        -- 8-bit port granularity
    sdb_component => (
      addr_first  => x"0000000000000000",
      addr_last   => x"0000000000000003",
      product     => (
        vendor_id => x"000000000000CE42",         -- CERN
        device_id => x"00008889",
        version   => x"00000001",
        date      => x"20170126",
        name      => "BE-BI I2C Mux Cfg  ")));

  constant c_mux_bridge_sdb : t_sdb_bridge :=
    f_xwb_bridge_manual_sdb(x"00003fff", x"00000000");

  -- f_xwb_bridge_manual_sdb(size, sdb_addr)
  -- Note: sdb_addr is the sdb records address relative to the bridge base address
  constant c_wrc_bridge_sdb : t_sdb_bridge :=
    f_xwb_bridge_manual_sdb(x"0003ffff", x"00030000");

  -- Primary wishbone crossbar layout
  constant c_WB_LAYOUT : t_sdb_record_array(c_NUM_WB1_SLAVES - 1 downto 0) := (
    c_WB_SLAVE_I2CCFG => f_sdb_embed_device(c_xwb_i2ccfg_sdb, x"00001000"),
    c_WB_SLAVE_SECOND => f_sdb_embed_bridge(c_mux_bridge_sdb, x"00004000"),
    c_WB_SLAVE_WRC    => f_sdb_embed_bridge(c_wrc_bridge_sdb, x"00040000"));

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  -- Wishbone buse(s) from masters attached to primary crossbar
  signal cnx1_master_out : t_wishbone_master_out_array(c_NUM_WB1_MASTERS-1 downto 0);
  signal cnx1_master_in  : t_wishbone_master_in_array(c_NUM_WB1_MASTERS-1 downto 0);

  -- Wishbone buse(s) to slaves attached to primary crossbar
  signal cnx1_slave_out : t_wishbone_slave_out_array(c_NUM_WB1_SLAVES-1 downto 0);
  signal cnx1_slave_in  : t_wishbone_slave_in_array(c_NUM_WB1_SLAVES-1 downto 0);

  -- Wishbone buse(s) from masters attached to secondary crossbar
  signal cnx2_master_out : t_wishbone_master_out_array(c_NUM_WB2_MASTERS-1 downto 0);
  signal cnx2_master_in  : t_wishbone_master_in_array(c_NUM_WB2_MASTERS-1 downto 0);

  -- Wishbone buse(s) to slaves attached to secondary crossbar
  signal cnx2_slave_out : t_wishbone_slave_out_array(c_NUM_WB2_SLAVES-1 downto 0);
  signal cnx2_slave_in  : t_wishbone_slave_in_array(c_NUM_WB2_SLAVES-1 downto 0);

  -- clock and reset
  signal clk_sys_62m5   : std_logic;
  signal rst_sys_62m5   : std_logic;
  signal rst_sys_62m5_n : std_logic;
  signal clk_ref_125m   : std_logic;
  signal clk_ref_div2   : std_logic;
  signal clk_ext_ref    : std_logic;

  -- I2C EEPROM
  signal eeprom_sda_in  : std_logic;
  signal eeprom_sda_out : std_logic;
  signal eeprom_scl_in  : std_logic;
  signal eeprom_scl_out : std_logic;

  -- VME
  signal vme_data_b_out    : std_logic_vector(31 downto 0);
  signal vme_addr_b_out    : std_logic_vector(31 downto 1);
  signal vme_lword_n_b_out : std_logic;
  signal Vme_data_dir_int  : std_logic;
  signal vme_addr_dir_int  : std_logic;
  signal vme_dtack_n       : std_logic;
  signal vme_ga            : std_logic_vector(5 downto 0);

  -- SFP
  signal sfp_present    : std_logic;
  signal sfp_det_valid  : std_logic;
  signal sfp_data       : std_logic_vector (127 downto 0);
  signal sfp_wb_adr     : t_wishbone_address;
  signal sfp_tx_fault   : std_logic;
  signal sfp_los        : std_logic;
  signal sfp_tx_disable : std_logic;

  -- OneWire
  signal onewire_data : std_logic;
  signal onewire_oe   : std_logic;

  -- IO/I2C Mux
  signal io_exp_init_done : std_logic;
  signal io_exp_wr_req    : std_logic;
  signal io_exp_wr_on     : std_logic;
  signal io_exp_rd_req    : std_logic;
  signal io_exp_rd_on     : std_logic;
  signal io_exp_addr      : std_logic_vector(2 downto 0);
  signal io_exp_reg_addr  : std_logic_vector(1 downto 0);
  signal io_exp_data      : std_logic_vector(7 downto 0);
  signal i2c_slv_wr_req   : std_logic;
  signal i2c_slv_wr_on    : std_logic;
  signal i2c_slv_rd_req   : std_logic;
  signal i2c_slv_rd_on    : std_logic;
  signal i2c_slv_addr     : std_logic_vector(6 downto 0);
  signal i2c_slv_reg_addr : std_logic_vector(7 downto 0);
  signal i2c_slv_byte     : std_logic_vector(7 downto 0);
  signal i2c_mux_addr     : std_logic;
  signal i2c_mux_channel  : std_logic_vector(1 downto 0);
  signal i2c_mst_busy     : std_logic;
  signal i2c_mst_dav      : std_logic;
  signal i2c_mst_ack_err  : std_logic;
  signal i2c_mst_data     : std_logic_vector(7 downto 0);
  signal i2c_wb_adr       : std_logic_vector(11 downto 0);
  signal i2c_wb_dat_in    : std_logic_vector(7 downto 0);
  signal i2c_wb_dat_out   : std_logic_vector(7 downto 0);

  -- LEDs
  signal pps_led     : std_logic;
  signal pps_led_d   : std_logic;
  signal pps_ext_in  : std_logic;
  signal vfchd_led   : std_logic_vector(7 downto 0);
  signal wr_led_link : std_logic;
  signal wr_led_act  : std_logic;

begin  -- architecture top

  rst_sys_62m5 <= not rst_sys_62m5_n;

  -----------------------------------------------------------------------------
  -- Primary wishbone Crossbar
  -----------------------------------------------------------------------------

  cmp_primary_sdb_crossbar : xwb_sdb_crossbar
    generic map (
      g_num_masters => c_NUM_WB1_MASTERS,
      g_num_slaves  => c_NUM_WB1_SLAVES,
      g_registered  => TRUE,
      g_wraparound  => TRUE,
      g_layout      => c_WB_LAYOUT,
      g_sdb_addr    => c_SDB_ADDRESS)
    port map (
      clk_sys_i => clk_sys_62m5,
      rst_n_i   => rst_sys_62m5_n,
      slave_i   => cnx1_master_out,
      slave_o   => cnx1_master_in,
      master_i  => cnx1_slave_out,
      master_o  => cnx1_slave_in);

  -----------------------------------------------------------------------------
  -- VME64x Core (WB Master #1)
  -----------------------------------------------------------------------------

  cmp_vme_core : xvme64x_core
    port map (
      clk_i           => clk_sys_62m5,
      rst_n_i         => rst_sys_62m5_n,
      VME_AS_n_i      => vme_as_n_i,
      VME_RST_n_i     => io_exp_init_done,
      VME_WRITE_n_i   => vme_write_n_i,
      VME_AM_i        => vme_am_i,
      VME_DS_n_i      => vme_ds_n_i,
      VME_GA_i        => vme_ga,
      VME_BERR_o      => open,
      VME_DTACK_n_o   => vme_dtack_n,
      VME_RETRY_n_o   => open,
      VME_RETRY_OE_o  => open,
      VME_LWORD_n_b_i => vme_lword_n_b,
      VME_LWORD_n_b_o => vme_lword_n_b_out,
      VME_ADDR_b_i    => vme_addr_b,
      VME_DATA_b_o    => vme_data_b_out,
      VME_ADDR_b_o    => vme_addr_b_out,
      VME_DATA_b_i    => vme_data_b,
      VME_IRQ_n_o     => vme_irq_n_o,
      VME_IACK_n_i    => vme_iack_n_i,
      VME_IACKIN_n_i  => vme_iackin_n_i,
      VME_IACKOUT_n_o => vme_iackout_n_o,
      VME_DTACK_OE_o  => open,
      VME_DATA_DIR_o  => vme_data_dir_int,
      VME_DATA_OE_N_o => vme_data_oe_n_o,
      VME_ADDR_DIR_o  => vme_addr_dir_int,
      VME_ADDR_OE_N_o => vme_addr_oe_n_o,
      master_o        => cnx1_master_out(c_WB_MASTER_VME),
      master_i        => cnx1_master_in(c_WB_MASTER_VME),
      irq_i           => '0');

  -- Handle DTACK according to VFC-HD hardware
  vme_dtack_oe_o <= not vme_dtack_n;

  -- VME tri-state buffers
  vme_data_b    <= vme_data_b_out    when vme_data_dir_int = '1' else (others => 'Z');
  vme_addr_b    <= vme_addr_b_out    when vme_addr_dir_int = '1' else (others => 'Z');
  vme_lword_n_b <= vme_lword_n_b_out when vme_addr_dir_int = '1' else 'Z';

  vme_addr_dir_o <= vme_addr_dir_int;
  vme_data_dir_o <= vme_data_dir_int;

  -----------------------------------------------------------------------------
  -- The WR PTP core board package (WB Slave + WB Master #2 (Etherbone))
  -----------------------------------------------------------------------------

  cmp_xwrc_board_vfchd : xwrc_board_vfchd
    generic map (
      g_simulation                => g_simulation,
      g_with_external_clock_input => FALSE,
      g_dpram_initf               => g_dpram_initf,
      g_fabric_iface              => ETHERBONE)
    port map (
      clk_board_125m_i  => clk_board_125m_i,
      clk_board_20m_i   => clk_board_20m_i,
      clk_10m_ext_i     => clk_ext_ref,
      areset_n_i        => areset_n_i,
      clk_sys_62m5_o    => clk_sys_62m5,
      clk_ref_125m_o    => clk_ref_125m,
      rst_sys_62m5_n_o  => rst_sys_62m5_n,
      dac_ref_sync_n_o  => dac_ref_sync_n_o,
      dac_dmtd_sync_n_o => dac_dmtd_sync_n_o,
      dac_din_o         => dac_din_o,
      dac_sclk_o        => dac_sclk_o,
      sfp_tx_o          => sfp_tx_o,
      sfp_rx_i          => sfp_rx_i,
      sfp_det_valid_i   => sfp_det_valid,
      sfp_data_i        => sfp_data,
      sfp_tx_fault_i    => sfp_tx_fault,
      sfp_los_i         => sfp_los,
      sfp_tx_disable_o  => sfp_tx_disable,
      eeprom_sda_i      => eeprom_sda_in,
      eeprom_sda_o      => eeprom_sda_out,
      eeprom_scl_i      => eeprom_scl_in,
      eeprom_scl_o      => eeprom_scl_out,
      onewire_i         => onewire_data,
      onewire_oen_o     => onewire_oe,
      wb_slave_o        => cnx1_slave_out(c_WB_SLAVE_WRC),
      wb_slave_i        => cnx1_slave_in(c_WB_SLAVE_WRC),
      wb_eth_master_o   => cnx1_master_out(c_WB_MASTER_ETHBONE),
      wb_eth_master_i   => cnx1_master_in(c_WB_MASTER_ETHBONE),
      pps_ext_i         => pps_ext_in,
      pps_p_o           => vfchd_gpio3_o,
      pps_led_o         => pps_led,
      led_link_o        => wr_led_link,
      led_act_o         => wr_led_act);

  -- tri-state I2C EEPROM
  eeprom_sda_b  <= '0' when (eeprom_sda_out = '0') else 'Z';
  eeprom_sda_in <= eeprom_sda_b;

  eeprom_scl_b  <= '0' when (eeprom_scl_out = '0') else 'Z';
  eeprom_scl_in <= eeprom_scl_b;

  -- tri-state onewire access
  onewire_b    <= '0' when (onewire_oe = '1') else 'Z';
  onewire_data <= onewire_b;

  -----------------------------------------------------------------------------
  -- VFCHD I2C MUX and Arbiter
  -- Presents two WB Slave ports, one to primary crossbar, one to secondary
  -----------------------------------------------------------------------------

  cmp_secondary_crossbar : xwb_crossbar
    generic map (
      g_num_masters => c_NUM_WB2_MASTERS,
      g_num_slaves  => c_NUM_WB2_SLAVES,
      g_registered  => TRUE,
      g_address     => (0 => (others => '0')),
      g_mask        => (0 => (others => '0')))
    port map (
      clk_sys_i => clk_sys_62m5,
      rst_n_i   => rst_sys_62m5_n,
      slave_i   => cnx2_master_out,
      slave_o   => cnx2_master_in,
      master_i  => cnx2_slave_out,
      master_o  => cnx2_slave_in);

  -- link with primary crossbar
  cnx1_slave_out(c_WB_SLAVE_SECOND) <= cnx2_master_in(c_WB_MASTER_PRIM);
  cnx2_master_out(c_WB_MASTER_PRIM) <= cnx1_slave_in(c_WB_SLAVE_SECOND);

  cmp_SfpIdReader : SfpIdReader
    generic map (
      -- g_SfpWbBaseAddress is 0x1a00.
      -- X"1000" for crossbar (x"4000" >> 2 because of adapter later on)
      -- X"0a00" from I2cMuxAndExpReqArbiter
      g_SfpWbBaseAddress => 6656,
      g_WbAddrWidth      => c_wishbone_address_width)
    port map (
      Clk_ik       => clk_sys_62m5,
      SfpPlugged_i => sfp_present,
      SfpIdValid_o => sfp_det_valid,
      SfpPN_b128   => sfp_data,
      WbCyc_o      => cnx2_master_out(c_WB_MASTER_SFPID).cyc,
      WbStb_o      => cnx2_master_out(c_WB_MASTER_SFPID).stb,
      WbAddr_ob    => sfp_wb_adr,
      WbData_ib8   => cnx2_master_in(c_WB_MASTER_SFPID).dat(7 downto 0),
      WbAck_i      => cnx2_master_in(c_WB_MASTER_SFPID).ack);

  -- address adapter needed to properly access each byte in the I2CMux
  cnx2_master_out(c_WB_MASTER_SFPID).adr <= sfp_wb_adr (29 downto 0) & "00";

  -- Drive unused signals
  cnx2_master_out(c_WB_MASTER_SFPID).dat <= (others => '0');
  cnx2_master_out(c_WB_MASTER_SFPID).sel <= (others => '1');
  cnx2_master_out(c_WB_MASTER_SFPID).we  <= '0';

  cmp_I2cExpAndMuxReqArbiter : I2cExpAndMuxReqArbiter
    port map (
      Clk_ik                     => clk_sys_62m5,
      Rst_irq                    => rst_sys_62m5,
      IoExpWrReq_oq              => io_exp_wr_req,
      IoExpWrOn_i                => io_exp_wr_on,
      IoExpRdReq_oq              => io_exp_rd_req,
      IoExpRdOn_i                => io_exp_rd_on,
      IoExpAddr_oqb3             => io_exp_addr,
      IoExpRegAddr_oqb2          => io_exp_reg_addr,
      IoExpData_oqb8             => io_exp_data,
      I2cSlaveWrReq_oq           => i2c_slv_wr_req,
      I2cSlaveWrOn_i             => i2c_slv_wr_on,
      I2cSlaveRdReq_oq           => i2c_slv_rd_req,
      I2cSlaveRdOn_i             => i2c_slv_rd_on,
      I2cMuxAddress_oq           => i2c_mux_addr,
      I2cMuxChannel_oqb2         => i2c_mux_channel,
      I2cSlaveAddr_oqb7          => i2c_slv_addr,
      I2cSlaveRegAddr_oqb8       => i2c_slv_reg_addr,
      I2cSlaveByte_oqb8          => i2c_slv_byte,
      MasterBusy_i               => i2c_mst_busy,
      MasterNewByteRead_ip       => i2c_mst_dav,
      MasterByteOut_ib8          => i2c_mst_data,
      MasterAckError_i           => i2c_mst_ack_err,
      IoExpApp12Int_ian          => '1',
      IoExpApp34Int_ian          => '1',
      IoExpBstEthInt_ian         => io_exp_irq_bsteth_n_i,
      IoExpLosInt_ian            => io_exp_irq_los_n_i,
      IoExpBlmInInt_ian          => '1',
      InitDone_oq                => io_exp_init_done,
      VmeGa_onqb5                => vme_ga(4 downto 0),
      VmeGaP_onq                 => vme_ga(5),
      Led_ib8                    => vfchd_led,
      StatusLed_ob8              => open,
      GpIo1A2B_i                 => '0',
      EnGpIo1Term_i              => '0',
      GpIo2A2B_i                 => '0',
      EnGpIo2Term_i              => '0',
      GpIo34A2B_i                => '1',
      EnGpIo3Term_i              => '0',
      EnGpIo4Term_i              => '0',
      StatusGpIo1A2B_oq          => open,
      StatusEnGpIo1Term_oq       => open,
      StatusGpIo2A2B_oq          => open,
      StatusEnGpIo2Term_oq       => open,
      StatusGpIo34A2B_oq         => open,
      StatusEnGpIo3Term_oq       => open,
      StatusEnGpIo4Term_oq       => open,
      BlmIn_oqb8                 => open,
      AppSfp1Present_oq          => open,
      AppSfp1Id_oq16             => open,
      AppSfp1TxFault_oq          => open,
      AppSfp1Los_oq              => open,
      AppSfp1TxDisable_i         => '0',
      AppSfp1RateSelect_i        => '0',
      StatusAppSfp1TxDisable_oq  => open,
      StatusAppSfp1RateSelect_oq => open,
      AppSfp2Present_oq          => open,
      AppSfp2Id_oq16             => open,
      AppSfp2TxFault_oq          => open,
      AppSfp2Los_oq              => open,
      AppSfp2TxDisable_i         => '0',
      AppSfp2RateSelect_i        => '0',
      StatusAppSfp2TxDisable_oq  => open,
      StatusAppSfp2RateSelect_oq => open,
      AppSfp3Present_oq          => open,
      AppSfp3Id_oq16             => open,
      AppSfp3TxFault_oq          => open,
      AppSfp3Los_oq              => open,
      AppSfp3TxDisable_i         => '0',
      AppSfp3RateSelect_i        => '0',
      StatusAppSfp3TxDisable_oq  => open,
      StatusAppSfp3RateSelect_oq => open,
      AppSfp4Present_oq          => open,
      AppSfp4Id_oq16             => open,
      AppSfp4TxFault_oq          => open,
      AppSfp4Los_oq              => open,
      AppSfp4TxDisable_i         => '0',
      AppSfp4RateSelect_i        => '0',
      StatusAppSfp4TxDisable_oq  => open,
      StatusAppSfp4RateSelect_oq => open,
      BstSfpPresent_oq           => open,
      BstSfpId_oq16              => open,
      BstSfpTxFault_oq           => open,
      BstSfpLos_oq               => open,
      BstSfpTxDisable_i          => '0',
      BstSfpRateSelect_i         => '0',
      StatusBstSfpTxDisable_oq   => open,
      StatusBstSfpRateSelect_oq  => open,
      EthSfpPresent_oq           => sfp_present,
      EthSfpId_oq16              => open,
      EthSfpTxFault_oq           => sfp_tx_fault,
      EthSfpLos_oq               => sfp_los,
      EthSfpTxDisable_i          => sfp_tx_disable,
      EthSfpRateSelect_i         => '1',
      StatusEthSfpTxDisable_oq   => open,         -- TODO
      StatusEthSfpRateSelect_oq  => open,
      CdrLos_oq                  => open,
      CdrLol_oq                  => open,
      I2cWbCyc_i                 => cnx2_slave_in(c_WB_SLAVE_I2CMUX).cyc,
      I2cWbStb_i                 => cnx2_slave_in(c_WB_SLAVE_I2CMUX).stb,
      I2cWbWe_i                  => cnx2_slave_in(c_WB_SLAVE_I2CMUX).we,
      I2cWbAdr_ib12              => i2c_wb_adr,
      I2cWbDat_ib8               => i2c_wb_dat_in,
      I2cWbDat_ob8               => i2c_wb_dat_out,
      I2cWbAck_o                 => cnx2_slave_out(c_WB_SLAVE_I2CMUX).ack,
      WbCyc_i                    => cnx1_slave_in(c_WB_SLAVE_I2CCFG).cyc,
      WbStb_i                    => cnx1_slave_in(c_WB_SLAVE_I2CCFG).stb,
      WbWe_i                     => cnx1_slave_in(c_WB_SLAVE_I2CCFG).we,
      WbDat_ib32                 => cnx1_slave_in(c_WB_SLAVE_I2CCFG).dat,
      WbDat_oqb32                => cnx1_slave_out(c_WB_SLAVE_I2CCFG).dat,
      WbAck_oa                   => cnx1_slave_out(c_WB_SLAVE_I2CCFG).ack);

  -- Adjust WB interface to VFCHD I2CMux expectations and drive unused signals
  -- Adr(13 downto 10) are used to select the I2C peripheral in the mux
  -- Adr(9 downto 2) are the I2C address of that peripheral
  -- Adr(1 downto 0) are dropped (to allow access to the individual bytes
  -- since I2CMux only provides an 8-bit output)
  i2c_wb_adr    <= cnx2_slave_in(c_WB_SLAVE_I2CMUX).adr(13 downto 2);
  i2c_wb_dat_in <= cnx2_slave_in(c_WB_SLAVE_I2CMUX).dat(7 downto 0);

  cnx2_slave_out(c_WB_SLAVE_I2CMUX).dat   <= X"000000" & i2c_wb_dat_out;
  cnx2_slave_out(c_WB_SLAVE_I2CMUX).err   <= '0';
  cnx2_slave_out(c_WB_SLAVE_I2CMUX).rty   <= '0';
  cnx2_slave_out(c_WB_SLAVE_I2CMUX).int   <= '0';
  cnx2_slave_out(c_WB_SLAVE_I2CMUX).stall <= not cnx2_slave_out(c_WB_SLAVE_I2CMUX).ack and
                                             (cnx2_slave_in(c_WB_SLAVE_I2CMUX).stb and
                                              cnx2_slave_in(c_WB_SLAVE_I2CMUX).cyc);

  cnx1_slave_out(c_WB_SLAVE_I2CCFG).err   <= '0';
  cnx1_slave_out(c_WB_SLAVE_I2CCFG).rty   <= '0';
  cnx1_slave_out(c_WB_SLAVE_I2CCFG).int   <= '0';
  cnx1_slave_out(c_WB_SLAVE_I2CCFG).stall <= not cnx1_slave_out(c_WB_SLAVE_I2CCFG).ack and
                                             (cnx1_slave_in(c_WB_SLAVE_I2CCFG).stb and
                                              cnx1_slave_in(c_WB_SLAVE_I2CCFG).cyc);

  cmp_I2cExpAndMuxMaster : I2cExpAndMuxMaster
    generic map (
      g_SclHalfPeriod => "0010100000")            -- 10'd160
    port map (
      Clk_ik              => clk_sys_62m5,
      Rst_irq             => rst_sys_62m5,
      IoExpWrReq_i        => io_exp_wr_req,
      IoExpWrOn_oq        => io_exp_wr_on,
      IoExpRdReq_i        => io_exp_rd_req,
      IoExpRdOn_oq        => io_exp_rd_on,
      IoExpAddr_ib3       => io_exp_addr,
      IoExpRegAddr_ib2    => io_exp_reg_addr,
      IoExpData_ib8       => io_exp_data,
      I2cSlaveWrReq_i     => i2c_slv_wr_req,
      I2cSlaveWrOn_o      => i2c_slv_wr_on,
      I2cSlaveRdReq_i     => i2c_slv_rd_req,
      I2cSlaveRdOn_o      => i2c_slv_rd_on,
      I2cMuxAddress_i     => i2c_mux_addr,
      I2cMuxChannel_ib2   => i2c_mux_channel,
      I2cSlaveAddr_ib7    => i2c_slv_addr,
      I2cSlaveRegAddr_ib8 => i2c_slv_reg_addr,
      I2cSlaveByte_ib8    => i2c_slv_byte,
      Busy_o              => i2c_mst_busy,
      NewByteRead_op      => i2c_mst_dav,
      ByteOut_ob8         => i2c_mst_data,
      AckError_op         => i2c_mst_ack_err,
      Scl_ioz             => i2c_mux_scl_b,
      Sda_ioz             => i2c_mux_sda_b);

  -----------------------------------------------------------------------------
  -- Remaining DIO, VFC GPIO and LEDs
  -----------------------------------------------------------------------------

  -- extend pulse for PPS LEDs
  cmp_gc_extend_pulse1 : gc_extend_pulse
    generic map (
      g_width => 5000000)
    port map (
      clk_i      => clk_sys_62m5,
      rst_n_i    => rst_sys_62m5_n,
      pulse_i    => pps_led,
      extended_o => pps_led_d);

  -- assign LEDs
  vfchd_led <= (0      => wr_led_link,
                4      => wr_led_act,
                others => '0');

  -- Debug: Reference clock to GPIO output
  process(clk_ref_125m)
  begin
    if rising_edge(clk_ref_125m) then
      clk_ref_div2 <= not clk_ref_div2;
    end if;
  end process;

  vfchd_gpio4_o <= clk_ref_div2;

  -- Configure DIO LEMO1 and LEMO5 as inputs
  dio4_term_en_o <= '0';
  dio4_oe_n_o    <= '1';
  pps_ext_in     <= dio4_i;
  dio5_term_en_o <= '0';
  dio5_oe_n_o    <= '1';
  clk_ext_ref    <= dio5_clk_i;
  dio_led_term_o <= '0';
  dio_led_out_o  <= pps_led_d;

  fmc_enable_n_o <= '0';

end architecture top;
