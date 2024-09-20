-------------------------------------------------------------------------------
-- Title      : Deterministic Xilinx GTP wrapper - Spartan-6 top module
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : wr_gtp_phy_spartan6.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-11-18
-- Last update: 2019-11-12
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Dual channel wrapper for Xilinx Spartan-6 GTP adapted for
-- deterministic delays at 1.25 Gbps.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2010 CERN / Tomasz Wlostowski
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
--
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author    Description
-- 2010-11-18  0.4      twlostow  Initial release
-- 2011-02-07  0.5      twlostow  Verified on Spartan6 GTP (single channel only)
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

library work;
use work.gencores_pkg.all;
use work.disparity_gen_pkg.all;
use work.wishbone_pkg.all;
use work.lpdc_mdio_regs_pkg.all;
  
entity wr_gtx_phy_virtex6_lp is

  generic (
    -- set to non-zero value to speed up the simulation by reducing some delays
    g_simulation         : integer := 0;
    g_use_slave_tx_clock : integer := 0;
    g_rxclk_bufr         : boolean := false;
    g_txclk_bufr         : boolean := false;
    g_id : integer := 0
    );

  port (

    -- Reference 62.5 MHz clock input for the TX/RX logic (not the GTX itself)
    clk_ref_i : in std_logic;
    -- Reference 62.5 MHz clock for the GTX transceiver
    clk_gtx_i : in std_logic;

    -- system clock for MDIO reg interface
    clk_sys_i     : in std_logic;
    rst_sys_n_i   : in std_logic;

    -- DMTD clock for phase measurements (done in the PHY module as we need to
    -- multiplex between several GTX clock outputs)
    clk_dmtd_i : in std_logic;
    -- TX path, clk_ref_i - synchronous:

    -- data input (8 bits, not 8b10b-encoded)
    tx_data_i : in std_logic_vector(15 downto 0);

    -- 1 when tx_data_i contains a control code, 0 when it's a data byte
    tx_k_i : in std_logic_vector(1 downto 0);

    -- disparity of the currently transmitted 8b10b code (1 = plus, 0 = minus).
    -- Necessary for the PCS to generate proper frame termination sequences.
    -- Generated for the 2nd byte (LSB) of tx_data_i.
    tx_disparity_o : out std_logic;

    -- Encoding error indication (1 = error, 0 = no error)
    tx_enc_err_o : out std_logic;

    -- RX path, synchronous to ch0_rx_rbclk_o.

    -- RX recovered clock
    rx_rbclk_o : out std_logic;

    clk_sampled_o : out std_logic;

    -- 8b10b-decoded data output. The data output must be kept invalid before
    -- the transceiver is locked on the incoming signal to prevent the EP from
    -- detecting a false carrier.
    rx_data_o : out std_logic_vector(15 downto 0);

    -- 1 when the byte on rx_data_o is a control code
    rx_k_o : out std_logic_vector(1 downto 0);

    -- encoding error indication
    rx_enc_err_o : out std_logic;

    -- RX bitslide indication, indicating the delay of the RX path of the
    -- transceiver (in UIs). Must be valid when ch0_rx_data_o is valid.
    rx_bitslide_o : out std_logic_vector(4 downto 0);

    -- reset input, active hi
    rst_i      : in std_logic;
    rx_pdown_i : in std_logic := '0';
    loopen_i : in std_logic;

    pad_txn_o : out std_logic;
    pad_txp_o : out std_logic;

    pad_rxn_i : in std_logic := '0';
    pad_rxp_i : in std_logic := '0';

    rdy_o : out std_logic;

    mdio_slave_i         : in  t_wishbone_slave_in;
    mdio_slave_o         : out t_wishbone_slave_out;

    TX_CLK_o : out std_logic

    );


end wr_gtx_phy_virtex6_lp;

architecture rtl of wr_gtx_phy_virtex6_lp is

  signal rx_data_o_int : std_logic_vector(15 downto 0);
  signal rx_k_o_int : std_logic_vector(1 downto 0);
  signal rx_enc_err_o_int : std_logic;
  
  
  component WHITERABBITGTX_WRAPPER_GTX_LP
    generic (
      GTX_SIM_GTXRESET_SPEEDUP : integer;
      GTX_TX_CLK_SOURCE        : string;
      GTX_POWER_SAVE           : bit_vector);
    port (
      LOOPBACK_IN           : in  std_logic_vector(2 downto 0);
  --    RXCHARISK_OUT         : out std_logic_vector(1 downto 0);
--      RXDISPERR_OUT         : out std_logic_vector(1 downto 0);
--      RXNOTINTABLE_OUT      : out std_logic_vector(1 downto 0);
--      RXBYTEISALIGNED_OUT   : out std_logic;
  --    RXCOMMADET_OUT        : out std_logic;
--      RXSLIDE_IN            : in  std_logic;
      RXDATA_OUT            : out std_logic_vector(19 downto 0);
      RXRECCLK_OUT          : out std_logic;
      RXUSRCLK2_IN          : in  std_logic;
      RXCDRRESET_IN         : in  std_logic;
      RXN_IN                : in  std_logic;
      RXP_IN                : in  std_logic;
      GTXRXRESET_IN         : in  std_logic;
      MGTREFCLKRX_IN        : in  std_logic_vector(1 downto 0);
      PLLRXRESET_IN         : in  std_logic;
      RXPLLLKDET_OUT        : out std_logic;
      RXRESETDONE_OUT       : out std_logic;
      TXCHARISK_IN          : in  std_logic_vector(1 downto 0);
      GTXTEST_IN            : in  std_logic_vector(12 downto 0);
      TXDATA_IN             : in  std_logic_vector(15 downto 0);
      TXOUTCLK_OUT          : out std_logic;
      TXUSRCLK2_IN          : in  std_logic;
      TXRUNDISP_OUT         : out std_logic_vector(1 downto 0);
      TXN_OUT               : out std_logic;
      TXP_OUT               : out std_logic;
      TXDLYALIGNDISABLE_IN  : in  std_logic;
      TXDLYALIGNMONENB_IN   : in  std_logic;
      TXDLYALIGNMONITOR_OUT : out std_logic_vector(7 downto 0);
      TXDLYALIGNRESET_IN    : in  std_logic;
      TXENPMAPHASEALIGN_IN  : in  std_logic;
      TXPMASETPHASE_IN      : in  std_logic;
      GTXTXRESET_IN         : in  std_logic;
      MGTREFCLKTX_IN        : in  std_logic_vector(1 downto 0);
      PLLTXRESET_IN         : in  std_logic;
      TXPLLLKDET_OUT        : out std_logic;
      TXRESETDONE_OUT       : out std_logic);
  end component;

  component BUFG
    port (
      O : out std_ulogic;
      I : in  std_ulogic);
  end component;

  component BUFR
    generic (
      BUFR_DIVIDE : string := "BYPASS";
      SIM_DEVICE  : string := "VIRTEX6");
    port (
      O   : out std_ulogic;
      CE  : in  std_ulogic := '1';
      CLR : in  std_ulogic := '0';
      I   : in  std_ulogic);
  end component;

  component dmtd_sampler is
    generic (
      g_divide_input_by_2 : boolean;
      g_reverse           : boolean);
    port (
      clk_in_i      : in  std_logic;
      clk_dmtd_i    : in  std_logic;
      clk_sampled_o : out std_logic);
  end component dmtd_sampler;


  signal gtx_rst                    : std_logic;
  signal gtx_loopback               : std_logic_vector(2 downto 0);
  signal gtx_reset_done             : std_logic;
  signal gtx_pll_lockdet            : std_logic;
  signal rst_synced                 : std_logic;
  signal rst_d0                     : std_logic;
  signal reset_counter              : unsigned(9 downto 0);
  signal gtx_test                   : std_logic_vector(12 downto 0);

  signal rx_rec_clk_bufin   : std_logic;
  signal rx_rec_clk         : std_logic;
  signal rx_comma_det       : std_logic;
  signal rx_byte_is_aligned : std_logic;

  signal tx_dly_align_disable  : std_logic;
  signal tx_dly_align_reset    : std_logic;
  signal tx_en_pma_phase_align : std_logic;
  signal tx_pma_set_phase      : std_logic;

  signal align_enable : std_logic;
  signal align_done   : std_logic;

  signal gtx_tx_rst_done, rx_rst_done : std_logic;

  signal txpll_lockdet, rxpll_lockdet : std_logic;
  signal pll_lockdet                  : std_logic;
  signal serdes_ready                 : std_logic;
  signal rx_slide                     : std_logic;
  signal rx_cdr_rst                   : std_logic;
  signal rx_synced                    : std_logic;
  signal rst_done                     : std_logic;
  signal everything_ready             : std_logic;

  signal mgtrefclk_in : std_logic_vector(1 downto 0);


  signal rx_k_int    : std_logic_vector(1 downto 0);
  signal rx_data_int : std_logic_vector(15 downto 0);
  signal rx_data_raw, rx_data_raw_d : std_logic_vector(19 downto 0);
  signal rx_data_raw_gtx : std_logic_vector(19 downto 0);

  signal rx_disp_err, rx_code_err : std_logic_vector(1 downto 0);

  signal tx_is_k_swapped : std_logic_vector(1 downto 0);
  signal tx_data_swapped : std_logic_vector(15 downto 0);

  signal cur_disp                               : t_8b10b_disparity;
  signal tx_out_clk, tx_out_clk_buf             : std_logic;
  signal rx_rec_clk_sampled, tx_out_clk_sampled : std_logic;

  signal tx_rundisp_v6  : std_logic_vector(1 downto 0);
  signal gtx_tx_reset_a : std_logic;

  signal tx_reset_done : std_logic;

  signal link_up, link_aligned : std_logic;
  signal tx_enable, tx_enable_refclk : std_logic;

  signal tx_sw_reset : std_logic;
  signal rx_enable, rx_enable_rxclk : std_logic;
  signal gtx_rx_rst_a : std_logic;
  signal rx_sw_reset : std_logic;

  function f_reverse_bits(x : std_logic_vector) return std_logic_vector is
    variable rv : std_logic_vector(x'reverse_range);
  begin
    for i in x'range loop
      rv(i) := x(i);
    end loop;
    return rv;
  end f_reverse_bits;

  signal rx_rst, rx_rst_n : std_logic;
  signal rx_pdown_d0, rx_pdown_synced : std_logic;

  signal dbg_rst : std_logic;
  signal dbg_shift_en, dbg_shift_en_p: std_logic;
  signal dbg_data : std_logic;
  signal dbg_trig, dbg_trig_p : std_logic;

  signal dbg_reg : std_logic_vector(63 downto 0);
  
  signal rx_cdr_reset_a : std_logic;
  signal pll_tx_reset_a : std_logic;
  signal pll_rx_reset_a : std_logic;
  signal cd_reset : std_logic;

  signal lpdc_regs_out      : t_lpdc_regs_master_out;
  signal lpdc_regs_in       : t_lpdc_regs_master_in;
  signal drp_regs_in        : t_wishbone_slave_out;
  signal drp_regs_out       : t_wishbone_slave_in;
  
begin  -- rtl

  U_LPDC_regs : entity work.lpdc_mdio_regs
    port map (
      rst_n_i     => rst_sys_n_i,
      clk_i       => clk_sys_i,
      wb_i        => mdio_slave_i,
      wb_o        => mdio_slave_o,
      lpdc_regs_i => lpdc_regs_in,
      lpdc_regs_o => lpdc_regs_out,
      drp_regs_i  => drp_regs_in,
      drp_regs_o  => drp_regs_out);

  tx_sw_reset <= lpdc_regs_out.CTRL_tx_sw_reset;
  tx_enable   <= lpdc_regs_out.CTRL_tx_enable;
  rx_enable   <= lpdc_regs_out.CTRL_rx_enable;
  rx_sw_reset <= lpdc_regs_out.CTRL_rx_sw_reset or rx_rst;
  rx_cdr_reset_a <= lpdc_regs_out.CTRL_rx_sw_reset;
  pll_tx_reset_a <= lpdc_regs_out.CTRL_pll_sw_reset;
  pll_rx_reset_a <= lpdc_regs_out.CTRL_pll_sw_reset;

  dbg_data <= dbg_reg(0);

  cd_reset <= rx_sw_reset;

  U_SyncDBG: gc_sync_ffs
    port map (
      clk_i => rx_rec_clk,
      rst_n_i => '1',
      data_i => dbg_trig,
      ppulse_o => dbg_trig_p );

  U_SyncDBG2: gc_sync_ffs
    port map (
      clk_i => rx_rec_clk,
      rst_n_i => '1',
      data_i => dbg_shift_en,
      ppulse_o => dbg_shift_en_p );
  
  U_SyncTxEnable : gc_sync_ffs
    port map
    (
      clk_i    => clk_ref_i,
      rst_n_i  => '1',
      data_i   => tx_enable,
      synced_o => tx_enable_refclk
      );

  U_SyncRxEnable : gc_sync_ffs
    port map
    (
      clk_i    => rx_rec_clk,
      rst_n_i  => '1',
      data_i   => rx_enable,
      synced_o => rx_enable_rxclk
      );

  U_SyncRxReset : gc_sync_ffs
    port map
    (
      clk_i    => clk_dmtd_i,
      rst_n_i  => '1',
      data_i   => rx_sw_reset,
      synced_o => gtx_rx_rst_a
      );

  gen_bufr_for_tx_clock :  if g_txclk_bufr generate
  BUFR_TX : BUFR
    port map (
      O => tx_out_clk,
      I => tx_out_clk_buf);
  end generate gen_bufr_for_tx_clock;

  gen_bufg_for_tx_clock :  if not g_txclk_bufr generate
  BUFG_TX : BUFG
    port map (
      O => tx_out_clk,
      I => tx_out_clk_buf);
  end generate gen_bufg_for_tx_clock;

  U_Sampler_RX : dmtd_sampler
    generic map (
      g_divide_input_by_2 => false,
      g_reverse           => true)
    port map (
      clk_in_i      => rx_rec_clk,
      clk_dmtd_i    => clk_dmtd_i,
      clk_sampled_o => rx_rec_clk_sampled);

  U_Sampler_TX : dmtd_sampler
    generic map (
      g_divide_input_by_2 => false,
      g_reverse           => true)
    port map (
      clk_in_i      => tx_out_clk,
      clk_dmtd_i    => clk_dmtd_i,
      clk_sampled_o => tx_out_clk_sampled);

  clk_sampled_o <= rx_rec_clk_sampled when lpdc_regs_out.CTRL_dmtd_clk_sel = "00" else
                   tx_out_clk_sampled when lpdc_regs_out.CTRL_dmtd_clk_sel = "01" else
                   '0';


  tx_enc_err_o <= '0';

  -- Near-end PMA loopback if loopen_i active
  gtx_loopback <= "010" when loopen_i = '1' else "000";

  p_gen_reset : process(clk_ref_i)
  begin
    if rising_edge(clk_ref_i) then

      rst_d0     <= rst_i;
      rst_synced <= rst_d0;

      if(rst_synced = '1') then
        reset_counter <= (others => '0');
      else
        if(reset_counter(reset_counter'left) = '0') then
          reset_counter <= reset_counter + 1;
        end if;
      end if;
    end if;
  end process;

  gtx_rst <= rst_synced or std_logic(not reset_counter(reset_counter'left));

  p_gen_rx_pdown : process(clk_ref_i)
  begin
    if rising_edge(clk_ref_i) then

      rx_pdown_d0     <= rx_pdown_i;
      rx_pdown_synced <= rx_pdown_d0;

    end if;
  end process;

  rx_rst   <= gtx_rst or rx_pdown_synced;
  rx_rst_n <= not rx_rst;
  
  U_Tx_Reset_Gen : entity work.gtx_tx_reset_lp
    port map (
      clk_dmtd_i          => clk_dmtd_i,
      clk_tx_i            => clk_ref_i,
      rst_i               => gtx_rst,
      rst_sw_i            => tx_sw_reset,
      txpll_lockdet_i     => txpll_lockdet,
      gtx_test_o          => gtx_test,
      gtx_tx_reset_o      => gtx_tx_reset_a,
      gtx_tx_reset_done_i => gtx_tx_rst_done,
      done_o              => tx_reset_done);

  lpdc_regs_in.STAT_pll_locked        <= pll_lockdet;
  lpdc_regs_in.STAT_link_up           <= link_up;
  lpdc_regs_in.STAT_link_aligned      <= link_aligned;
  lpdc_regs_in.STAT_tx_rst_done       <= tx_reset_done;
  lpdc_regs_in.STAT_txusrpll_locked   <= txpll_lockdet;
  lpdc_regs_in.STAT_rx_rst_done       <= rx_rst_done;
  lpdc_regs_in.STAT_comma_current_pos <= (others => '0');
  lpdc_regs_in.STAT_comma_pos_valid   <= '0';
  
  gen_rx_bufg : if(g_rxclk_bufr = false) generate

    U_BUF_RxRecClk : BUFG
      port map (
        I => rx_rec_clk_bufin,
        O => rx_rec_clk);

  end generate gen_rx_bufg;

  gen_rx_bufr : if(g_rxclk_bufr = true) generate
    U_BUF_RxRecClk : BUFR
      port map (
        I => rx_rec_clk_bufin,
        O => rx_rec_clk);
  end generate gen_rx_bufr;


  rx_rbclk_o <= rx_rec_clk;

  process(clk_ref_i)
  begin
    if rising_edge(clk_ref_i) then
      if tx_enable_refclk = '0' then
        tx_is_k_swapped <= "00";
        tx_data_swapped <= (others => '0');
      else
        tx_is_k_swapped <= tx_k_i(0) & tx_k_i(1);
        tx_data_swapped <= tx_data_i(7 downto 0) & tx_data_i(15 downto 8);
      end if;
      
    end if;
    
  end process;

  U_GTX_INST : WHITERABBITGTX_WRAPPER_GTX_LP
    generic map (
      GTX_SIM_GTXRESET_SPEEDUP => 1,
      GTX_TX_CLK_SOURCE        => "TXPLL",
      GTX_POWER_SAVE           => "0000110000")
    port map (
      LOOPBACK_IN         => gtx_loopback,
--      RXCHARISK_OUT       => rx_k_int,
--      RXDISPERR_OUT       => rx_disp_err,
--      RXNOTINTABLE_OUT    => rx_code_err,
--      RXBYTEISALIGNED_OUT => open,
--      RXCOMMADET_OUT      => open,
--      RXSLIDE_IN          => '0',
      RXDATA_OUT          => rx_data_raw_gtx,
      RXRECCLK_OUT        => rx_rec_clk_bufin,
      RXUSRCLK2_IN        => rx_rec_clk,
      RXCDRRESET_IN       => rx_cdr_reset_a,
      RXN_IN              => pad_rxn_i,
      RXP_IN              => pad_rxp_i,
      GTXRXRESET_IN       => gtx_rx_rst_a,
      MGTREFCLKRX_IN      => mgtrefclk_in,
      PLLRXRESET_IN       => pll_rx_reset_a,
      RXPLLLKDET_OUT      => rxpll_lockdet,
      RXRESETDONE_OUT     => rx_rst_done,
      TXCHARISK_IN        => tx_is_k_swapped,
      GTXTEST_IN          => gtx_test,
      TXDATA_IN           => tx_data_swapped,
      TXOUTCLK_OUT        => tx_out_clk_buf,
      TXUSRCLK2_IN        => clk_ref_i,
      TXRUNDISP_OUT       => open,
      TXN_OUT             => pad_txn_o,

      TXP_OUT               => pad_txp_o,
      TXDLYALIGNDISABLE_IN  => '1',
      TXDLYALIGNMONENB_IN   => '1',
      TXDLYALIGNMONITOR_OUT => open,
      TXDLYALIGNRESET_IN    => '0',
      TXENPMAPHASEALIGN_IN  => '0',
      TXPMASETPHASE_IN      => '0',
      GTXTXRESET_IN         => gtx_tx_reset_a,
      MGTREFCLKTX_IN        => mgtrefclk_in,
      PLLTXRESET_IN         => pll_tx_reset_a,
      TXPLLLKDET_OUT        => txpll_lockdet,
      TXRESETDONE_OUT       => gtx_tx_rst_done);

  RX_DAT_ANTI_META: gc_sync_register
  generic map (
    g_width => 20)
  port map (
    clk_i     => rx_rec_clk,
    rst_n_a_i => '1', --gtx_rst,
    d_i       => rx_data_raw_gtx,
    q_o       => rx_data_raw);

  mgtrefclk_in <= '0' & clk_gtx_i;

  rx_synced <= '0';

  rst_done         <= rx_rst_done and tx_reset_done;
  pll_lockdet      <= txpll_lockdet and rxpll_lockdet;
  serdes_ready     <= rst_done and pll_lockdet;
  align_enable     <= serdes_ready;
  everything_ready <= serdes_ready and align_done;
  rdy_o            <= serdes_ready; --everything_ready;


  
  U_Comma_Detect : entity work.gtx_comma_detect_lp
    generic map(
      g_id => g_id
      )
    port map (
      clk_rx_i  => rx_rec_clk,
      rst_i     => cd_reset,
      rx_data_raw_i => rx_data_raw,
      link_up_o => link_up,
      aligned_o => link_aligned,
      rx_data_i =>rx_data_o_int,
      rx_k_i => rx_k_o_int,
      rx_error_i => rx_enc_err_o_int);

  p_debug_Register : process(rx_rec_clk)
  begin
    if rising_edge(rx_rec_clk) then
      rx_data_raw_d <= rx_data_raw;
      if dbg_shift_en_p = '1' then
        dbg_reg <= '0' & dbg_reg(dbg_reg'length-1 downto 1);
      elsif dbg_trig_p = '1' then
        dbg_reg (19 downto 0) <= rx_data_raw;
        dbg_reg (39 downto 20) <= rx_data_raw_d;
      end if;
    end if;
  end process;
  
  U_Dec1 : gc_dec_8b10b
    port map (
      clk_i       => rx_rec_clk,
      rst_n_i     => rx_rst_n,
      in_10b_i    => (rx_data_raw(19 downto 10)),
      ctrl_o      => rx_k_int(1),
      code_err_o  => rx_code_err(1),
      rdisp_err_o => open,
      out_8b_o    => rx_data_int(15 downto 8));

  U_Dec2 : gc_dec_8b10b
    port map (
      clk_i       => rx_rec_clk,
      rst_n_i     => rx_rst_n,
      in_10b_i    => (rx_data_raw(9 downto 0)),
      ctrl_o      => rx_k_int(0),
      code_err_o  => rx_code_err(0),
      rdisp_err_o => open,
      out_8b_o    => rx_data_int(7 downto 0));

  rx_disp_err <= (others => '0');

  p_gen_rx_outputs : process(rx_rec_clk, rx_rst)
  begin
    if(rx_rst = '1') then
      rx_data_o_int    <= (others => '0');
      rx_k_o_int       <= (others => '0');
      rx_enc_err_o_int <= '0';
    elsif rising_edge(rx_rec_clk) then
      if(rx_enable_rxclk = '1') then
        rx_data_o_int    <= rx_data_int(7 downto 0) & rx_data_int(15 downto 8);
        rx_k_o_int       <= rx_k_int(0) & rx_k_int(1);
        rx_enc_err_o_int <= rx_disp_err(0) or rx_disp_err(1) or rx_code_err(0) or rx_code_err(1);
      else
        rx_data_o_int    <= (others => '1');
        rx_k_o_int       <= (others => '1');
        rx_enc_err_o_int <= '1';
      end if;
    end if;
  end process;

  p_gen_tx_disparity : process(clk_ref_i)
  begin
    if rising_edge(clk_ref_i) then
      if tx_enable_refclk = '0' then
        cur_disp <= RD_MINUS;
      else
        cur_disp <= f_next_8b10b_disparity16(cur_disp, tx_k_i, tx_data_i);
      end if;
    end if;
  end process;

  tx_disparity_o <= to_std_logic(cur_disp);

  rx_data_o <= rx_data_o_int;
  rx_k_o <= rx_k_o_int;
  rx_enc_err_o <= '0';-- rx_enc_err_o_int;
end rtl;
