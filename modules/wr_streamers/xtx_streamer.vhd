-------------------------------------------------------------------------------
-- Title      : Simple Ethernet Data Streamer
-- Project    : White Rabbit Hands-On Course
-------------------------------------------------------------------------------
-- File       : xtx_streamer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2012-11-02
-- Platform   : FPGA-generic
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description: A simple core demonstrating how to encapsulate a continuous
-- stream of data words into Ethernet frames, in a format that is accepted by
-- the White Rabbit PTP core. More info in the documentation.
-------------------------------------------------------------------------------
-- Copyright (c) 2013 CERN BE-CO-HT.
-- Licensed under LGPL 2.1.
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                Description
-- 2012-11-02  1.0      Tomasz Wlostowski     Created
-- 2016-03-30  2.0      Maciej Lipinski       made B-train backward compatible 
--                                            & debugged
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;
use work.gencores_pkg.all;
use work.genram_pkg.all;

entity xtx_streamer is
  
  generic (
    -- Width of data words on tx_data_i.
    g_data_width : integer := 32;

    -- Minimum number of data words in the TX buffer that will trigger transmission of an
    -- Ethernet frame. Also defines the buffer size (2 * g_tx_threshold). Note
    -- that in order for a frame to be transmitted, the buffer must conatain at
    -- least one complete block.
    g_tx_threshold : integer := 128;


    -- Maximum number of data words in a single Ethernet frame. It also defines
    -- the maximum block size (since blocks can't be currently split across
    -- multiple frames). 
    g_tx_max_words_per_frame : integer := 128;
    
    -- Transmission timeout (in clk_sys_i cycles), after which the contents
    -- of TX buffer are sent regardless of the amount of data that is currently
    -- stored in the buffer, so that data in the buffer does not get stuck.
    g_tx_timeout : integer := 1024;
    
    -- DO NOT USE unless you know what you are doing
    -- legacy stuff: the streamers initially used in Btrain did not check/insert the escape
    -- code. This is justified if only one block of a known number of words is sent/expected
    g_escape_code_disable : boolean := FALSE
    );

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    -- Endpoint/WRC interface - packet source
    src_i : in  t_wrf_source_in;
    src_o : out t_wrf_source_out;

    ---------------------------------------------------------------------------
    -- WRC Timing interface, used for latency measurement
    -- Caution: uses clk_ref_i clock domain!
    ---------------------------------------------------------------------------

    -- White Rabbit reference clock
    clk_ref_i : in std_logic := '0';

    -- Time valid flag
    tm_time_valid_i : in std_logic := '0';

    -- TAI seconds
    tm_tai_i : in std_logic_vector(39 downto 0) := x"0000000000";

    -- Fractional part of the second (in clk_ref_i cycles)
    tm_cycles_i : in std_logic_vector(27 downto 0) := x"0000000";

    ---------------------------------------------------------------------------
    -- User interface
    ---------------------------------------------------------------------------

    -- Data word to be sent.
    tx_data_i : in std_logic_vector(g_data_width-1 downto 0);

    -- 1 indicates that the tx_data_i contains a valid data word.
    tx_valid_i : in std_logic;

    -- Synchronous data request: if active, the user may send a data word in
    -- the following clock cycle.
    tx_dreq_o : out std_logic;

    -- Last signal. Can be used to indicate the last data word in a larger
    -- block of samples (see documentation for more details).
    tx_last_p1_i : in std_logic := '1';

    -- Flush input. When asserted, the streamer will immediatly send out all
    -- the data that is stored in its TX buffer, ignoring g_tx_timeout.
    tx_flush_p1_i : in std_logic := '0';

    -- Reset sequence number. When asserted, the internal sequence number
    -- generator used to detect loss of frames is reset to 0. Advanced feature.
    tx_reset_seq_i : in std_logic := '0';
    -- successfully sent streamer frame
    tx_frame_p1_o : out std_logic;
    ---------------------------------------------------------------------------
    -- Configuration
    ---------------------------------------------------------------------------

    -- Local MAC address. Leave at 0 when using with the WR MAC/Core, it will
    -- insert its own source MAC.
    cfg_mac_local_i : in std_logic_vector(47 downto 0) := x"000000000000";

    -- Destination MAC address.
    cfg_mac_target_i : in std_logic_vector(47 downto 0);

    -- Ethertype of our frames. Default value is accepted by standard
    -- configuration of the WR PTP Core
    cfg_ethertype_i : in std_logic_vector(15 downto 0) := x"dbff"
    );

end xtx_streamer;

architecture rtl of xtx_streamer is

  type t_pipe is record
    dvalid : std_logic;
    dreq   : std_logic;
    sof    : std_logic;
    eof    : std_logic;
    error  : std_logic;
    data   : std_logic_vector(15 downto 0);
  end record;

  component xwb_fabric_source
    port (
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      src_i     : in  t_wrf_source_in;
      src_o     : out t_wrf_source_out;
      addr_i    : in  std_logic_vector(1 downto 0);
      data_i    : in  std_logic_vector(15 downto 0);
      dvalid_i  : in  std_logic;
      sof_i     : in  std_logic;
      eof_i     : in  std_logic;
      error_i   : in  std_logic;
      bytesel_i : in  std_logic;
      dreq_o    : out std_logic);
  end component;

  component gc_escape_inserter
    generic (
      g_data_width  : integer;
      g_escape_code : std_logic_vector);
    port (
      clk_i             : in  std_logic;
      rst_n_i           : in  std_logic;
      d_i               : in  std_logic_vector(g_data_width-1 downto 0);
      d_insert_enable_i : in  std_logic;
      d_escape_i        : in  std_logic;
      d_valid_i         : in  std_logic;
      d_req_o           : out std_logic;
      d_o               : out std_logic_vector (g_data_width-1 downto 0);
      d_valid_o         : out std_logic;
      d_req_i           : in  std_logic);
  end component;

  component pulse_stamper
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

  type t_tx_state is (IDLE, SOF, ETH_HEADER, FRAME_SEQ_ID, SUBFRAME_HEADER, PAYLOAD, CRC_WORD, PADDING, EOF);

  constant c_min_packet_size : integer := 32;

  signal tx_threshold_hit : std_logic;
  signal tx_timeout_hit   : std_logic;
  signal tx_flush_latched : std_logic;

  signal tx_fifo_last, tx_fifo_we, tx_fifo_full, tx_fifo_empty, tx_fifo_rd : std_logic;
  signal tx_fifo_q, tx_fifo_d                                              : std_logic_vector(g_data_width downto 0);
  signal state                                                             : t_tx_state;
  signal seq_no, count                                                     : unsigned(14 downto 0);
  signal ser_count                                                         : unsigned(7 downto 0);
  signal word_count                                                        : unsigned(11 downto 0); --2^12 = 4096*2 bytes (can accommodate jambo frame)
  signal total_words                                                       : unsigned(10 downto 0);

  signal timeout_counter : unsigned(11 downto 0);

  signal pack_data : std_logic_vector(15 downto 0);

  signal fsm_out, escaper, fab_src     : t_pipe;
  signal fsm_escape, fsm_escape_enable : std_logic;

  signal crc_en, crc_en_masked, crc_reset : std_logic;
  signal crc_value                        : std_logic_vector(15 downto 0);

  signal tx_almost_empty, tx_almost_full : std_logic;

  signal buf_frame_count : unsigned(5 downto 0) := (others => '0');


  signal tag_cycles                   : std_logic_vector(27 downto 0);
  signal tag_valid, tag_valid_latched : std_logic;

  signal reset_dly : std_logic;

begin  -- rtl
  
  U_tx_crc_generator : gc_crc_gen
    generic map (
      g_polynomial              => x"1021",
      g_init_value              => x"ffff",
      g_residue                 => x"0000",
      g_data_width              => 16,
      g_sync_reset              => 1,
      g_dual_width              => 0,
      g_registered_match_output => false,
      g_registered_crc_output   => false)
    port map (
      clk_i     => clk_sys_i,
      rst_i     => crc_reset,
      restart_i => '0',
      en_i      => crc_en_masked,
      data_i    => fsm_out.data,
      half_i    => '0',
      crc_o     => crc_value);

  crc_en_masked <= crc_en and fsm_out.dvalid;

  U_Fab_Source : xwb_fabric_source
    port map (
      clk_i     => clk_sys_i,
      rst_n_i   => rst_n_i,
      src_i     => src_i,
      src_o     => src_o,
      addr_i    => c_WRF_DATA,
      data_i    => fab_src.data,
      dvalid_i  => fab_src.dvalid,
      sof_i     => fab_src.sof,
      eof_i     => fab_src.eof,
      error_i   => '0',
      bytesel_i => '0',
      dreq_o    => fab_src.dreq);

  fab_src.sof <= fsm_out.sof;
  fab_src.eof <= fsm_out.eof;

  
  gen_escape: if (g_escape_code_disable = FALSE) generate
    U_Insert_Escape : gc_escape_inserter
      generic map (
        g_data_width  => 16,
        g_escape_code => x"cafe")
      port map (
        clk_i             => clk_sys_i,
        rst_n_i           => rst_n_i,
        d_i               => fsm_out.data,
        d_insert_enable_i => fsm_escape_enable,
        d_escape_i        => fsm_escape,
        d_valid_i         => fsm_out.dvalid,
        d_req_o           => fsm_out.dreq,

        d_o       => fab_src.data,
        d_valid_o => fab_src.dvalid,
        d_req_i   => fab_src.dreq);
  end generate gen_escape;
  gen_no_escape: if (g_escape_code_disable = TRUE) generate
    fab_src.data   <= fsm_out.data;
    fab_src.dvalid <= fsm_out.dvalid;
    fsm_out.dreq   <= fab_src.dreq;
  end generate gen_no_escape;


  tx_fifo_we <= tx_valid_i and not tx_fifo_full;
  tx_fifo_d  <= tx_last_p1_i & tx_data_i;

  U_TX_Buffer : generic_sync_fifo
    generic map (
      g_data_width             => g_data_width + 1,
      g_size                   => 2 * g_tx_threshold,
      g_with_almost_full       => true,
      g_with_almost_empty      => true,
      g_almost_empty_threshold => g_tx_threshold,
      g_almost_full_threshold  => 2*g_tx_threshold - 2,
      g_show_ahead             => true)
    port map (
      rst_n_i        => rst_n_i,
      clk_i          => clk_sys_i,
      d_i            => tx_fifo_d,
      we_i           => tx_fifo_we,
      q_o            => tx_fifo_q,
      rd_i           => tx_fifo_rd,
      empty_o        => tx_fifo_empty,
      full_o         => tx_fifo_full,
      almost_empty_o => tx_almost_empty,
      almost_full_o  => tx_almost_full
      );

  tx_threshold_hit <= '1' when tx_almost_empty = '0' and (buf_frame_count /= 0) else '0';
  tx_fifo_last     <= tx_fifo_q(g_data_width);

  U_Timestamper : pulse_stamper
    port map (
      clk_ref_i       => clk_ref_i,
      clk_sys_i       => clk_sys_i,
      rst_n_i         => rst_n_i,
      pulse_a_i       => fsm_out.sof,
      tm_time_valid_i => tm_time_valid_i,
      tm_tai_i        => tm_tai_i,
      tm_cycles_i     => tm_cycles_i,
      tag_tai_o       => open,
      tag_cycles_o    => tag_cycles,
      tag_valid_o     => tag_valid);

  p_latch_tx_tag : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' or state = IDLE then
        tag_valid_latched <= '0';
      elsif(tag_valid = '1') then
        tag_valid_latched <= '1';
      end if;
    end if;
  end process;

  p_frame_counter : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        buf_frame_count <= (others => '0');
      else
        if(tx_fifo_we = '1' and tx_last_p1_i = '1' and (tx_fifo_rd = '0' or tx_fifo_last = '0')) then
          buf_frame_count <= buf_frame_count+ 1;
        elsif((tx_fifo_we = '0' or tx_last_p1_i = '0') and (tx_fifo_rd = '1' and tx_fifo_last = '1')) then
          buf_frame_count <= buf_frame_count - 1;
        end if;
      end if;
    end if;

  end process;

  p_tx_timeout : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        timeout_counter <= (others => '0');
        tx_timeout_hit  <= '0';
      else
        if(tx_fifo_empty = '0' and tx_threshold_hit = '0') then
          timeout_counter <= timeout_counter + 1;
        else
          timeout_counter <= (others => '0');
        end if;

        if(timeout_counter = g_tx_timeout) then
          tx_timeout_hit <= '1';
        else
          tx_timeout_hit <= '0';
        end if;
      end if;
    end if;
  end process;

  p_latch_tx_flush : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        tx_flush_latched <= '0';
      else
        if(state = IDLE) then
          tx_flush_latched <= tx_flush_p1_i or tx_timeout_hit;
        else
          tx_flush_latched <= '0';
        end if;
      end if;
    end if;
  end process;

  p_fsm : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state          <= IDLE;
        fsm_out.sof    <= '0';
        fsm_out.eof    <= '0';
        fsm_out.dvalid <= '0';
        count          <= (others => '0');
        seq_no         <= (others => '0');
        word_count     <= (others => '0');
        crc_reset      <= '1';
        tx_frame_p1_o     <= '0';
      else
        if(tx_reset_seq_i = '1') then
          seq_no <= (others => '0');
        end if;

        case state is
          when IDLE =>
            crc_en         <= '0';
            crc_reset      <= '0';
            fsm_out.eof    <= '0';
            tx_frame_p1_o  <= '0';

            if(fsm_out.dreq = '1' and (tx_flush_latched = '1' or tx_flush_p1_i = '1' or tx_threshold_hit = '1')) then
              state       <= SOF;
              fsm_out.sof <= '1';
            end if;

            fsm_escape_enable <= '0';
            fsm_escape        <= '0';

          when SOF =>
            fsm_out.sof <= '0';
            ser_count   <= (others => '0');
            state       <= ETH_HEADER;
            count       <= (others => '0');
            word_count  <= (others => '0');

          when ETH_HEADER =>
            if(fsm_out.dreq = '1') then
              case count(7 downto 0) is
                when x"00" =>
                  fsm_out.data <= cfg_mac_target_i(47 downto 32);
                  count        <= count + 1;
                when x"01" =>
                  fsm_out.data <= cfg_mac_target_i(31 downto 16);
                  count        <= count + 1;
                when x"02" =>
                  fsm_out.data <= cfg_mac_target_i(15 downto 0);
                  count        <= count + 1;
                when x"03" =>
                  fsm_out.data <= cfg_mac_local_i(47 downto 32);
                  count        <= count + 1;
                when x"04" =>
                  fsm_out.data <= cfg_mac_local_i(31 downto 16);
                  count        <= count + 1;
                when x"05" =>
                  fsm_out.data <= cfg_mac_local_i(15 downto 0);
                  count        <= count + 1;
                when x"06" =>
                  fsm_out.data <= cfg_ethertype_i;
                  count        <= count + 1;
                when x"07" =>
                  fsm_out.data <= tag_valid_latched & "000" & tag_cycles(27 downto 16);
                  count        <= count + 1;
                when x"08" =>
                  fsm_out.data <= tag_cycles(15 downto 0);
                  count        <= count + 1;
                  state        <= FRAME_SEQ_ID;
                when others =>
                  fsm_out.data <= (others => 'X');
                  count        <= (others => 'X');
              end case;
              fsm_out.dvalid <= '1';
            else
              fsm_out.dvalid <= '0';
            end if;
          when FRAME_SEQ_ID =>
            if(fsm_out.dreq = '1') then
              fsm_out.data      <= '1' & std_logic_vector(seq_no);
              seq_no            <= seq_no + 1;
              count             <= "000" & x"001";
              fsm_out.dvalid    <= '1';
              fsm_escape        <= '0';
              fsm_escape_enable <= '1';
              crc_en            <= '1';
              crc_reset         <= '0';
              state             <= PAYLOAD;
            else
              fsm_out.dvalid    <= '0';
            end if;
          
          when SUBFRAME_HEADER =>
            crc_en    <= '1';
            crc_reset <= '0';

            if(fsm_out.dreq = '1') then
              fsm_out.dvalid    <= '1';
              fsm_escape        <= '1';
              fsm_escape_enable <= '1';
              fsm_out.data      <= '1' & std_logic_vector(count);
              count             <= count + 1;
              state             <= PAYLOAD;
            else
              fsm_out.dvalid <= '0';
              fsm_out.data   <= (others => 'X');
            end if;

          when PAYLOAD =>
            fsm_escape <= '0';

            if(fsm_out.dreq = '1') then
              -- next subframe?
              if(tx_fifo_empty = '1' or (ser_count = g_data_width/16-1 and tx_fifo_last = '1')) then
                state <= CRC_WORD;
              end if;

              if(ser_count = g_data_width/16-1) then
                word_count   <= word_count + 1;
                ser_count    <= (others => '0');
              else
                ser_count    <= ser_count + 1;
              end if;

              fsm_out.data   <= tx_fifo_q((to_integer(ser_count) + 1)* 16 -1 downto to_integer(ser_count) * 16);
              fsm_out.dvalid <= not tx_fifo_empty;
            else
              fsm_out.data   <= (others => 'X');
              fsm_out.dvalid <= '0';
            end if;

          when CRC_WORD =>
            crc_en    <= '0';
            ser_count <= (others => '0');

            if(fsm_out.dreq = '1') then
              fsm_out.dvalid <= '1';
              fsm_out.data   <= crc_value;

              crc_reset <= '1';

              if(tx_fifo_empty = '1' or word_count >= g_tx_max_words_per_frame) then
                state <= PADDING;
              else
                state <= SUBFRAME_HEADER;
              end if;
            end if;

          when PADDING =>
            if(fsm_out.dreq = '1') then
              fsm_escape     <= '1';
              fsm_out.dvalid <= '1';
              fsm_out.data   <= x"0bad"; 
              if(total_words >= c_min_packet_size) then
                state <= EOF;
              end if;
              crc_reset <= '0';
            else
              fsm_out.dvalid <= '0';
              fsm_out.data   <= (others => 'X');
            end if;
            
          when EOF =>
            fsm_out.dvalid <= '0';
            if(fsm_out.dreq = '1') then
              fsm_out.eof <= '1';
              tx_frame_p1_o  <= '1';
              state       <= IDLE;
            end if;
        end case;
      end if;
    end if;
  end process;

  p_count_words : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if fsm_out.sof = '1' then
        total_words <= (others => '0');
      elsif fsm_out.dvalid = '1' then
        total_words <= total_words +1;
      end if;
    end if;
  end process;

  p_comb_fx_fifo_read : process(state, fsm_out, ser_count)
  begin
    if(state = PAYLOAD and ser_count = g_data_width/16-1 and fsm_out.dreq = '1' and tx_fifo_empty = '0') then
      tx_fifo_rd <= '1';
    else
      tx_fifo_rd <= '0';
    end if;
  end process;

  p_delay_reset: process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        reset_dly <= rst_n_i;
      end if;
    end process;
    
  tx_dreq_o <= (not tx_almost_full) and reset_dly;

end rtl;
