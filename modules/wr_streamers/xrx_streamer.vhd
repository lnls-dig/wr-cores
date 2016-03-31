-------------------------------------------------------------------------------
-- Title      : Simple Ethernet Data Streamer
-- Project    : White Rabbit Hands-On Course
-------------------------------------------------------------------------------
-- File       : xrx_streamer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2012-11-02
-- Platform   : FPGA-generic
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description: A simple core demonstrating how to encapsulate a continuous
-- stream of data words into Ethernet frames, in a format that is accepted by
-- the White Rabbit PTP core. This core decodes Ethernet frames encoded by
-- xtx_streamer. More info in the documentation.
-------------------------------------------------------------------------------
-- Copyright (c) 2013 CERN BE-CO-HT.
-- Licensed under LGPL 2.1.
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                Description
-- 2012-11-02  1.0      Tomasz Wlostowski     Created
-- 2016-03-31  2.0      Maciej Lipinski       made B-train backward compatible 
--                                            & debugged
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;
use work.gencores_pkg.all;
use work.genram_pkg.all;

entity xrx_streamer is
  
  generic (
    -- Width of the data words. Must be same as in the TX streamer.
    g_data_width        : integer := 32;

    -- Size of RX buffer, in data words.
    g_buffer_size       : integer := 16;

    -- When true, receives only packets whose destination MAC equals
    -- cfg_mac_remote_i. When false. accepts all incoming packets. 
    g_filter_remote_mac : boolean := false;

    -- DO NOT USE unless you know what you are doing
    -- legacy stuff: the streamers that were initially used in Btrain did not check/insert 
    -- the escape code. This is justified if only one block of a known number of words is 
    -- sent/expected.
    g_escape_code_disable : boolean := FALSE;

    -- DO NOT USE unless you know what you are doing
    -- legacy stuff: the streamers that were initially used in Btrain accepted only a fixed
    -- number of words, regardless of the frame content. If this generic is set to number
    -- other than zero, only a fixed number of words is accepted. 
    -- In combination with the g_escape_code_disable generic set to TRUE, the behaviour of
    -- the "Btrain streamers" can be recreated.
    g_expected_words_number : integer := 0
    );

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    -- Endpoint/WRC interface 
    snk_i : in  t_wrf_sink_in;
    snk_o : out t_wrf_sink_out;

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

    -- 1 indicates the 1st word of the data block on rx_data_o.
    rx_first_o         : out std_logic;
    -- 1 indicates the last word of the data block on rx_data_o.
    rx_last_o          : out std_logic;
    -- Received data.
    rx_data_o          : out std_logic_vector(g_data_width-1 downto 0);
    -- 1 indicted that rx_data_o is outputting a valid data word.
    rx_valid_o         : out std_logic;
    -- Synchronous data request input: when 1, the streamer may output another
    -- data word in the subsequent clock cycle.
    rx_dreq_i          : in  std_logic;
    -- Lost output: 1 indicates that one or more of frames have been lost.
    rx_lost_o          : out std_logic := '0';
    -- Latency measurement output: indicates the transport latency (between the
    -- TX streamer in remote device and this streamer), in clk_ref_i clock cycles.
    rx_latency_o       : out std_logic_vector(27 downto 0);
    -- 1 when the latency on rx_latency_o is valid.
    rx_latency_valid_o : out std_logic;

    -- MAC address
    cfg_mac_local_i         : in std_logic_vector(47 downto 0);
    cfg_mac_remote_i        : in std_logic_vector(47 downto 0);
    cfg_ethertype_i         : in std_logic_vector(15 downto 0) := x"dbff";
    -- 1: accept all broadcast packets
    -- 0: accept only unicasts
    cfg_accept_broadcasts_i : in std_logic                     := '1'
    );

end xrx_streamer;

architecture rtl of xrx_streamer is

  type t_pipe is record
    dvalid  : std_logic;
    dreq    : std_logic;
    sof     : std_logic;
    eof     : std_logic;
    error   : std_logic;
    data    : std_logic_vector(15 downto 0);
    addr    : std_logic_vector(1 downto 0);
    bytesel : std_logic;
  end record;

  component escape_detector
    generic (
      g_data_width  : integer;
      g_escape_code : std_logic_vector);
    port (
      clk_i             : in  std_logic;
      rst_n_i           : in  std_logic;
      d_i               : in  std_logic_vector(g_data_width-1 downto 0);
      d_detect_enable_i : in  std_logic;
      d_valid_i         : in  std_logic;
      d_req_o           : out std_logic;
      d_o               : out std_logic_vector(g_data_width-1 downto 0);
      d_escape_o        : out std_logic;
      d_valid_o         : out std_logic;
      d_req_i           : in  std_logic);
  end component;

  component dropping_buffer
    generic (
      g_size       : integer;
      g_data_width : integer);
    port (
      clk_i      : in  std_logic;
      rst_n_i    : in  std_logic;
      d_i        : in  std_logic_vector(g_data_width-1 downto 0);
      d_req_o    : out std_logic;
      d_drop_i   : in  std_logic;
      d_accept_i : in  std_logic;
      d_valid_i  : in  std_logic;
      d_o        : out std_logic_vector(g_data_width-1 downto 0);
      d_valid_o  : out std_logic;
      d_req_i    : in  std_logic);
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

  component xwb_fabric_sink
    port (
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      snk_i     : in  t_wrf_sink_in;
      snk_o     : out t_wrf_sink_out;
      addr_o    : out std_logic_vector(1 downto 0);
      data_o    : out std_logic_vector(15 downto 0);
      dvalid_o  : out std_logic;
      sof_o     : out std_logic;
      eof_o     : out std_logic;
      error_o   : out std_logic;
      bytesel_o : out std_logic;
      dreq_i    : in  std_logic);
  end component;

  type t_rx_state is (IDLE, HEADER, FRAME_SEQ_ID, PAYLOAD, SUBFRAME_HEADER, EOF);

  signal fab, fsm_in : t_pipe;

  signal state : t_rx_state;

  signal ser_count : unsigned(7 downto 0);
  signal seq_no, seq_new,count  : unsigned(14 downto 0);

  signal crc_match, crc_en, crc_en_masked, crc_restart : std_logic;

  signal detect_escapes, is_escape : std_logic;
  signal rx_pending                : std_logic;

  signal pack_data, fifo_data : std_logic_vector(g_data_width-1 downto 0);

  signal fifo_drop, fifo_accept, fifo_accept_d0, fifo_dvalid : std_logic;
  signal fifo_sync, fifo_last, fifo_lost                     : std_logic;
  signal fifo_dout, fifo_din                                 : std_logic_vector(g_data_width + 1 downto 0);

  signal pending_write, fab_dvalid_pre : std_logic;


  signal tx_tag_cycles, rx_tag_cycles : std_logic_vector(27 downto 0);
  signal tx_tag_valid, rx_tag_valid   : std_logic;

  signal got_next_subframe : std_logic;
  signal rst : std_logic;
  signal is_frame_seq_id : std_logic;
  signal word_count                                                        : unsigned(11 downto 0);
  signal sync_seq_no : std_logic;
begin  -- rtl

  rst <= not rst_n_i;
  U_rx_crc_generator : gc_crc_gen
    generic map (
      g_polynomial              => x"1021",
      g_init_value              => x"ffff",
      g_residue                 => x"470f",
      g_data_width              => 16,
      g_sync_reset              => 1,
      g_dual_width              => 0,
      g_registered_match_output => true)
    port map (
      clk_i     => clk_sys_i,
      rst_i     => '0',
      restart_i => crc_restart,
      en_i      => crc_en_masked,
      data_i    => fsm_in.data,
      half_i    => '0',
      match_o   => crc_match);

  crc_en_masked <= crc_en and fsm_in.dvalid;

  U_Fabric_Sink : xwb_fabric_sink
    port map (
      clk_i     => clk_sys_i,
      rst_n_i   => rst_n_i,
      snk_i     => snk_i,
      snk_o     => snk_o,
      addr_o    => fab.addr,
      data_o    => fab.data,
      dvalid_o  => fab_dvalid_pre,
      sof_o     => fab.sof,
      eof_o     => fab.eof,
      error_o   => fab.error,
      bytesel_o => fab.bytesel,
      dreq_i    => fab.dreq);

  fab.dvalid <= '1' when fab_dvalid_pre = '1' and fab.addr = c_WRF_DATA and fab.bytesel = '0' else '0';
  gen_escape: if (g_escape_code_disable = FALSE) generate
    U_Escape_Detect : escape_detector
      generic map (
        g_data_width  => 16,
        g_escape_code => x"cafe")
      port map (
        clk_i             => clk_sys_i,
        rst_n_i           => rst_n_i,
        d_i               => fab.data,
        d_detect_enable_i => detect_escapes,
        d_valid_i         => fab.dvalid,
        d_req_o           => fab.dreq,
        d_o               => fsm_in.data,
        d_escape_o        => is_escape,
        d_valid_o         => fsm_in.dvalid,
        d_req_i           => fsm_in.dreq);
  end generate gen_escape;
  gen_no_escape: if (g_escape_code_disable = TRUE) generate
    fsm_in.dvalid <= fab.dvalid;
    fsm_in.data   <= fab.data;
    fab.dreq      <= fsm_in.dreq;
    is_escape     <= '0';
  end generate gen_no_escape;
  fsm_in.eof <= fab.eof or fab.error;
  fsm_in.sof <= fab.sof;


  U_Output_FIFO : dropping_buffer
    generic map (
      g_size       => g_buffer_size,
      g_data_width => g_data_width + 2)
    port map (
      clk_i      => clk_sys_i,
      rst_n_i    => rst_n_i,
      d_i        => fifo_din,
      d_req_o    => fsm_in.dreq,
      d_drop_i   => fifo_drop,
      d_accept_i => fifo_accept_d0,
      d_valid_i  => fifo_dvalid,
      d_o        => fifo_dout,
      d_valid_o  => rx_valid_o,
      d_req_i    => rx_dreq_i);

  fifo_din(g_data_width+1)          <= fifo_sync;
  fifo_din(g_data_width)            <= fifo_last or 
                                        ((not pending_write) and is_escape); -- when word is 16 bits
  fifo_din(g_data_width-1 downto 0) <= fifo_data;

  rx_data_o  <= fifo_dout(g_data_width-1 downto 0);
  rx_first_o <= fifo_dout(g_data_width+1);
  rx_last_o  <= fifo_dout(g_data_width);

  U_RX_Timestamper : pulse_stamper
    port map (
      clk_ref_i       => clk_ref_i,
      clk_sys_i       => clk_sys_i,
      rst_n_i         => rst_n_i,
      pulse_a_i       => fsm_in.sof,
      tm_time_valid_i => tm_time_valid_i,
      tm_tai_i        => tm_tai_i,
      tm_cycles_i     => tm_cycles_i,
      tag_cycles_o    => rx_tag_cycles);

  p_fsm : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state                  <= IDLE;
        count                  <= (others => '0');
        seq_no                 <= (others => '0');
        detect_escapes         <= '0';
        crc_en                 <= '0';
        fifo_accept            <= '0';
        fifo_drop              <= '0';
        fifo_dvalid            <= '0';
        pending_write          <= '0';
        got_next_subframe      <= '0';
        fifo_sync              <= '0';
        fifo_last              <= '0';
        rx_latency_valid_o     <= '0';
        tx_tag_valid           <= '0';
        ser_count              <= (others => '0');
        word_count             <= (others => '0');
        sync_seq_no            <= '1';
      else
        case state is
          when IDLE =>
            detect_escapes     <= '0';
            crc_en             <= '0';
            count              <= (others => '0');
            fifo_accept        <= '0';
            fifo_drop          <= '0';
            fifo_dvalid        <= '0';
            pending_write      <= '0';
            got_next_subframe  <='0';
            ser_count          <= (others => '0');
            fifo_sync          <='0';
            fifo_last          <= '0';
            word_count         <= (others => '0');
            rx_latency_valid_o <= '0';
            tx_tag_valid       <= '0';

            if(fsm_in.sof = '1') then
              state            <= HEADER;
            end if;

          when HEADER =>
            if(fsm_in.eof = '1') then
              state <= IDLE;
            elsif(fsm_in.dvalid = '1') then
              case count(7 downto 0) is
                when x"00" =>
                  if(fsm_in.data /= cfg_mac_local_i(47 downto 32) nor (cfg_accept_broadcasts_i = '1' and fsm_in.data /= x"ffff")) then
                    state <= IDLE;
                  end if;
                  count <= count + 1;
                when x"01" =>
                  if(fsm_in.data /= cfg_mac_local_i(31 downto 16) nor (cfg_accept_broadcasts_i = '1' and fsm_in.data /= x"ffff")) then
                    state <= IDLE;
                  end if;
                  count <= count + 1;
                when x"02" =>
                  if(fsm_in.data /= cfg_mac_local_i(15 downto 0) nor (cfg_accept_broadcasts_i = '1' and fsm_in.data /= x"ffff")) then
                    state <= IDLE;
                  end if;
                  count <= count + 1;
                when x"03" =>
                  if(fsm_in.data /= cfg_mac_remote_i(47 downto 32) and g_filter_remote_mac) then
                    state <= IDLE;
                  end if;
                  count <= count + 1;
                when x"04" =>
                  if(fsm_in.data /= cfg_mac_remote_i(31 downto 16) and g_filter_remote_mac) then
                    state <= IDLE;
                  end if;
                  count <= count + 1;
                when x"05" =>
                  if(fsm_in.data /= cfg_mac_remote_i(15 downto 0) and g_filter_remote_mac) then
                    state <= IDLE;
                  end if;
                  count <= count + 1;
                when x"06" =>
                  if(fsm_in.data /= cfg_ethertype_i) then
                    state <= IDLE;
                  end if;
                  count <= count + 1;
                when x"07" =>
                  tx_tag_valid               <= fsm_in.data(15);
                  tx_tag_cycles(27 downto 16)<= fsm_in.data(11 downto 0);
                  count <= count + 1;
                when x"08" =>
                  tx_tag_cycles(15 downto 0) <= fsm_in.data;
                  count                      <= count + 1;
                  crc_en                     <= '1';
                  detect_escapes             <= '1';
                  state                      <= FRAME_SEQ_ID;
                when others => null;
              end case;
            end if;

          when FRAME_SEQ_ID =>
            if(fsm_in.eof = '1') then
              state <= IDLE;
            elsif(fsm_in.dvalid = '1') then
              count               <= "000" & x"001"; -- use as subframe seq_no
              state               <= PAYLOAD;
              fifo_drop           <= '0';
              fifo_accept         <= '0';
              ser_count           <= (others => '0');
              word_count          <= word_count + 1; -- count words, increment in advance
              got_next_subframe   <= '1';
              if(tx_tag_valid = '1') then
                rx_latency_valid_o <= '1';
                if(unsigned(tx_tag_cycles) > unsigned(rx_tag_cycles)) then
                  rx_latency_o <= std_logic_vector(unsigned(rx_tag_cycles) - unsigned(tx_tag_cycles) + to_unsigned(125000000, 28));
                else
                  rx_latency_o <= std_logic_vector(unsigned(rx_tag_cycles) - unsigned(tx_tag_cycles));
                end if;
                tx_tag_valid <= '0';
              else
                rx_latency_valid_o <= '0';
              end if;

              if(std_logic_vector(seq_no) /= fsm_in.data(14 downto 0)) then
                seq_no    <= unsigned(fsm_in.data(14 downto 0))+1;
                if (sync_seq_no = '1') then -- sync to the first received seq_no
                  sync_seq_no <= '0';
                else
                  fifo_lost <= '1';
                end if;
              else
                seq_no    <= unsigned(seq_no + 1);
                fifo_lost <= '0';
              end if;
            end if;

          when SUBFRAME_HEADER =>
            fifo_drop   <= '0';
            fifo_accept <= '0';
            ser_count <= (others => '0');

            if(fsm_in.eof = '1') then
              state <= IDLE;
              got_next_subframe <= '0';
            elsif (fsm_in.dvalid = '1' and is_escape = '1') then
              got_next_subframe <= '1';

              if(std_logic_vector(count) /= fsm_in.data(14 downto 0)) then
                count     <= unsigned(fsm_in.data(14 downto 0))+1;
                fifo_lost <= '1';
              else
                count    <= count + 1;
                fifo_lost <= '0';
              end if;
              state <= PAYLOAD;
            end if;

          when PAYLOAD =>
            rx_latency_valid_o <= '0';
            fifo_sync <= got_next_subframe;

            if(fsm_in.eof = '1') then
              state       <= IDLE;
              fifo_drop   <= '1';
              fifo_accept <= '0';
              got_next_subframe <= '0';
              
            elsif(fsm_in.dvalid = '1') then

              
              
              if(is_escape = '1') then
                ser_count <= (others => '0');
                fifo_last <= '1';

                got_next_subframe <= '1';

                if(fsm_in.data(15) = '1') then
                  
                  if(std_logic_vector(count) /= fsm_in.data(14 downto 0)) then
                    count     <= unsigned(fsm_in.data(14 downto 0));
                    fifo_lost <= '1';
                  else
                    count     <= unsigned(count + 1);
                    fifo_lost <= '0';
                  end if;
                  
                  state <= PAYLOAD;

                  fifo_accept   <= crc_match;      --_latched;
                  fifo_drop     <= not crc_match;  --_latched;
                  fifo_dvalid   <= pending_write and not fifo_dvalid;
                  pending_write <= '0';
                  
                elsif fsm_in.data = x"0bad" then
                  
                  state       <= EOF;
                  fifo_accept <= crc_match;      --_latched;
                  fifo_drop   <= not crc_match;  --_latched;
                  fifo_dvalid <= pending_write and not fifo_dvalid;
                else
                  
                  state       <= EOF;
                  fifo_drop   <= '1';
                  fifo_accept <= '0';
                end if;

--                fifo_dvalid <= '0';
              else --of:  if(is_escape = '1' or word_count = g_expected_words_number) then

                fifo_last   <= '0';
                fifo_accept <= '0';
                fifo_drop   <= '0';

                pack_data(to_integer(ser_count) * 16 + 15 downto to_integer(ser_count) * 16) <= fsm_in.data;

                if(ser_count = g_data_width/16 - 1) then
                  ser_count                                        <= (others => '0');

                  if (ser_count = x"00") then -- ML: the case when g_data_width == 16
                     fifo_sync <= got_next_subframe;
                     fifo_data(g_data_width-1 downto 0)            <= pack_data(g_data_width-1 downto 0);
                     fifo_dvalid <= not is_escape;
                     pending_write <= '0';
                  else
                    ser_count                                        <= (others => '0');
                    fifo_data(g_data_width-16-1 downto 0)            <= pack_data(g_data_width-16-1 downto 0);
                    fifo_data(g_data_width-1 downto g_data_width-16) <= fsm_in.data;
                    fifo_dvalid                                      <= '0';
                    pending_write                                    <= '1';
                  end if;
                  if(word_count = g_expected_words_number) then
                    state       <= EOF;
                    fifo_accept <= '1'; 
                    fifo_drop   <= '0'; 
                    fifo_dvalid <= '1';
                  else
                    word_count <= word_count + 1;
                  end if;
                elsif(ser_count = g_data_width/16-2 and pending_write = '1') then
                  pending_write <= '0';
                  ser_count     <= ser_count + 1;
                  fifo_dvalid   <= '1';
                  fifo_sync <= got_next_subframe;
                  got_next_subframe <= '0';
                else
                  ser_count   <= ser_count + 1;
                  fifo_dvalid <= '0';
                end if;
                
              end if;
            else --of:  elsif(fsm_in.dvalid = '1') then
              fifo_dvalid <= '0';
            end if;

            if(fifo_dvalid = '1') then
              fifo_sync <= '0';
            end if;
            

          when EOF =>
            fifo_dvalid <= '0';
            fifo_drop   <= '0';
            fifo_accept <= '0';
            state       <= IDLE;
            
        end case;
      end if;
    end if;
  end process;

  p_delay_fifo_accept : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      fifo_accept_d0 <= fifo_accept;
    end if;
  end process;

--  fifo_data <= pack_data;
  rx_lost_o   <= fifo_lost;

  crc_restart <= '1' when (state = FRAME_SEQ_ID or (is_escape = '1' and fsm_in.data(15) = '1')) else not rst_n_i;
  
  
end rtl;
