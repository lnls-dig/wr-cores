library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.wr_fabric_pkg.all;
use work.wishbone_pkg.all;
use work.genram_pkg.all;
use work.lbk_wbgen2_pkg.all;
use work.endpoint_pkg.all;
use work.endpoint_private_pkg.all;

entity xwrf_loopback is
  generic(
    g_interface_mode        : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity   : t_wishbone_address_granularity := WORD);
  port(
    clk_sys_i : in  std_logic;
    rst_n_i   : in  std_logic;

    wrf_snk_i : in  t_wrf_sink_in;
    wrf_snk_o : out t_wrf_sink_out;
    wrf_src_o : out t_wrf_source_out;
    wrf_src_i : in  t_wrf_source_in;

    wb_i : in  t_wishbone_slave_in;
    wb_o : out t_wishbone_slave_out);
end xwrf_loopback;

architecture behav of xwrf_loopback is

  component lbk_wishbone_controller
    port (
      rst_n_i    : in     std_logic;
      clk_sys_i  : in     std_logic;
      wb_adr_i   : in     std_logic_vector(2 downto 0);
      wb_dat_i   : in     std_logic_vector(31 downto 0);
      wb_dat_o   : out    std_logic_vector(31 downto 0);
      wb_cyc_i   : in     std_logic;
      wb_sel_i   : in     std_logic_vector(3 downto 0);
      wb_stb_i   : in     std_logic;
      wb_we_i    : in     std_logic;
      wb_ack_o   : out    std_logic;
      wb_stall_o : out    std_logic;
      regs_i     : in     t_lbk_in_registers;
      regs_o     : out    t_lbk_out_registers);
  end component;

  signal regs_fromwb  : t_lbk_out_registers;
  signal regs_towb    : t_lbk_in_registers;
  signal wb_out : t_wishbone_slave_out;
  signal wb_in  : t_wishbone_slave_in;

  type t_lbk_rxfsm is (IDLE, PAYLOAD, DROP, FEND);
  signal lbk_rxfsm : t_lbk_rxfsm;
  type t_lbk_txfsm is (IDLE, GET_SIZE, PAYLOAD, EOF);
  signal lbk_txfsm : t_lbk_txfsm;

  signal rcv_cnt  : unsigned(31 downto 0);
  signal drp_cnt  : unsigned(31 downto 0);
  signal fwd_cnt  : unsigned(31 downto 0);
  signal fsize    : unsigned(15 downto 0);
  signal txsize   : unsigned(15 downto 0);
  signal tx_cnt   : unsigned(3 downto 0);
  signal rcv_cnt_inc : std_logic;
  signal drp_cnt_inc : std_logic;
  signal fwd_cnt_inc : std_logic;

  signal frame_in  : std_logic_vector(15 downto 0);
  signal frame_out : std_logic_vector(15 downto 0);
  signal frame_wr  : std_logic;
  signal frame_rd  : std_logic;
  signal ffifo_full  : std_logic;
  signal sfifo_empty : std_logic;
  signal sfifo_full  : std_logic;
  signal fsize_in  : std_logic_vector(15 downto 0);
  signal fsize_out : std_logic_vector(15 downto 0);
  signal fsize_wr  : std_logic;
  signal fsize_rd  : std_logic;
  signal fword_valid : std_logic;

  type t_mac_array is array (natural range <>) of std_logic_vector(15 downto 0);
  signal forced_dmac : t_mac_array(2 downto 0);

  signal src_fab  : t_ep_internal_fabric;
  signal src_dreq : std_logic;
begin

  -------------------------------------------
  --  Standard Wishbone stuff
  -------------------------------------------
  U_Slave_adapter : wb_slave_adapter
    generic map (
      g_master_use_struct  => true,
      g_master_mode        => CLASSIC,
      g_master_granularity => WORD,
      g_slave_use_struct   => true,
      g_slave_mode         => g_interface_mode,
      g_slave_granularity  => g_address_granularity)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      slave_i    => wb_i,
      slave_o    => wb_o,
      master_i   => wb_out,
      master_o   => wb_in);

  U_WB_SLAVE: lbk_wishbone_controller
    port map(
      rst_n_i    => rst_n_i,
      clk_sys_i  => clk_sys_i,
      wb_adr_i   => wb_in.adr(2 downto 0),
      wb_dat_i   => wb_in.dat,
      wb_dat_o   => wb_out.dat,
      wb_cyc_i   => wb_in.cyc,
      wb_sel_i   => wb_in.sel,
      wb_stb_i   => wb_in.stb,
      wb_we_i    => wb_in.we,
      wb_ack_o   => wb_out.ack,
      wb_stall_o => wb_out.stall,
      regs_i     => regs_towb,
      regs_o     => regs_fromwb);
  wb_out.rty <= '0';
  wb_out.err <= '0';
  wb_out.int <= '0';
  -------------------------------------------

  FRAME_FIFO: generic_sync_fifo
    generic map(
      g_data_width  => 16,
      g_size        => 2048,
      g_with_empty  => true,
      g_with_full   => true,
      g_with_almost_empty  => false,
      g_with_almost_full   => false,
      g_with_count  => true)
    port map(
      rst_n_i => rst_n_i,
      clk_i   => clk_sys_i,
      d_i     => frame_in,
      we_i    => frame_wr,
      q_o     => frame_out,
      rd_i    => frame_rd,
      empty_o => open,
      full_o  => ffifo_full);

  SIZE_FIFO: generic_sync_fifo
    generic map(
      g_data_width  => 16,
      g_size        => 8,
      g_show_ahead  => true,
      g_with_empty  => true,
      g_with_full   => true,
      g_with_almost_empty  => false,
      g_with_almost_full   => false,
      g_with_count  => true)
    port map(
      rst_n_i => rst_n_i,
      clk_i   => clk_sys_i,
      d_i     => fsize_in,
      we_i    => fsize_wr,
      q_o     => fsize_out,
      rd_i    => fsize_rd,
      empty_o => sfifo_empty,
      full_o  => sfifo_full);

  -------------------------------------------
  -- Counters
  -------------------------------------------
  regs_towb.rcv_cnt_i <= std_logic_vector(rcv_cnt);
  regs_towb.drp_cnt_i <= std_logic_vector(drp_cnt);
  regs_towb.fwd_cnt_i <= std_logic_vector(fwd_cnt);

  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i='0' or regs_fromwb.rcv_cnt_load_o='1' or regs_fromwb.mcr_clr_o='1') then
        rcv_cnt <= (others=>'0');
      elsif(rcv_cnt_inc='1') then
        rcv_cnt <= rcv_cnt+1;
      end if;
    end if;
  end process;

  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i='0' or regs_fromwb.drp_cnt_load_o='1' or regs_fromwb.mcr_clr_o='1') then
        drp_cnt <= (others=>'0');
      elsif(drp_cnt_inc='1') then
        drp_cnt <= drp_cnt+1;
      end if;
    end if;
  end process;

  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i='0' or regs_fromwb.fwd_cnt_load_o='1' or regs_fromwb.mcr_clr_o='1') then
        fwd_cnt <= (others=>'0');
      elsif(fwd_cnt_inc='1') then
        fwd_cnt <= fwd_cnt+1;
      end if;
    end if;
  end process;

  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i='0') then
        forced_dmac(0) <= (others=>'0');
        forced_dmac(1) <= (others=>'0');
        forced_dmac(2) <= (others=>'0');
      else
        if(regs_fromwb.dmac_h_load_o='1') then
          forced_dmac(0) <= regs_fromwb.dmac_h_o;
        end if;
        if(regs_fromwb.dmac_l_load_o='1') then
          forced_dmac(1) <= regs_fromwb.dmac_l_o(31 downto 16);
          forced_dmac(2) <= regs_fromwb.dmac_l_o(15 downto  0);
        end if;
      end if;
    end if;
  end process;
  regs_towb.dmac_h_i <= forced_dmac(0);
  regs_towb.dmac_l_i <= forced_dmac(1) & forced_dmac(2);

  -------------------------------------------
  -- RX FSM
  -------------------------------------------
  fword_valid <= '1' when(wrf_snk_i.cyc='1' and wrf_snk_i.stb='1' and wrf_snk_i.adr=c_WRF_DATA) else
                 '0';

  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i='0') then
        fsize <= (others=>'0');
        lbk_rxfsm  <= IDLE;
      else
        frame_wr <= '0';
        fsize_wr <= '0';
        drp_cnt_inc <= '0';
        rcv_cnt_inc <= '0';

        if(fword_valid = '1') then
          wrf_snk_o.ack <= '1';
        else
          wrf_snk_o.ack <= '0';
        end if;
        
        case lbk_rxfsm is
          when IDLE =>
            if(regs_fromwb.mcr_ena_o='1' and wrf_snk_i.cyc='1' and ffifo_full='0' and sfifo_full='0') then
              lbk_rxfsm <= PAYLOAD;
            elsif(regs_fromwb.mcr_ena_o='1' and wrf_snk_i.cyc='1') then
              drp_cnt_inc <= '1';
              lbk_rxfsm <= DROP;
            end if;

          when PAYLOAD =>
            if(fword_valid = '1' and wrf_snk_i.sel = "11" and ffifo_full='0') then
              fsize <= fsize + 2;
            elsif(fword_valid = '1' and wrf_snk_i.sel = "10" and ffifo_full='0') then
              fsize <= fsize + 1;
            end if;

            if(fsize>4 and fword_valid='1' and ffifo_full='0') then -- because we don't store DMAC
              frame_wr <= '1';
              frame_in <= wrf_snk_i.dat;
            elsif(fsize>4 and fword_valid='1' and ffifo_full='1') then
              fsize <= fsize-2; --last write was already unsuccesfull lbk_rxfsm   <= DROP; end if; 
              lbk_rxfsm <= DROP;
            end if;

            if(wrf_snk_i.cyc='0') then
              lbk_rxfsm <= FEND;
            end if;

          when DROP =>
            if(wrf_snk_i.cyc='0') then
              --fsize <= (others=>'0');
              lbk_rxfsm <= FEND;
            end if;

          when FEND =>
            if(fsize>4) then
              fsize_wr <= '1';
              fsize_in <= std_logic_vector(fsize);
              rcv_cnt_inc <= '1';
              fsize <= (others=>'0');
            end if;
            lbk_rxfsm <= IDLE;
        end case;
      end if;
    end if;
  end process;
  wrf_snk_o.stall <= '0';
  wrf_snk_o.rty   <= '0';
  wrf_snk_o.err   <= '0';

  -------------------------------------------
  -- TX FSM
  -------------------------------------------
  WRF_SRC: ep_rx_wb_master
    generic map(
      g_ignore_ack => false,
      g_cyc_on_stall => true)
    port map(
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      snk_fab_i  => src_fab,
      snk_dreq_o => src_dreq,
      src_wb_i   => wrf_src_i,
      src_wb_o   => wrf_src_o);

  src_fab.has_rx_timestamp <= '0';
  src_fab.rx_timestamp_valid <= '0';
  src_fab.error <= '0';
  src_fab.data <= forced_dmac(0) when(regs_fromwb.mcr_fdmac_o='1' and tx_cnt=1) else
                  forced_dmac(1) when(regs_fromwb.mcr_fdmac_o='1' and tx_cnt=2) else
                  forced_dmac(2) when(regs_fromwb.mcr_fdmac_o='1' and tx_cnt=3) else
                  frame_out;
  src_fab.addr <= c_WRF_DATA;

  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i='0') then
        lbk_txfsm <= IDLE;
        txsize <= (others=>'0');
        fsize_rd <= '0';
        frame_rd <= '0';
        fwd_cnt_inc <= '0';
      else
        fsize_rd <= '0';
        frame_rd <= '0';
        fwd_cnt_inc <= '0';
        src_fab.sof <= '0';
        src_fab.eof <= '0';
        src_fab.dvalid <= '0';

        case lbk_txfsm is
          when IDLE =>
            txsize  <= (others=>'0');
            tx_cnt  <= (others=>'0');
            src_fab.bytesel <= '0';
            if(sfifo_empty = '0' and src_dreq='1') then
              lbk_txfsm <= GET_SIZE;
              fsize_rd <= '1';
            end if;

          when GET_SIZE =>
            txsize   <= unsigned(fsize_out);
            tx_cnt   <= (others=>'0');
            if(src_dreq='1') then
              src_fab.sof <= '1';
              frame_rd <= '1';
              lbk_txfsm <= PAYLOAD;
            end if;

          when PAYLOAD =>
            if(src_dreq='1' and txsize>1) then
              txsize <= txsize - 2;
              src_fab.bytesel <= '0';
            elsif(src_dreq='1' and txsize=1) then
              txsize <= txsize - 1;
              src_fab.bytesel <= '1';
            end if;
            
            --tx_cnt used only for ETH header counting words
            if(src_dreq='1' and tx_cnt<15) then
              tx_cnt <= tx_cnt + 1;
            end if;

            if(src_dreq='1' and (tx_cnt<3 or tx_cnt>5) and txsize>2) then
              frame_rd <= '1';
              src_fab.dvalid <= '1';
            elsif( (src_dreq='1' and tx_cnt>=3 and tx_cnt<=5) or
                   (src_dreq='1' and tx_cnt>5 and txsize<=2) ) then
              src_fab.dvalid <= '1';
            end if;

            if(txsize<=2 and src_dreq='1') then
              lbk_txfsm   <= EOF;
            end if;

          when EOF =>
            if(src_dreq='1') then
              src_fab.eof <= '1';
              fwd_cnt_inc <= '1';
              lbk_txfsm <= IDLE;
            end if;

        end case;
      end if;
    end if;
  end process;

end behav;
