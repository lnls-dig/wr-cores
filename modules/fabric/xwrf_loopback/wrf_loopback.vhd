library ieee;
use ieee.std_logic_1164.all;
use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;

entity wrf_loopback is
  generic(
    g_interface_mode        : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity   : t_wishbone_address_granularity := WORD);
  port(
    clk_sys_i : in  std_logic;
    rst_n_i   : in  std_logic;

    snk_cyc_i   : in std_logic;
    snk_stb_i   : in std_logic;
    snk_we_i    : in std_logic;
    snk_sel_i   : in std_logic_vector(1 downto 0);
    snk_adr_i   : in std_logic_vector(1 downto 0);
    snk_dat_i   : in std_logic_vector(15 downto 0);
    snk_ack_o   : out std_logic;
    snk_stall_o : out std_logic;

    src_cyc_o   : out std_logic;
    src_stb_o   : out std_logic;
    src_we_o    : out std_logic;
    src_sel_o   : out std_logic_vector(1 downto 0);
    src_adr_o   : out std_logic_vector(1 downto 0);
    src_dat_o   : out std_logic_vector(15 downto 0);
    src_ack_i   : in std_logic;
    src_stall_i : in std_logic;

    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_adr_i  : in  std_logic_vector(31 downto 0);
    wb_dat_i  : in  std_logic_vector(31 downto 0);
    wb_dat_o  : out std_logic_vector(31 downto 0);
    wb_ack_o  : out std_logic;
    wb_stall_o: out std_logic);
end wrf_loopback;

architecture behav of wrf_loopback is

  component xwrf_loopback
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
  end component;

  signal snk_in : t_wrf_sink_in;
  signal snk_out: t_wrf_sink_out;
  signal src_in : t_wrf_source_in;
  signal src_out: t_wrf_source_out;
  signal wb_in  : t_wishbone_slave_in;
  signal wb_out : t_wishbone_slave_out;

begin

  X_LOOPBACK: xwrf_loopback
  generic map(
    g_interface_mode => g_interface_mode,
    g_address_granularity => g_address_granularity)
  port map(
    clk_sys_i => clk_sys_i,
    rst_n_i   => rst_n_i,
    wrf_snk_i => snk_in,
    wrf_snk_o => snk_out,
    wrf_src_o => src_out,
    wrf_src_i => src_in,
    wb_i      => wb_in,
    wb_o      => wb_out);

  snk_in.cyc <= snk_cyc_i;
  snk_in.stb <= snk_stb_i;
  snk_in.we  <= snk_we_i;
  snk_in.sel <= snk_sel_i;
  snk_in.adr <= snk_adr_i;
  snk_in.dat <= snk_dat_i;
  snk_ack_o   <= snk_out.ack;
  snk_stall_o <= snk_out.stall;

  src_cyc_o <= src_out.cyc;
  src_stb_o <= src_out.stb;
  src_we_o  <= src_out.we;
  src_sel_o <= src_out.sel;
  src_adr_o <= src_out.adr;
  src_dat_o <= src_out.dat;
  src_in.ack <= src_ack_i;
  src_in.stall <= src_stall_i;
  src_in.err <= '0';
  src_in.rty <= '0';

  wb_in.cyc <= wb_cyc_i;
  wb_in.stb <= wb_stb_i;
  wb_in.we  <= wb_we_i;
  wb_in.sel <= wb_sel_i;
  wb_in.adr <= wb_adr_i;
  wb_in.dat <= wb_dat_i;
  wb_dat_o  <= wb_out.dat;
  wb_ack_o   <= wb_out.ack;
  wb_stall_o <= wb_out.stall;

end behav;
