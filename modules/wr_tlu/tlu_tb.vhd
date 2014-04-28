library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.genram_pkg.all;
use work.wishbone_pkg.all;
use work.gencores_pkg.all;
use work.tlu_pkg.all;

entity tlu_tb is
end tlu_tb;

architecture rtl of tlu_tb is 

   constant c_dummy_slave_in     : t_wishbone_slave_in   := ('0', '0', x"00000000", x"F", '0', x"00000000");
   constant c_dummy_slave_out    : t_wishbone_slave_out  := ('0', '0', '0', '0', '0', x"00000000");
   constant c_dummy_master_out   : t_wishbone_master_out := c_dummy_slave_in;
   constant c_dummy_master_in    : t_wishbone_master_in  := c_dummy_slave_out;

   signal r_ref_time : std_logic_vector(63 downto 0);
   signal r_test_time : std_logic_vector(63 downto 0);   
  
   signal clk_sys, clk_ref, clk_test : std_logic := '0';
	signal rst_n            : std_logic := '0';
	
   -- Clock period definitions
   constant clk_period_sys  : time      := 8 ns;
   constant clk_period_ref  : time      := 8 ns;
   
   signal s_ctrl_i : t_wishbone_slave_in;
   signal s_ctrl_o : t_wishbone_slave_out;
   
   signal s_irq_i : t_wishbone_master_in;
   signal s_irq_o : t_wishbone_master_out;
   
   constant c_num_triggers : natural := 3; 
   signal s_triggers : t_trigger_array(c_num_triggers-1 downto 0);
   
   --------------------------------------------------------
     --wb registers
   constant c_STAT      : natural := 0;               --0x00, ro, fifo n..0 status (0 empty, 1 ne)
   constant c_CLR       : natural := c_STAT     +4;   --0x04, wo, Clear channels n..0
   constant c_TEST      : natural := c_CLR      +4;   --0x08, ro, trigger n..0 status
   constant c_ACT_GET   : natural := c_TEST     +4;   --0x0C, ro, trigger n..0 status
   constant c_ACT_SET   : natural := c_ACT_GET  +4;   --0x10, wo, Activate trigger n..0
   constant c_ACT_CLR   : natural := c_ACT_SET  +4;   --0x14, wo, deactivate trigger n..0
   constant c_EDG_GET   : natural := c_ACT_CLR  +4;   --0x18, ro, trigger n..0 latch edge (1 pos, 0 neg)
   constant c_EDG_POS   : natural := c_EDG_GET  +4;   --0x1C, wo, latch trigger n..0 pos
   constant c_EDG_NEG   : natural := c_EDG_POS  +4;   --0x20, wo, latch trigger n..0 neg
   constant c_IE        : natural := c_EDG_NEG  +4;   --0x24, rw, Global IRQ enable
   constant c_MSK_GET   : natural := c_IE       +4;   --0x28, ro, IRQ channels mask
   constant c_MSK_SET   : natural := c_MSK_GET  +4;   --0x2C, wo, set IRQ channels mask n..0
   constant c_MSK_CLR   : natural := c_MSK_SET  +4;   --0x30, wo, clr IRQ channels mask n..0
   constant c_CH_NUM    : natural := c_MSK_CLR  +4;   --0x34, ro, number channels present              
   constant c_CH_DEPTH  : natural := c_CH_NUM   +4;   --0x38, ro, channels depth
   --reserved
   constant c_TC_HI     : natural := 16#50#;          --0x50, ro, Current time (Cycle Count) Hi. read Hi, then Lo 
   constant c_TC_LO     : natural := c_TC_HI    +4;   --0x54, ro, Current time (Cycle Count) Lo
   constant c_CH_SEL    : natural := c_TC_LO    +4;   --0x58, rw, channels select  
-- ***** CAREFUL! From here on, all addresses depend on channels select Reg !
   constant c_TS_POP    : natural := c_CH_SEL   +4;   --0x5C, wo  writing anything here will pop selected channel
   constant c_TS_TEST   : natural := c_TS_POP   +4;   --0x60, wo, Test selected channel
   constant c_TS_CNT    : natural := c_TS_TEST  +4;   --0x64, ro, fifo fill count
   constant c_TS_HI     : natural := c_TS_CNT   +4;   --0x68, ro, fifo q - Cycle Count Hi
   constant c_TS_LO     : natural := c_TS_HI    +4;   --0x6C, ro, fifo q - Cycle Count Lo  
   constant c_TS_SUB    : natural := c_TS_LO    +4;   --0x70, ro, fifo q - Sub cycle word
   constant c_STABLE    : natural := c_TS_SUB   +4;   --0x70, rw, stable time in ns, how long a signal has to be constant before edges are detected
   constant c_TS_MSG    : natural := c_STABLE   +4;   --0x74, rw, MSI msg to be sent 
   constant c_TS_DST_ADR: natural := c_TS_MSG   +4;   --0x78, rw, MSI adr to send to
   
   
   -------------------------------------------------------- 

begin
  
   DUT : wr_tlu 
   generic map(g_num_triggers => c_num_triggers,
               g_fifo_depth   => 64,
               g_auto_msg     => false) 
   port map(
      clk_ref_i         => clk_ref,    -- tranceiver clock domain
      rst_ref_n_i       => rst_n,
      clk_sys_i         => clk_sys,    -- local clock domain
      rst_sys_n_i       => rst_n,
      triggers_i        => s_triggers,  -- trigger vectors for latch. meant to be used with 8bits from lvds derserializer for 1GhZ res
                                                                -- if you only have a normal signal, connect it as triggers_i(m) => (others => s_my_signal)
      tm_tai_cyc_i      => r_ref_time,   -- TAI Timestamp in 8ns cycles  

      ctrl_slave_i      => s_ctrl_i,            -- Wishbone slave interface (sys_clk domain)
      ctrl_slave_o      => s_ctrl_o,
      
      irq_master_o      => s_irq_o,           -- msi irq src 
      irq_master_i      => s_irq_i
   );              

   
    
   -- Clock process definitions( clock with 50% duty cycle is generated here.
   clk_process1 :process
   begin
        clk_sys <= '0';
        wait for clk_period_sys/2;  --for 0.5 ns signal is '0'.
        clk_sys <= '1';
        wait for clk_period_sys/2;  --for next 0.5 ns signal is '1'.
        
        
   end process;
   
   clk_process2 :process
   begin
        clk_test <= '0';
        wait for 500 ps;  --for 0.5 ns signal is '0'.
         clk_test <= '1';
        wait for 500 ps;  --for 0.5 ns signal is '0'
   end process;
   
   clk_process3 :process
   begin
        clk_ref <= '0';
        wait for clk_period_ref/2;  --for 0.5 ns signal is '0'.
        clk_ref <= '1';
        wait for clk_period_ref/2;  --for next 0.5 ns signal is '1'.
   end process;
   
   sample :process(clk_ref)
   begin
   
   if(rising_edge(clk_ref)) then
      if(rst_n = '0') then
         r_ref_time <= (others => '0' );
         s_irq_i.err <= '0';
         s_irq_i.stall <= '0';
         s_irq_i.dat <= (others => '0');
      else
         r_ref_time <= std_logic_vector(unsigned(r_ref_time) +1);
         s_irq_i.ack <= s_irq_o.cyc and s_irq_o.stb;
      end if;
   end if;  
   end process;
   
   sample2 :process(clk_test)
   begin
   if(rising_edge(clk_test)) then
      if(rst_n = '0') then
         r_test_time <= (others => '0' );
      else
         r_test_time <= std_logic_vector(unsigned(r_test_time) +1);
      end if;
   end if;  
   end process;      
     
  
   
    -- Stimulus process
  rst: process
  begin        
        
        rst_n  <= '0';
        wait until rising_edge(clk_sys);
        wait for clk_period_sys*5;
        rst_n <= '1';
        wait for clk_period_sys*10;
        wait until rst_n = '0';
        
        
        
   end process;
   

   
    -- Stimulus process
  stim_WB_proc: process
  variable i, j, v_cnt : natural;
  variable v_T : time := clk_period_sys;
   begin        
        
        i := 50;
        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000");
        s_triggers <= (x"00", x"00", x"00");
        wait until rst_n = '1';
        wait until rising_edge(clk_sys);
        wait for v_T*5.5; 
        
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T*9;
        
        
        
        
        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_EDG_POS, 32)), x"F", '1', x"0000000f"); wait for v_T;
        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_MSK_SET, 32)), x"F", '1', x"00000007"); wait for v_T;
        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_IE, 32)),  x"F", '1', x"00000001"); wait for v_T;
        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_ACT_SET, 32)), x"F", '1', x"00000007"); wait for v_T;
        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
        for i in 0 to c_num_triggers-1 loop
           s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_CH_SEL, 32)),      x"F", '1', std_logic_vector(to_unsigned(i, 32)));              wait for v_T;
           s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TS_MSG, 32)),      x"F", '1', x"abcd" & std_logic_vector(to_unsigned(i, 16)) );   wait for v_T;
           s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TS_DST_ADR, 32)),  x"F", '1', x"1234" & std_logic_vector(to_unsigned(i, 16)) );   wait for v_T;
           s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
        end loop;
           
           
        
        --s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TEST, 32)), x"F", '1', x"000000FF");      wait for v_T;
--        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
--        wait for v_T*9;
--        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TEST, 32)), x"F", '1', x"00000000");      wait for v_T;
--        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
--        wait for v_T*9;
--        
--        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TEST, 32)), x"F", '1', x"0000007F");      wait for v_T;
--        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
--        wait for v_T*9;
--        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TEST, 32)), x"F", '1', x"0000003F");      wait for v_T;
--        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
--        wait for v_T*9;
--        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TEST, 32)), x"F", '1', x"0000001F");      wait for v_T;
--        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
--        wait for v_T*9;
--        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TEST, 32)), x"F", '1', x"0000000F");      wait for v_T;
--        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
--        wait for v_T*9;
--        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TEST, 32)), x"F", '1', x"00000007");      wait for v_T;
--        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
--        wait for v_T*9;
--        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TEST, 32)), x"F", '1', x"00000003");      wait for v_T;
--        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
--        wait for v_T*9;
--        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TEST, 32)), x"F", '1', x"00000001");      wait for v_T;
--        s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
--        wait for v_T*9;
        
        wait until rising_edge(clk_ref);
        v_t := clk_period_ref;
        wait for 7 ns; 
        
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T*20;
        
        report "TRIGGER!" severity warning;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"03", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"fc", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T*5;
        
        report "TRIGGER!" severity warning;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"07", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"f8", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T*5;
        
        report "TRIGGER!" severity warning;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"0f", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"f0", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T*5;
        
        report "TRIGGER!" severity warning;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"ff", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T*5;
        
        report "TRIGGER!" severity warning;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"ff", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"ff", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"ff", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T*5;
      
        
        report "TRIGGER!" severity warning;
        s_triggers <= (x"00", x"AF", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"f0", x"00");
        wait for v_T;
        
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T*10;
        
        s_triggers <= (x"00", x"FF", x"00");
        wait for v_T*10;
        report "TRIGGER!" severity warning;
        s_triggers <= (x"00", x"F0", x"00");
        wait for v_T;
        s_triggers <= (x"00", x"0F", x"00");
        wait for v_T;
        
        s_triggers <= (x"00", x"ff", x"00");
        wait for v_T*20;
        ------------------------------------------
        report "Glitch" severity note;
        s_triggers <= (x"0f", x"0f", x"0f");
        wait for v_T/8;
        s_triggers <= (x"00", x"ff", x"00");
        wait for v_T/8;
        s_triggers <= (x"0f", x"0f", x"0f");
        wait for v_T/8;
        s_triggers <= (x"00", x"ff", x"00");
        wait for v_T/8;
        s_triggers <= (x"0f", x"0f", x"0f");
        wait until rising_edge(clk_sys);
        wait for v_T;
        s_triggers <= (x"00", x"ff", x"00");
        wait for v_T;
         ------------------------------------------
        s_triggers <= (x"1f", x"ff", x"00");
        wait for 5*v_T;
        s_triggers <= (x"00", x"00", x"00");
        wait for v_T*2;
        s_triggers <= (x"00", x"00", x"00");
        
         s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_STAT, 32)), x"F", '0', x"00000000");      wait for v_T;
        report "FIFO Status before:" severity warning; 
        for i in 0 to c_num_triggers-1 loop
           s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_CH_SEL, 32)), x"F", '1', std_logic_vector(to_unsigned(i, 32))); wait for v_T;
           s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TS_CNT, 32)), x"F", '0', x"00000000");      wait for v_T;
           s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); 
           v_cnt := to_integer(unsigned(s_ctrl_o.dat));
           
           wait for v_T;
           while v_cnt /= 0 loop
              s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TS_HI, 32)), x"F", '0', x"00000000");      wait for v_T;
              s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TS_LO, 32)), x"F", '0', x"00000000");     wait for v_T;
              s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TS_SUB, 32)), x"F", '0', x"00000000");     wait for v_T;
              s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TS_POP, 32)), x"F", '1', x"00000001");     wait for v_T;
              s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for 3*v_T;
              s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_TS_CNT, 32)), x"F", '0', x"00000000");      wait for v_T;
              v_cnt := to_integer(unsigned(s_ctrl_o.dat));
              s_ctrl_i <= ('0', '0', x"00000000", x"F", '0', x"00000000"); wait for v_T;
           end loop;
        end loop;
        
        wait for v_T*9;
        s_ctrl_i <= ('1', '1', std_logic_vector(to_unsigned(16#100# + c_STAT, 32)), x"F", '0', x"00000000");      wait for v_T;
        report "FIFO Status after:" severity warning;   
        wait until rst_n = '0';
  end process;

  


end rtl;  
