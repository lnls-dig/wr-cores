--! @file wr_tlu.vhd
--! @brief Timestamp latch unit for WR core with WB b4 interface
--!
--! Copyright (C) 2011-2012 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! Register map:
--!----------------------------------------------------------------------------
--! 0x00, ro, Channel n..0 contains timestamp
--! 0x04, wo, Clear channels n..0
--! 0x08, wo, Test channels n..0
--! 0x0C, ro, Trigger n..0 status
--! 0x10, wo, Activate trigger n..0
--! 0x14, wo, Deactivate trigger n..0
--! 0x18, ro, Trigger n..0 latch edge (1 pos, 0 neg)
--! 0x1C, wo, Latch trigger n..0 pos
--! 0x20, wo, Latch trigger n..0 neg
--! 0x24, rw, Global IRQ enable
--! 0x28, ro, IRQ channels mask
--! 0x2C, wo, Set IRQ channels mask n..0
--! 0x30, wo, Slr IRQ channels mask n..0
--! 0x34, ro, Number of channels               
--! 0x38, ro, Channel fifo depth
--! **** reserved
--! 0x50, ro, Current time (Cycle Count) High word. read high 1st, then low 
--! 0x54, ro, Current time (Cycle Count) Low word
--!
--! 0x58, rw, Channel select  
--! ***** CAREFUL! From here on, all addresses depend on channel select Reg !
--! ***** make sure to keep cycle line HI while manipulating 0x5C - 0x74
--!  
--! 0x5C, wo  writing anything here will pop selected channel
--! 0x60, wo, Test selected channel
--! 0x64, ro, fifo fill count
--! 0x68, ro, fifo q - Cycle Count High word
--! 0x6c, ro, fifo q - Cycle Count Low word  
--! 0x70, ro, fifo q - Sub cycle word
--! 0x74, rw, MSI msg to be sent 
--! 0x78, rw, MSI adr to send to
--!----------------------------------------------------------------------------
--1
--! @author Mathias Kreider <m.kreider@gsi.de>
--!
--! @bug No know bugs.
--!
--------------------------------------------------------------------------------
--! This library is free software; you can redistribute it and/or
--! modify it under the terms of the GNU Lesser General Public
--! License as published by the Free Software Foundation; either
--! version 3 of the License, or (at your option) any later version.
--!
--! This library is distributed in the hope that it will be useful,
--! but WITHOUT ANY WARRANTY; without even the implied warranty of
--! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--! Lesser General Public License for more details.
--!  
--! You should have received a copy of the GNU Lesser General Public
--! License along with this library. If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------------

--! Standard library
library IEEE;
--! Standard packages    
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library work;
use work.wishbone_pkg.all;
use work.genram_pkg.all;
use work.gencores_pkg.all;
use work.wb_irq_pkg.all;
use work.tlu_pkg.all;

entity wr_tlu is
   generic( g_offset       : natural := 6;
            g_num_triggers : natural := 1;
            g_fifo_depth   : natural := 32;
            g_auto_msg     : boolean := false);  
   port (
      clk_ref_i         : in  std_logic;    -- tranceiver clock domain
      rst_ref_n_i       : in  std_logic;
      clk_sys_i         : in  std_logic;    -- local clock domain
      rst_sys_n_i       : in  std_logic;

      triggers_i        : in  t_trigger_array(g_num_triggers-1 downto 0);  -- trigger vectors for latch. meant to be used with 8bits from
                                                                           -- lvds derserializer for 1GhZ res
                                                                           -- connect single ended inputs as triggers_i(m) => (others => s_my_signal)     
      tm_tai_cyc_i      : in  std_logic_vector(63 downto 0);   -- TAI Timestamp in 8ns cycles  

      ctrl_slave_i      : in  t_wishbone_slave_in;             -- Wishbone slave interface (sys_clk domain)
      ctrl_slave_o      : out t_wishbone_slave_out;
      
      irq_master_o      : out t_wishbone_master_out;           -- msi irq src 
      irq_master_i      : in  t_wishbone_master_in
   );               

end wr_tlu;

architecture behavioral of wr_tlu is
-------------------------------------------------------------------------------
-- type definitions
-------------------------------------------------------------------------------
-- stdlv with bit for every trigger channel
  subtype channels is std_logic_vector (g_num_triggers-1 downto 0);
-- stdlv to hold utc + cycle counter
  subtype t_timestamp is std_logic_vector(64 + 3 - 1 downto 0);
  type    t_tm_array is array (0 to g_num_triggers-1) of t_timestamp;
-- stdlv 32b wb bus word
  subtype t_word is std_logic_vector(31 downto 0);
  type    t_word_array is array (0 to g_num_triggers-1) of t_word;
-- stdlv to hold read and write counts of fifos 
  subtype t_cnt is std_logic_vector(f_log2_size(g_fifo_depth)-1 downto 0);
  type    t_cnt_array is array (0 to g_num_triggers-1) of t_cnt;

 -------------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- LATCH UNIT(s)
  -----------------------------------------------------------------------------
  signal s_triggers                 : t_trigger_array(g_num_triggers-1 downto 0);
  -- these control registers must be synced to ref_clk domain

   
  -- tm latch registers
  signal tm_fifo_in                 : t_tm_array;
  signal tm_fifo_out                : t_tm_array;
  
  signal r_tm_tai_cyc_LO            : t_word;
 -- fifo signals
  signal nRst_fifo                  : channels;
  signal rd                         : channels;
  signal we                         : channels;
  signal rd_empty                   : channels;
  signal wr_full, 
         r_wr_full,
         s_wr_full_edge             : channels;
  signal rd_count                   : t_cnt_array;
  signal wr_count                   : t_cnt_array;
  
  --fifo clear is asynchronous
  -- rd_empty signal is already in sys_clk domain
  signal fifo_data_rdy, 
         r_fifo_data_rdy,            
         s_fifo_data_rdy_edge       : channels;


   signal s_adr : unsigned(7 downto 0);
-------------------------------------------------------------------------------
-- WB BUS INTERFACE
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- wb interface signals
   signal r_c_ack, r_c_err, r_c_stall   : std_logic;
   signal r_c_dato : t_wishbone_data;

-------------------------------------------------------------------------------
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
   
   signal r_rst_n    : std_logic; 
   signal r_csl      : t_wishbone_data;
   signal r_clr      : channels;   
   signal r_stable   : t_wishbone_data;
   signal r_test     : t_trigger_array(g_num_triggers-1 downto 0);
   signal r_test_all : t_trigger;  
   signal r_msk      : channels;
   signal r_act      : channels;
   signal r_edg      : channels;
   signal r_pop      : channels;
   signal r_ie       : std_logic_vector(0 downto 0);
   signal r_msg, 
          s_msg      : t_wishbone_data_array(g_num_triggers-1 downto 0);   
   signal r_dst      : t_wishbone_address_array(g_num_triggers-1 downto 0);

   -- sync chains
   signal r_clr1,
          r_clr_ref_synced       : channels;
   
   signal r_triggers1,
          r_triggers_ref_synced  : t_trigger_array(g_num_triggers-1 downto 0);  
   
   signal r_act1,
          r_act_ref_synced       : channels;
          
   signal r_edg1,
          r_edg_ref_synced       : channels;
   
   signal r_stable1,
          r_stable_ref_synced     : t_wishbone_data;
   
   
   signal r_irq1,
          r_irq_sys_synced       : channels;         
   
   signal tm_tai_cyc1,
          tm_tai_cyc_sys_synced  : std_logic_vector(63 downto 0);                                
-----------------------------------------------------------------------------

begin  -- behavioral


   ref_sync_chains : process(clk_ref_i)
   begin
     if rising_edge(clk_ref_i) then 
        r_triggers1           <= s_triggers;
        r_triggers_ref_synced <= r_triggers1;
         
        r_act1                <= r_act;
        r_act_ref_synced      <= r_act1; 
        
        r_edg1                <= r_edg;
        r_edg_ref_synced      <= r_edg1;
        
        r_stable1             <= r_stable;
        r_stable_ref_synced   <= r_stable1;
     end if;           
   end process;
       
   sys_sync_chains : process(clk_sys_i)
   begin
      if rising_edge(clk_sys_i) then
         r_irq1                  <= we;
         r_irq_sys_synced        <= r_irq1;
         
         tm_tai_cyc1             <= tm_tai_cyc_i;
         tm_tai_cyc_sys_synced   <= tm_tai_cyc1;
      end if;          
   end process;

    
     
-------------------------------------------------------------------------------
-- BEGIN TRIGGER channels GENERATE
-------------------------------------------------------------------------------

   trig_sync : for i in 0 to g_num_triggers-1 generate

   fifo_clr_logic : process(clk_ref_i, r_clr(i))
   begin
      if(r_clr(i) = '1') then
         r_clr1(i)            <= '0';
         r_clr_ref_synced(i)  <= '0';
      elsif clk_ref_i'event and clk_ref_i = '1' then  -- rising clock edge
        r_clr1(i)             <= '1';
        r_clr_ref_synced(i)   <= r_clr1(i);
      end if;           
   end process; 

   nRst_fifo(i)  <= rst_ref_n_i and r_clr_ref_synced(i);
    
   fsm: wr_tlu_fsm 
   generic map( g_offset => g_offset+2) -- +2 for sync chain latency 
   port map(
      clk_ref_i         => clk_ref_i,    -- tranceiver clock domain
      rst_ref_n_i       => rst_ref_n_i,
      
      trigger_i         => r_triggers_ref_synced(i),
      tm_tai_cyc_i      => tm_tai_cyc_i,

      captured_time_o   => tm_fifo_in(i),
      valid_o           => we(i),

      -- sys clk domain signals, sync them first
      stable_i          => r_stable_ref_synced,
      active_i          => r_act_ref_synced(i),
      edge_i            => r_edg_ref_synced(i)
   );
   
   --combine software triggers with trigger inputs
   s_triggers(i) <= triggers_i(i) or r_test(i) or r_test_all;
 
   --timestamp fifos
   rd(i) <= r_pop(i);
   
     generic_async_fifo_1 : generic_async_fifo
   generic map (
     g_data_width => t_timestamp'length,  
     g_size       => g_fifo_depth,
     g_show_ahead => true,

     g_with_rd_empty        => true,
     g_with_rd_full         => false,
     g_with_rd_almost_empty => false,
     g_with_rd_almost_full  => false,
     g_with_rd_count        => true,

     g_with_wr_empty        => true,
     g_with_wr_full         => true,
     g_with_wr_almost_empty => false,
     g_with_wr_almost_full  => false,
     g_with_wr_count        => false,

     g_almost_empty_threshold => 0,
     g_almost_full_threshold  => 0
     )
   port map (
     rst_n_i           => nRst_fifo(i),
     clk_wr_i          => clk_ref_i,
     d_i               => tm_fifo_in(i),
     we_i              => we(i),
     wr_empty_o        => open,
     wr_full_o         => wr_full(i),
     wr_almost_empty_o => open,
     wr_almost_full_o  => open,
     wr_count_o        => open,
     clk_rd_i          => clk_sys_i,
     q_o               => tm_fifo_out(i),
     rd_i              => rd(i),
     rd_empty_o        => rd_empty(i),
     rd_full_o         => open,
     rd_almost_empty_o => open,
     rd_almost_full_o  => open,
     rd_count_o        => rd_count(i));

  end generate trig_sync;

-------------------------------------------------------------------------------
-- END TRIGGER channels GENERATE
-------------------------------------------------------------------------------

-- show which fifos hold unread timestamps
   fifo_data_rdy <= (not(rd_empty));
   
   -----------------------------------------------------------------------------
   -- WB Interface
   -----------------------------------------------------------------------------  
   ctrl_slave_o.int     <= '0';
   ctrl_slave_o.rty     <= '0';
   ctrl_slave_o.stall   <= r_c_stall;    
   ctrl_slave_o.ack     <= r_c_ack;
   ctrl_slave_o.err     <= r_c_err;
   ctrl_slave_o.dat     <= r_c_dato;
  
   s_adr <= unsigned(ctrl_slave_i.adr(7 downto 2)) & "00";
  
   process(clk_sys_i)
      variable v_ch_sl : natural range g_num_triggers-1 downto 0;
      variable v_en, v_we  : std_logic;
      variable v_adr       : natural;
      variable v_dati      : t_wishbone_data;
      variable v_sel       : t_wishbone_byte_select;
   begin
      if rising_edge(clk_sys_i) then
         if(rst_sys_n_i = '0' or r_rst_n = '0') then
            r_c_ack  <= '0';
            r_c_err  <= '0';
            r_rst_n  <= '1';
            r_c_stall <= '0';
            r_csl  <= (others => '0'); -- channel select
            r_act    <= (others => '0'); -- trigger active
            r_edg    <= (others => '1'); -- trigger edge
            r_msk    <= (others => '0'); -- irq mask
            r_ie     <= (others => '0'); -- global interrupt enable
            r_stable <= std_logic_vector(to_unsigned(8, r_stable'length));
         else
            -- Fire and Forget Registers        
            v_en    := ctrl_slave_i.cyc and ctrl_slave_i.stb and not r_c_stall;
            v_adr   := to_integer(s_adr);
            v_we    := ctrl_slave_i.we;
            v_dati  := ctrl_slave_i.dat;
            v_sel   := ctrl_slave_i.sel;
         
            r_c_ack     <= '0';
            r_c_err     <= '0';
            r_c_stall   <= '0';
            r_c_dato    <= (others => '0');
            r_clr       <= (others => '0');
            r_test      <= (others => (others => '0'));
            r_test_all  <= (others => '0');
            r_pop       <= (others => '0');
             
  
            if(v_en = '1') then
               v_ch_sl := to_integer(unsigned(r_csl));
               r_c_ack  <= '1';
               if(v_we = '1') then              
                  -- WISHBONE WRITE ACTIONS
                  case v_adr is
                     when c_CLR           => r_clr             <= f_wb_wr(r_clr,             v_dati, v_sel, "owr"); -- fifo clear n..0
                     when c_TEST          => r_test_all        <= f_wb_wr(r_test_all,        v_dati, v_sel, "owr"); -- soft trigger test 
                     when c_ACT_SET       => r_act             <= f_wb_wr(r_act,             v_dati, v_sel, "set"); -- channel active/inactive
                     when c_ACT_CLR       => r_act             <= f_wb_wr(r_act,             v_dati, v_sel, "clr");
                     when c_EDG_POS       => r_edg             <= f_wb_wr(r_edg,             v_dati, v_sel, "set"); -- edge pos/neg 
                     when c_EDG_NEG       => r_edg             <= f_wb_wr(r_edg,             v_dati, v_sel, "clr");
                     when c_IE            => r_ie              <= f_wb_wr(r_ie,              v_dati, v_sel, "owr"); -- global interrupt generation enable
                     when c_MSK_SET       => r_msk             <= f_wb_wr(r_msk,             v_dati, v_sel, "set"); -- interrupt generation mask
                     when c_MSK_CLR       => r_msk             <= f_wb_wr(r_msk,             v_dati, v_sel, "clr");
                     when c_CH_SEL        => if(to_integer(unsigned(v_dati)) >= g_num_triggers) then  -- channel select with limit check
                                                r_c_ack <= '0'; r_c_err <= '1';
                                             else
                                                r_csl          <= f_wb_wr(r_csl,             v_dati, v_sel, "owr"); 
                                             end if;
                     -- ***** CAREFUL! From here on, all addresses depend on channels select Reg !
                     when c_TS_POP        => if(fifo_data_rdy(v_ch_sl) = '0') then     -- fifo pop. only when fifo not empty
                                                r_c_ack <= '0'; r_c_err <= '1';
                                             else
                                                r_pop(v_ch_sl) <= '1';
                                                r_c_stall      <= '1'; 
                                             end if;
                     
                     when c_STABLE        => r_stable          <= f_wb_wr(r_stable,          v_dati, v_sel, "owr"); -- time in ns signal has to stay stable                                     
                     when c_TS_TEST       => r_test(v_ch_sl)   <= f_wb_wr(r_test(v_ch_sl),   v_dati, v_sel, "owr"); -- soft trigger test 
                     when c_TS_MSG        => r_msg(v_ch_sl)    <= f_wb_wr(r_msg(v_ch_sl),    v_dati, v_sel, "owr"); -- msi channel message  
                     when c_TS_DST_ADR    => r_dst(v_ch_sl)    <= f_wb_wr(r_dst(v_ch_sl),    v_dati, v_sel, "owr"); -- msi destination address                    
                    
                     when others          => r_c_ack <= '0'; r_c_err <= '1';
                  end case;
               else
               -- WISHBONE READ ACTIONS
                  case v_adr is 
                     when c_STAT          => r_c_dato(fifo_data_rdy'range)       <= fifo_data_rdy; -- show channels holding timestamp n..0
                     when c_ACT_GET       => r_c_dato(r_act'range)               <= r_act;         -- show active channels n..0
                     when c_EDG_GET       => r_c_dato(r_edg'range)               <= r_edg;         -- show trigger edge n..0
                     when c_IE            => r_c_dato(r_ie'range)                <= r_ie;          -- global interrupt generation enable
                     when c_MSK_GET       => r_c_dato(r_msk'range)               <= r_msk;         -- interrupt generation mask
                     when c_CH_NUM        => r_c_dato(4 downto 0)                <= std_logic_vector(to_unsigned(g_num_triggers, 5)); -- number of channels
                     when c_CH_DEPTH      => r_c_dato(15 downto 0)               <= std_logic_vector(to_unsigned(g_fifo_depth , 16)); -- channel depth
                     when c_TC_HI         => r_c_dato                            <= tm_tai_cyc_sys_synced(63 downto 32); -- current time hi word. read 1st
                                                                 r_tm_tai_cyc_LO <= tm_tai_cyc_sys_synced(31 downto 0);                      
                     when c_TC_LO         => r_c_dato                            <= r_tm_tai_cyc_LO;
                     when c_CH_SEL        => r_c_dato(r_csl'range)               <= r_csl;     -- current time lo word. read 2nd
                     -- ***** CAREFUL! From here on, all addresses depend on channels select Reg !
                     when c_STABLE        => r_c_dato                            <= r_stable; -- time in ns signal has to stay stable                                                               
                     -- FIXME: Tom's fifo has no actual element cnt, ie. can only count to N-1 and is therefore not able to show full/empty in count
                     -- this is a workaround: if count is 0 but the buffer not empty, return N instead of count 
                     when c_TS_CNT        => if(rd_empty(v_ch_sl) = '0' and unsigned(rd_count(v_ch_sl)) = 0) then
                                                r_c_dato(rd_count(v_ch_sl)'length downto 0) <= std_logic_vector(to_unsigned(g_fifo_depth,rd_count(v_ch_sl)'length+1)); 
                                             else
                                                r_c_dato(rd_count(v_ch_sl)'range)   <= rd_count(v_ch_sl); 
                                             end if;
                     when c_TS_HI         => r_c_dato                            <= tm_fifo_out(v_ch_sl)(64+3-1 downto 32+3+0); -- timestamp hi word
                     when c_TS_LO         => r_c_dato                            <= tm_fifo_out(v_ch_sl)(32+3-1 downto   +3+0); -- timestamp lo word
                     when c_TS_SUB        => r_c_dato(2 downto 0)                <= tm_fifo_out(v_ch_sl)(2 downto           0); -- timestamp sub nano word
                     when c_TS_MSG        => r_c_dato(r_msg(v_ch_sl)'range)      <= r_msg(v_ch_sl);                     -- msi channel message 
                     when c_TS_DST_ADR    => r_c_dato(r_dst(v_ch_sl)'range)      <= r_dst(v_ch_sl);                     -- msi destination address  
                     
                     when others          => r_c_ack <= '0'; r_c_err <= '1';
                  end case;
               end if; -- v_we
            end if; -- v_en
         end if; -- rst     
      end if; -- clk edge
   end process;
 
   -----------------------------------------------------------------------------
   -- MSI IRQ Interface
   ----------------------------------------------------------------------------- 
   


   irq_msg : for i in 0 to g_num_triggers-1 generate
   with g_auto_msg select
        s_msg(i) <= (r_msg(i)(31 downto 8) & std_logic_vector(to_unsigned(i, 6)) & not rd_empty(i) & wr_full(i)) when true, -- r_msg 31..16, ch no, not empty, full
                     r_msg(i) when others;
   end generate irq_msg;
   
   
   irq :  irqm_core
   generic map(g_channels     => g_num_triggers,
               g_round_rb     => true,
               g_det_edge     => true
   ) 
   port map(   clk_i          => clk_sys_i,
               rst_n_i        => rst_sys_n_i, 
            --msi if
               irq_master_o   => irq_master_o,
               irq_master_i   => irq_master_i,
            --config        
               msi_dst_array  => r_dst,
               msi_msg_array  => s_msg,
               en_i           => r_ie(0),  
               mask_i         => r_msk,
            --irq lines
               irq_i          => r_irq_sys_synced
   );

end behavioral;




