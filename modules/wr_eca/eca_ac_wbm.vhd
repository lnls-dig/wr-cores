--! File Name         : eca_ac_wbm.vhd
--! Design Unit Name  : eca_ac_wbm
--! Revision          : 0.0.1
--! Author            : M. Kreider <m.kreider@gsi.de>
--! Created           : 11/12/2014
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component is a programmable wishbone master for an action channel,
--! it replays prerecorded macros to the WB bus.
--! Takes an action channels tag field as index to the macro to replay.
--!
--! Detailed instructions below
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

--+******************************************************************************************+
--| ------------------------  Control Slave interface, address map ------------------------------- |
--+******************************************************************************************+
--
-- SLAVE_STATUS_GET         0x00  // r     _0x000000ff , Shows if the device is rdy/busy
-- SLAVE_MAX_MACROS_GET     0x04  // r     _0xffffffff , Shows maximum number of macros
-- SLAVE_MAX_SPACE_GET      0x08  // r     _0xffffffff , Shows maximum memory space
-- SLAVE_ENABLE_GET         0x0c  // rw    _0x00000001 , Macro execution on/off
-- SLAVE_ENABLE_SET         0x10  // rw    _0x00000001 , ""
-- SLAVE_ENABLE_CLR         0x14  // rw    _0x00000001 , ""
-- SLAVE_EXEC_OWR           0x18  // wfs   _0x000000ff , Executes macro at idx
-- SLAVE_LAST_EXEC_GET      0x1c  // r     _0x000000ff , Shows idx of last executed macro
-- SLAVE_REC_OWR            0x20  // wfs   _0x000000ff , Records macro at idx
-- SLAVE_LAST_REC_GET       0x24  // r     _0x000000ff , Shows idx of last recorded macro
-- SLAVE_MACRO_QTY_GET      0x28  // r     _0x000000ff , Shows the number of macros in the ram
-- SLAVE_SPACE_LEFT_GET     0x2c  // r     _0x0000ffff , Shows number of free spaces in the RAM
-- SLAVE_CLEAR_ALL_OWR      0x30  // wsp   _0x00000001 , Clears all macros
-- SLAVE_CLEAR_IDX_OWR      0x34  // wfs   _0x000000ff , Clears macro at idx
-- SLAVE_REC_FIFO_OWR       0x38  // wf    _0xffffffff , Recording fifo. 3 word sequences: #ADR# #VAL# #META#

------------------------------------------------------------------------------------------------
-- USAGE:
-- all commands marked * are optional

------
-- Record macro:
-- record macro @ idx   -> write SLAVE_REC_OWR        0x20 <idx>
--
-- record element(s)
-- wb target address    -> write SLAVE_REC_FIFO_OWR   0x38 <adr>
-- wb data to write     -> write SLAVE_REC_FIFO_OWR   0x38 <data>
-- meta data / flags    -> write SLAVE_REC_FIFO_OWR   0x38 <drop><data src><wb select>
-- repeat for <n> elements / finish
--
-- finish recording     -> write SLAVE_REC_OWR        0x20 <>
--
--
--    Metadata format:
--    b8       - wb drop cycle bit
--    b7..b4   - wb output data source
--       output select  enums:
--       c_SHOW_PAYLOAD 0x0 data field from record
--       c_SHOW_FLAGS   0x1 channel valid/late/conflict
--       c_SHOW_EVT_HI  0x2 channel evt ID
--       c_SHOW_EVT_LO  0x3  
--       c_SHOW_PAR_HI  0x4 channel parameter
--       c_SHOW_PAR_LO  0x5 
--       c_SHOW_TAG     0x6 channel tag
--       c_SHOW_TEF     0x7 channel tef
--       c_SHOW_T_HI    0x8 channel time
--       c_SHOW_T_LO    0x9 
--    b3..b0   - wb select lines
--
------
-- Clear macro(s):
-- clear macro @ idx    -> write SLAVE_CLEAR_IDX_OWR  0x34 <idx>
--
-- clear all macros         -> write SLAVE_CLEAR_IDX_OWR  0x30 <>
------
-- Enable output: Macros will only be executed if SLAVE_ENABLE is 1
--
-- enable               -> write SLAVE_ENABLE_SET     0x10 0x1 
--
-- disable              -> write SLAVE_ENABLE_CLR     0x14 0x1
--
-- check if enabled     -> read SLAVE_ENABLE_GET      0x0c
------
-- Execute macro:
--
-- via channel: tag low bits are used as index <idx>. Will not execute if already busy
-- *check if executed   -> read SLAVE_LAST_EXEC_GET   0x1C -> must be <idx>
--
-- via WB command
-- execute macro @ idx  -> write SLAVE_EXEC_OWR       0x18 <idx>
-- *check if executed   -> read SLAVE_LAST_EXEC_GET   0x1C -> must be <idx>
--
------------------------------------------------------------------------------------------------
                
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.wishbone_pkg.all;
use work.genram_pkg.all;
use work.eca_pkg.all;
use work.eca_ac_wbm_auto_pkg.all;

entity eca_ac_wbm is
generic(
   g_entries  : natural := 32;
   g_ram_size : natural := 256   
);
Port(
   clk_ref_i   : in  std_logic;                                            
   rst_ref_n_i : in  std_logic;
   channel_i   : in  t_channel;
   
   clk_sys_i   : in  std_logic;
   rst_sys_n_i : in  std_logic;
   slave_i     : in  t_wishbone_slave_in  := ('0', '0', x"00000000", x"F", '0', x"00000000"); 
   slave_o     : out t_wishbone_slave_out;
   master_o    : out t_wishbone_master_out;
   master_i    : in  t_wishbone_master_in                                             
);
end eca_ac_wbm;

architecture rtl of eca_ac_wbm is
  
  -- let's try this for a change
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
   --+******************************************************************************************+
   --| ------------------------------  constants & types -------------------------------------- |
   --+******************************************************************************************+
   constant c_rec_meta_bits   : natural := 1 + f_ceil_log2(g_ram_size);
   constant c_wb_bits         : natural := 69; 
   constant c_show_enum_bits  : natural := 4;
   constant c_sel_bits        : natural := 4;
   constant c_ram_width       : natural := (c_rec_meta_bits + c_wb_bits + c_show_enum_bits);
   
   constant c_STAT_RDY        : natural := 0;
   constant c_STAT_BUSY       : natural := 1;
   
   constant c_META_DROP       : natural := c_sel_bits+c_show_enum_bits;
   constant c_META_FLAGS_MSB  : natural := c_sel_bits+c_show_enum_bits-1;
   constant c_META_FLAGS_LSB  : natural := c_sel_bits;
   constant c_META_SEL_MSB    : natural := c_sel_bits-1;
   constant c_META_SEL_LSB    : natural := 0;
   
   constant c_SHOW_PAYLOAD    : natural := 0;
   constant c_SHOW_FLAGS      : natural := 1;
   constant c_SHOW_EVT_HI     : natural := 2;
   constant c_SHOW_EVT_LO     : natural := 3;
   constant c_SHOW_PAR_HI     : natural := 4;
   constant c_SHOW_PAR_LO     : natural := 5;
   constant c_SHOW_TAG        : natural := 6;
   constant c_SHOW_TEF        : natural := 7;
   constant c_SHOW_T_HI       : natural := 8;
   constant c_SHOW_T_LO       : natural := 9;
  
   type t_wb is record 
      drop          : std_logic;
      sel           : std_logic_vector(3 downto 0);
      addr          : std_logic_vector(31 downto 0);
      data          : std_logic_vector(31 downto 0);
   end record t_wb;
   
   type t_rec is record
      has_next : std_logic;
      pnext    : std_logic_vector(f_ceil_log2(g_ram_size)-1 downto 0);
      wb       : t_wb;
      ch_show  : unsigned(c_show_enum_bits-1 downto 0);         
   end record t_rec;
   
   subtype t_adr is std_logic_vector(f_ceil_log2(g_ram_size)-1 downto 0);
   type st_state is (st_IDLE, st_REC_ADDR, st_REC_DATA, st_REC_META, st_TX_LOAD, st_TX, st_TX_WAIT, st_CLR_LOAD, st_CLR);
   type t_adr_array is array(natural range <>) of t_adr;
   
   
   --+******************************************************************************************+
   --| ------------------------------  helper functions --------------------------------------- |
   --+******************************************************************************************+
   -- serializer for ram port
   function f_ser_rec(a : t_rec) return std_logic_vector is
      variable x : t_rec;
      variable y : t_wb;
      variable res_x : std_logic_vector( c_rec_meta_bits-1 downto 0);
      variable res_y : std_logic_vector( c_wb_bits-1 downto 0);
      variable res_z : std_logic_vector( c_show_enum_bits-1 downto 0);
      variable result : std_logic_vector( (res_x'length + res_y'length + res_z'length )-1 downto 0);
   begin
      x       := a;
      y       := a.wb;
      res_x   := x.has_next & x.pnext;
      res_y   := y.drop & y.sel & y.addr & y.data;
      res_z   := std_logic_vector(a.ch_show);  
      result  := res_x & res_y & res_z;
      return result;
   end f_ser_rec;
  
   -- deserializer for ram port
   function f_dser_rec(a : std_logic_vector) return t_rec is
      variable x : std_logic_vector( c_rec_meta_bits-1 downto 0);
      variable y : std_logic_vector( c_wb_bits-1 downto 0);
      variable z : std_logic_vector( c_show_enum_bits-1 downto 0);
      variable r : t_rec;
   begin
      x       := a(a'high downto a'length-x'length);
      y       := a(a'high-x'length downto a'length-x'length-y'length);
      z       := a(z'high downto 0);
      r.has_next := x(x'high);
      r.pnext    := x(x'high-1 downto 0);
      r.wb.drop  := y(68);
      r.wb.sel   := y(67 downto 64);
      r.wb.addr  := y(63 downto 32);
      r.wb.data  := y(31 downto 0);
      r.ch_show  := unsigned(z);
      return r;
   end f_dser_rec;

   
   --+******************************************************************************************+
   --| ------------------------------  registers & signals------------------------------------- |
   --+******************************************************************************************+   
   -- FSM
   signal r_state             : st_state;  
   -- wbm
   signal master_out          : t_wishbone_master_out;
   signal r_op_cnt            : unsigned(f_ceil_log2(g_ram_size)-1 downto 0);
   signal r_ackerr_cnt        : unsigned(f_ceil_log2(g_ram_size)-1 downto 0);
   -- ctrl wbs
   signal s_slave_i           : t_wishbone_slave_in   := ('0', '0', x"00000000", x"F", '0', x"00000000"); 
   signal s_slave_o           : t_wishbone_slave_out;                                                     
   signal s_slave_regs_o      : t_slave_regs_o;                                                     
   signal s_slave_regs_i      : t_slave_regs_i;
   signal s_rst_n             : std_logic;
   -- ram
   signal s_ram_a,
          r_ram_a             : t_adr;
   signal r_ram_d,
          s_ram_q             : std_logic_vector(c_ram_width -1 downto 0);
   signal r_d, 
          s_q                 : t_rec;
   signal r_wb_addr_d         : std_logic_vector(31 downto 0);        
   signal s_ram_we            : std_logic;
   -- memory management
   signal s_addr_pool_d,
          s_addr_pool_q       : t_adr;
   signal s_addr_pool_push,
          s_addr_pool_pop,
          s_addr_pool_empty   : std_logic;
   signal r_free              : std_logic_vector(g_entries-1 downto 0);
   signal r_ptr               : t_adr_array(g_entries-1 downto 0);
   signal r_op_write          : std_logic;
   -- eca
   signal r_channel           : t_channel;
   signal r_valid_sync,
          r_busy_sync         : std_logic_vector(2 downto 0);
   signal s_valid             : std_logic;   
  
begin

   --+******************************************************************************************+
   --| ------------------------------  capture eca channel data  ------------------------------ |
   --+******************************************************************************************+
   channel_in : process(clk_ref_i, s_rst_n, rst_ref_n_i)
   begin
      
         if((s_rst_n and rst_ref_n_i) = '0') then
            r_channel <= ( '0', '0', '0', '0', '0',
                           (others=>'0'),
                           (others=>'0'),
                           (others=>'0'),
                           (others=>'0'),
                           (others=>'0'),
                           (others=>'0'),
                           (others=>'0'));
            r_busy_sync <= (others=>'0');               
         elsif rising_edge(clk_ref_i) then
            --sync fsm busy to ref clk
            r_busy_sync(0) <= s_slave_regs_i.STATUS(c_STAT_BUSY);
            r_busy_sync(1) <= r_busy_sync(0);
            r_busy_sync(2) <= r_busy_sync(1);
            
            -- only capture once. wait for falling edge of fsm busy before new capture
            if (channel_i.valid = '1' and r_channel.valid = '0') then
               r_channel <= channel_i; -- TODO: this is a multicycle path. Make sure it's in the clock constraints!
               report "CAPTURE!" severity note;
            else
               -- reset capture state if falling edge of fsm busy detected
               r_channel.valid <= r_channel.valid and not (r_busy_sync(2) and not r_busy_sync(1));  
            end if;
         end if; -- clk           
   end process channel_in;

   channel_valid_sync : process(clk_sys_i, s_rst_n)
   begin
         -- sync channel captured to ref
         if(s_rst_n = '0') then
            r_valid_sync <= (others=>'0');
         elsif rising_edge(clk_sys_i) then
            r_valid_sync(0) <= r_channel.valid; -- channel_i.valid is too short as a pulse. use register instead
            r_valid_sync(1) <= r_valid_sync(0);
            r_valid_sync(2) <= r_valid_sync(1);
         end if; -- clk           
   end process; 
   
   s_valid <= not r_valid_sync(2) and r_valid_sync(1); -- rising edge valid signal

   --+******************************************************************************************+
   --| ------------------------------  ram and memory management  ----------------------------- |
   --+******************************************************************************************+
   U_FIFO : generic_sync_fifo
    generic map(
      -- standard parameters
      g_data_width   => f_ceil_log2(g_ram_size),
      g_size         => g_ram_size,
      g_show_ahead   => true,
      g_with_empty   => true
      )
    port map(
      rst_n_i => s_rst_n,
      -- Port A
      clk_i    => clk_sys_i,
      we_i     => s_addr_pool_push,
      rd_i     => s_addr_pool_pop,
      d_i      => s_addr_pool_d,
      q_o      => s_addr_pool_q,
      empty_o  => s_addr_pool_empty
      );
   
  U_RAM : generic_spram
    generic map(
      -- standard parameters
      g_data_width               => c_ram_width,
      g_size                     => g_ram_size,
      g_with_byte_enable         => false,
      g_addr_conflict_resolution => "dont_care"
      )
    port map(
      rst_n_i => s_rst_n,
      -- Port A
      clk_i  => clk_sys_i,
      we_i   => s_ram_we,
      a_i    => s_ram_a,
      d_i    => r_ram_d,
      q_o    => s_ram_q
      );
   
   r_ram_d        <= f_ser_rec(r_d);
   s_q            <= f_dser_rec(s_ram_q);
   s_ram_a        <= r_ram_a;
   s_addr_pool_d  <= s_ram_a;
   
   
   --+******************************************************************************************+
   --| ---------------   ctrl slave & wishbone master signals except flow control ------------- |
   --+******************************************************************************************+
   INST_eca_ac_wbm : eca_ac_wbm_auto
   port map (
      clk_sys_i      => clk_sys_i, 
      rst_n_i        => rst_sys_n_i,
 
      slave_regs_o   => s_slave_regs_o, 
      slave_regs_i   => s_slave_regs_i, 
      slave_i        => s_slave_i, 
      slave_o        => s_slave_o 
   );
   
   s_slave_i      <= slave_i;
   slave_o        <= s_slave_o; 
   s_rst_n        <= rst_sys_n_i  and not s_slave_regs_o.CLEAR_ALL(0);
   
   s_slave_regs_i.MAX_MACROS <= std_logic_vector(to_unsigned(g_entries,  s_slave_regs_i.MAX_MACROS'length));
   s_slave_regs_i.MAX_SPACE  <= std_logic_vector(to_unsigned(g_ram_size, s_slave_regs_i.MAX_SPACE'length));
   
   master_cnt : process(clk_sys_i)
   begin
      if rising_edge(clk_sys_i) then
         if(s_rst_n  = '0') then
            r_op_cnt       <= (others => '0');
            r_ackerr_cnt   <= (others => '0');
         else
            if(master_out.cyc = '0') then
               r_op_cnt       <= (others => '0');
               r_ackerr_cnt   <= (others => '0');
            else
               if (master_out.cyc and master_out.stb and not master_i.stall) = '1'  then
                  r_op_cnt       <= r_op_cnt +1;
               end if;
               if((master_i.ack or master_i.err) = '1') then
                  r_ackerr_cnt   <= r_ackerr_cnt +1;
               end if;
            end if; -- cyc
         end if; -- rst           
      end if; --clk
   end process master_cnt;
   
   master_out.we  <= '1';
   master_out.sel <= s_q.wb.sel;
   master_out.adr <= s_q.wb.addr;
   
   mux_master_out : process(s_q, r_channel)
      variable v_sel : natural range 0 to 2**c_show_enum_bits-1;
      variable v_out : t_wishbone_data;
   begin
      v_sel := to_integer(s_q.ch_show);
      v_out := (others => '0');
      
      case v_sel is
         when c_SHOW_PAYLOAD  => v_out                := s_q.wb.data;
         when c_SHOW_FLAGS    => v_out(1 downto 0)    := r_channel.late & r_channel.conflict;
         when c_SHOW_EVT_HI   => v_out                := r_channel.event(63 downto 32);
         when c_SHOW_EVT_LO   => v_out                := r_channel.event(31 downto 0);
         when c_SHOW_PAR_HI   => v_out                := r_channel.param(63 downto 32);
         when c_SHOW_PAR_LO   => v_out                := r_channel.param(31 downto 0);
         when c_SHOW_TAG      => v_out                := r_channel.tag;
         when c_SHOW_TEF      => v_out                := r_channel.tef;
         when c_SHOW_T_HI     => v_out                := r_channel.deadline(63 downto 32);
         when c_SHOW_T_LO     => v_out                := r_channel.deadline(31 downto 0);
         when others          => v_out                := s_q.wb.data;              
      end case;
      
      master_out.dat <= v_out;
   end process mux_master_out;
   
   master_o       <= master_out;
   
   --+******************************************************************************************+
   --| ----------------------------   main state machine   ------------------------------------ |
   --+******************************************************************************************+
   main : process(clk_sys_i)
      variable v_macro_idx : natural;
      variable v_wb        : std_logic;
   begin
      if rising_edge(clk_sys_i) then
         if(s_rst_n = '0') then
            s_slave_regs_i.ERR         <= '0';
            s_slave_regs_i.STALL       <= '1';
            s_slave_regs_i.STATUS      <= (others => '0');
            s_slave_regs_i.LAST_EXEC   <= (others => '0');
            s_slave_regs_i.LAST_REC    <= (others => '0');
            s_slave_regs_i.MACRO_QTY   <= (others => '0');
            s_slave_regs_i.SPACE_LEFT  <= std_logic_vector(to_unsigned(g_ram_size, s_slave_regs_i.SPACE_LEFT'length));
            s_addr_pool_pop            <= '0';
            s_addr_pool_push           <= '0';
            master_out.cyc             <= '0';
            master_out.stb             <= '0';
            r_op_write                 <= '0';
            r_ram_a                    <= (others => '1'); -- so the first addr for pool prep will be 0x0 by overflow
            r_free                     <= (others => '1');
            r_wb_addr_d                <= (others => '0');
            s_ram_we                   <= '0';
         else
            -- not rdy. prep address pool and ptr array
            if(s_slave_regs_i.STATUS(c_STAT_RDY) =  '0') then 
               -- prep address pool
               if(to_integer(unsigned(r_ram_a)) < g_entries) then -- abuse ram address as index and init ptr array
                  r_ptr(to_integer(unsigned(r_ram_a))) <= (others => '0');
               end if;
               
               s_addr_pool_push  <= '1';
               r_ram_a           <= std_logic_vector(unsigned(r_ram_a) +1);
               if(r_ram_a = std_logic_vector(to_unsigned(g_ram_size-2 , r_ram_a'length))) then
                  s_slave_regs_i.STATUS(c_STAT_RDY)   <= '1';
               end if;
            else 
               -- normal operation
               s_slave_regs_i.STALL <= '0';
               s_slave_regs_i.ERR   <= '0';
               s_addr_pool_pop      <= '0';
               s_addr_pool_push     <= '0';
               master_out.stb       <= '0';
               s_ram_we             <= '0';
                  
               case r_state is
                  when st_IDLE      => -- choice execute macro
                                       v_wb        := '0';
                                       if((s_valid or s_slave_regs_o.EXEC_WE) = '1') then
                                          if(s_slave_regs_o.ENABLE = "1") then
                                             report "EXEC!" severity note;      
                                             -- select channelif / wbif
                                             if(s_valid = '1') then
                                                v_macro_idx := to_integer(unsigned(r_channel.tag(f_ceil_log2(g_entries)-1 downto 0 )));
                                                v_wb        := '0';
                                             else
                                                v_macro_idx := to_integer(unsigned(s_slave_regs_o.EXEC));
                                                v_wb        := '1';
                                             end if;
                                             
                                             if (v_macro_idx < g_entries) then  
                                                if(r_free(v_macro_idx) = '0') then -- is there a macro at that index?
                                                   s_slave_regs_i.LAST_EXEC   <= std_logic_vector(to_unsigned(v_macro_idx, s_slave_regs_i.LAST_EXEC'length));
                                                   r_ram_a                    <= r_ptr(v_macro_idx);
                                                   s_slave_regs_i.STALL       <= '1';
                                                   s_slave_regs_i.STATUS(c_STAT_BUSY)   <= '1';
                                                   r_state                    <= st_TX_LOAD;
                                                else
                                                   s_slave_regs_i.ERR         <= v_wb;  
                                                end if;
                                             else
                                                s_slave_regs_i.ERR         <= v_wb;    
                                             end if;      
                                          end if; --if enable
                                          
                                       -- choice - record macro ( wbslif only)
                                       elsif(s_slave_regs_o.REC_WE = '1') then
                                          report "RECORD!" severity note; 
                                          v_macro_idx := to_integer(unsigned(s_slave_regs_o.REC));
                                          if (v_macro_idx < g_entries) then 
                                             -- do we have addresses left in the pool and is the requested index free?
                                             if(s_addr_pool_empty = '0' and r_free(v_macro_idx) = '1') then
                                                s_slave_regs_i.LAST_REC                         <= s_slave_regs_o.REC;
                                                s_slave_regs_i.MACRO_QTY                        <= std_logic_vector(unsigned(s_slave_regs_i.MACRO_QTY) +1);
                                                r_ptr(v_macro_idx)                              <= s_addr_pool_q;
                                                r_free(v_macro_idx)                             <= '0';
                                                r_op_write                                      <= '0';
                                                s_slave_regs_i.STATUS(c_STAT_BUSY)              <= '1'; 
                                                r_state                                         <= st_REC_ADDR;
                                             else
                                                s_slave_regs_i.ERR         <= '1';  
                                             end if;
                                          else
                                             s_slave_regs_i.ERR         <= '1';   
                                          end if;     
                                      
                                      -- choice - clear macro ( wbslif only)
                                       elsif(s_slave_regs_o.CLEAR_IDX_WE = '1') then
                                          report "CLEAR!" severity note; 
                                          v_macro_idx := to_integer(unsigned(s_slave_regs_o.CLEAR_IDX));
                                          if (v_macro_idx < g_entries) then
                                             if (r_free(v_macro_idx) = '0') then
                                                s_slave_regs_i.MACRO_QTY   <= std_logic_vector(unsigned(s_slave_regs_i.MACRO_QTY) -1);
                                                r_ram_a                    <= r_ptr(v_macro_idx);
                                                s_addr_pool_push           <= '1';
                                                r_free(v_macro_idx)        <= '1';
                                                s_slave_regs_i.STATUS(c_STAT_BUSY)   <= '1';
                                                r_state                    <= st_CLR_LOAD;
                                             else
                                                s_slave_regs_i.ERR         <= '1';  
                                             end if;
                                          else
                                             s_slave_regs_i.ERR         <= '1';   
                                          end if;      
                                       end if;
                                          
                                       
                  -- record macro                     
                  when st_REC_ADDR  => if(s_slave_regs_o.REC_WE = '1') then
                                          -- if an op was queued: write element, write has no next ptr
                                          r_d.has_next         <= '0';
                                          s_ram_we             <= r_op_write;
                                          r_op_write           <= '0';
                                          s_slave_regs_i.STALL <= '1';
                                          s_slave_regs_i.STATUS(c_STAT_BUSY) <= '0';   
                                          r_state              <= st_IDLE;
                                       elsif(s_slave_regs_o.REC_FIFO_WE = '1') then
                                          if(s_addr_pool_empty = '1') then -- no more addresses? finalize normally and go idle
                                             r_d.has_next         <= '0';
                                             s_ram_we             <= r_op_write;
                                             r_op_write           <= '0'; 
                                             s_slave_regs_i.STATUS(c_STAT_BUSY) <= '0'; 
                                             r_state              <= st_IDLE;
                                          else
                                             -- if an op was queued: write element, write has a next, write ptr
                                             r_d.has_next      <= '1';
                                             r_d.pnext         <= s_addr_pool_q;
                                             s_ram_we          <= r_op_write;
                                             r_op_write        <= '0';
                                             -- get wb addr from fifo port
                                             r_wb_addr_d       <= s_slave_regs_o.REC_FIFO;
                                             r_state           <= st_REC_DATA;
                                          end if; 
                                       end if;
                                       
                  when st_REC_DATA  => if(s_slave_regs_o.REC_FIFO_WE = '1') then
                                          -- set RAM to first free address
                                          -- get wb data from fifo port, get a new address for next element
                                          r_ram_a           <= s_addr_pool_q;
                                          s_addr_pool_pop   <= '1';
                                          r_d.wb.addr       <= r_wb_addr_d;
                                          r_d.wb.data       <= s_slave_regs_o.REC_FIFO;
                                          r_state           <= st_REC_META;
                                       end if;    
                                       
                  when st_REC_META  => if(s_slave_regs_o.REC_FIFO_WE = '1') then
                                          -- get wb metadate from fifo port, queue operation
                                          r_d.ch_show <= unsigned(s_slave_regs_o.REC_FIFO(c_META_FLAGS_MSB downto c_META_FLAGS_LSB));
                                          r_d.wb.drop <= s_slave_regs_o.REC_FIFO(c_META_DROP); 
                                          r_d.wb.sel  <= s_slave_regs_o.REC_FIFO(c_META_SEL_MSB downto c_META_SEL_LSB);
                                          r_free(to_integer(unsigned(s_slave_regs_i.LAST_REC))) <= '0';
                                          s_slave_regs_i.SPACE_LEFT <= std_logic_vector(unsigned(s_slave_regs_i.SPACE_LEFT) -1);
                                          r_op_write  <= '1';
                                          r_state     <= st_REC_ADDR; 
                                       end if;
                  
                  -- execute macro
                  when st_TX_LOAD   => s_slave_regs_i.STALL <= '1';
                                       master_out.cyc       <= '1';
                                       master_out.stb       <= '1';
                                       r_state              <= st_TX;
                                        
                  when st_TX        => s_slave_regs_i.STALL <= '1';
                                       if((master_out.cyc and master_out.stb and not master_i.stall) = '1') then
                                          -- do we need to pause ?
                                          master_out.stb <= '0';
                                          if( (not s_q.has_next or s_q.wb.drop) = '1') then
                                             r_state  <= st_TX_WAIT;   
                                          else
                                             -- continue sending
                                             r_ram_a  <= s_q.pnext(r_ram_a'range);
                                             r_state  <= st_TX_LOAD;
                                          end if;
                                       else
                                         master_out.stb <= '1';
                                       end if; 
                  
                  when st_TX_WAIT   => s_slave_regs_i.STALL <= '1';
                                       --wait for all ops to be ackknowledged
                                       if(r_ackerr_cnt = r_op_cnt) then
                                          -- is there more to send?
                                          master_out.cyc <= '0';
                                          if( s_q.has_next = '1') then
                                             r_ram_a        <= s_q.pnext(r_ram_a'range);
                                             r_state        <= st_TX_LOAD;
                                          else
                                             s_slave_regs_i.STATUS(c_STAT_BUSY) <= '0';
                                             r_state        <= st_IDLE;
                                          end if;   
                                       end if;
                  
                  -- clear macro
                  when st_CLR_LOAD  => s_slave_regs_i.STALL <= '1';
                                       r_state              <= st_CLR;
                                             
                  when st_CLR       => s_slave_regs_i.STALL <= '1';
                                       -- traverse macro, return all addresses to the pool
                                       s_slave_regs_i.SPACE_LEFT <= std_logic_vector(unsigned(s_slave_regs_i.SPACE_LEFT) +1);
                                       if(s_q.has_next = '1') then
                                          s_addr_pool_push  <= '1';
                                          r_ram_a           <= s_q.pnext(r_ram_a'range);
                                          r_state           <= st_CLR_LOAD;
                                       else
                                          s_slave_regs_i.STATUS(c_STAT_BUSY) <= '0';
                                          r_state  <= st_IDLE;
                                       end if;
                  
                  when others       => r_state <= st_IDLE;               
               end case;
            end if; -- prep address pool
         end if; -- nrst   
      end if; -- clock
   
   end process main;
   
end rtl;
