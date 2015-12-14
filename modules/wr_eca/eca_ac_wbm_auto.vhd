-- File Name         : /home/mkreider/hdlprojects/bel_projects/ip_cores/wr-cores/modules/wr_eca/eca_ac_wbm_auto.vhd
-- Design Unit Name  : eca_ac_wbm_auto
-- Revision          : 0.0.1
-- Author            : M. Kreider
-- Created           : 08/01/2015

-- ***********************************************************
-- ** WARNING - THIS IS AUTO-GENERATED CODE! DO NOT MODIFY! **
-- ***********************************************************
--
-- If you want to change the interface,
-- modify eca_ac_wbm.xml and re-run 'python wbgenplus.py eca_ac_wbm.xml' !

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.wishbone_pkg.all;
use work.eca_ac_wbm_auto_pkg.all;

entity eca_ac_wbm_auto is
Port(
   clk_sys_i      : in  std_logic;
   rst_n_i        : in  std_logic;

   slave_regs_i   : in  t_slave_regs_i;
   slave_regs_o   : out t_slave_regs_o;
   
   slave_i        : in  t_wishbone_slave_in  := ('0', '0', x"00000000", x"F", '0', x"00000000");
   slave_o        : out t_wishbone_slave_out
   
);
end eca_ac_wbm_auto;

architecture rtl of eca_ac_wbm_auto is

   --+******************************************************************************************+
   --|  ------------------------------------- WB Registers -------------------------------------|
   --+******************************************************************************************+

   --| WBS Regs ---------------------------- slave ---------------------------------------------|
   signal r_slave             : t_slave_regs_o;
   signal s_slave             : t_slave_regs_i;
   signal r_slave_out_stall   : std_logic;
   signal r_slave_out_ack0,
          r_slave_out_ack1,
          r_slave_out_err0,
          r_slave_out_err1    : std_logic;
   signal r_slave_out_dat0,
          r_slave_out_dat1    : std_logic_vector(31 downto 0);



begin

   --+******************************************************************************************+
   --| WBS FSM ------------------------------ slave --------------------------------------------|
   --+******************************************************************************************+
   slave : process(clk_sys_i)
      variable v_dat_i  : t_wishbone_data;
      variable v_dat_o  : t_wishbone_data;
      variable v_adr    : natural;
      variable v_page   : natural;
      variable v_sel    : t_wishbone_byte_select;
      variable v_we     : std_logic;
      variable v_en     : std_logic;
   begin
      if rising_edge(clk_sys_i) then
         if(rst_n_i = '0') then
            r_slave.ENABLE       <= (others => '0');
            r_slave.EXEC         <= (others => '0');
            r_slave.EXEC_WE      <= '0'; --  pulse
            r_slave.REC          <= (others => '0');
            r_slave.REC_WE       <= '0'; --  pulse
            r_slave.CLEAR_ALL    <= (others => '0');
            r_slave.CLEAR_IDX    <= (others => '0');
            r_slave.CLEAR_IDX_WE <= '0'; --  pulse
            r_slave.REC_FIFO     <= (others => '0');
            r_slave.REC_FIFO_WE  <= '0'; --  pulse
            r_slave_out_stall    <= '0';
            r_slave_out_ack0     <= '0';
            r_slave_out_err0     <= '0';
            r_slave_out_dat0     <= (others => '0');
            r_slave_out_ack1     <= '0';
            r_slave_out_err1     <= '0';
            r_slave_out_dat1     <= (others => '0');
         else
            -- short names
            v_dat_i           := slave_i.dat;
            v_adr             := to_integer(unsigned(slave_i.adr(5 downto 2)) & "00");
            v_sel             := slave_i.sel;
            v_en              := slave_i.cyc and slave_i.stb and not (r_slave_out_stall or slave_regs_i.STALL);
            v_we              := slave_i.we;

            --interface outputs
            r_slave_out_stall   <= '0';
            r_slave_out_ack0    <= '0';
            r_slave_out_err0    <= '0';
            r_slave_out_dat0    <= (others => '0');

            r_slave_out_ack1    <= r_slave_out_ack0;
            r_slave_out_err1    <= r_slave_out_err0;
            r_slave_out_dat1    <= r_slave_out_dat0;

            r_slave.EXEC_WE      <= '0';              -- EXEC pulse
            r_slave.REC_WE       <= '0';              -- REC pulse
            r_slave.CLEAR_ALL    <= (others => '0');  -- CLEAR_ALL pulse
            r_slave.CLEAR_IDX_WE <= '0';              -- CLEAR_IDX pulse
            r_slave.REC_FIFO_WE  <= '0';              -- REC_FIFO pulse
            
            if(v_en = '1') then
               r_slave_out_ack0  <= '1';
               if(v_we = '1') then
                  -- WISHBONE WRITE ACTIONS
                  case v_adr is
                     when c_slave_ENABLE_SET    => r_slave.ENABLE       <= f_wb_wr(r_slave.ENABLE,    v_dat_i, v_sel, "set"); -- Turns device on/off
                     when c_slave_ENABLE_CLR    => r_slave.ENABLE       <= f_wb_wr(r_slave.ENABLE,    v_dat_i, v_sel, "clr"); -- ""
                     when c_slave_EXEC_OWR      => r_slave.EXEC         <= f_wb_wr(r_slave.EXEC,      v_dat_i, v_sel, "owr"); -- Executes macro at idx
                                                   r_slave.EXEC_WE      <= '1';                                               --    EXEC write enable
                                                   r_slave_out_stall    <= '1';                                               --    EXEC auto stall
                     when c_slave_REC_OWR       => r_slave.REC          <= f_wb_wr(r_slave.REC,       v_dat_i, v_sel, "owr"); -- Records macro at idx
                                                   r_slave.REC_WE       <= '1';                                               --    REC write enable
                                                   r_slave_out_stall    <= '1';                                               --    REC auto stall
                     when c_slave_CLEAR_ALL_OWR => r_slave.CLEAR_ALL    <= f_wb_wr(r_slave.CLEAR_ALL, v_dat_i, v_sel, "owr"); -- Clears all macros
                                                   r_slave_out_stall    <= '1';                                               --    CLEAR_ALL auto stall
                     when c_slave_CLEAR_IDX_OWR => r_slave.CLEAR_IDX    <= f_wb_wr(r_slave.CLEAR_IDX, v_dat_i, v_sel, "owr"); -- Clears macro at idx
                                                   r_slave.CLEAR_IDX_WE <= '1';                                               --    CLEAR_IDX write enable
                                                   r_slave_out_stall    <= '1';                                               --    CLEAR_IDX auto stall
                     when c_slave_REC_FIFO_OWR  => r_slave.REC_FIFO     <= f_wb_wr(r_slave.REC_FIFO,  v_dat_i, v_sel, "owr"); -- Recording fifo. 3 word sequences: #ADR# #VAL# #META#
                                                   r_slave.REC_FIFO_WE  <= '1';                                               --    REC_FIFO write enable
                     when others => r_slave_out_ack0 <= '0'; r_slave_out_err0 <= '1';
                  end case;
               else
                  -- WISHBONE READ ACTIONS
                  case v_adr is
                     when c_slave_STATUS_GET       => r_slave_out_dat0(7 downto 0)  <= s_slave.STATUS;      -- Shows if the device is rdy/busy
                     when c_slave_MAX_MACROS_GET   => r_slave_out_dat0(31 downto 0) <= s_slave.MAX_MACROS;  -- Shows maximum number of macros
                     when c_slave_MAX_SPACE_GET    => r_slave_out_dat0(31 downto 0) <= s_slave.MAX_SPACE;   -- Shows maximum memory space
                     when c_slave_ENABLE_GET       => r_slave_out_dat0(0 downto 0)  <= r_slave.ENABLE;      -- Turns device on/off
                     when c_slave_LAST_EXEC_GET    => r_slave_out_dat0(7 downto 0)  <= s_slave.LAST_EXEC;   -- Shows idx of last executed macro
                     when c_slave_LAST_REC_GET     => r_slave_out_dat0(7 downto 0)  <= s_slave.LAST_REC;    -- Shows idx of last recorded macro
                     when c_slave_MACRO_QTY_GET    => r_slave_out_dat0(7 downto 0)  <= s_slave.MACRO_QTY;   -- Shows the number of macros in the ram
                     when c_slave_SPACE_LEFT_GET   => r_slave_out_dat0(15 downto 0) <= s_slave.SPACE_LEFT;  -- Shows number of free spaces in the RAM
                     when others => r_slave_out_ack0 <= '0'; r_slave_out_err0 <= '1';
                  end case;
               end if; -- v_we
            end if; -- v_en
         end if; -- rst
      end if; -- clk edge
   end process;

   slave_regs_o   <= r_slave;
   s_slave        <= slave_regs_i;
   slave_o.stall  <= r_slave_out_stall or slave_regs_i.STALL;
   slave_o.dat    <= r_slave_out_dat1;
   slave_o.ack    <= r_slave_out_ack1 and not slave_regs_i.ERR;
   slave_o.err    <= r_slave_out_err1 or      slave_regs_i.ERR;


end rtl;
