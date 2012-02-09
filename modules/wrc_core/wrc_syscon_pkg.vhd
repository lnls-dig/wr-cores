---------------------------------------------------------------------------------------
-- Title          : Wishbone slave core for WR Core System Controller
---------------------------------------------------------------------------------------
-- File           : wrc_syscon_pkg.vhd
-- Author         : auto-generated by wbgen2 from wrc_syscon_wb.wb
-- Created        : Wed Nov 30 15:12:21 2011
-- Standard       : VHDL'87
---------------------------------------------------------------------------------------
-- THIS FILE WAS GENERATED BY wbgen2 FROM SOURCE FILE wrc_syscon_wb.wb
-- DO NOT HAND-EDIT UNLESS IT'S ABSOLUTELY NECESSARY!
---------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package sysc_wbgen2_pkg is
  
  
  -- Input registers (user design -> WB slave)
  
  type t_sysc_in_registers is record
    gpsr_fmc_scl_i                           : std_logic;
    gpsr_fmc_sda_i                           : std_logic;
    gpsr_btn1_i                              : std_logic;
    gpsr_btn2_i                              : std_logic;
    hwfr_memsize_i                           : std_logic_vector(3 downto 0);
    tcr_tdiv_i                               : std_logic_vector(11 downto 0);
    tvr_i                                    : std_logic_vector(31 downto 0);
    end record;
  
  constant c_sysc_in_registers_init_value: t_sysc_in_registers := (
    gpsr_fmc_scl_i => '0',
    gpsr_fmc_sda_i => '0',
    gpsr_btn1_i => '0',
    gpsr_btn2_i => '0',
    hwfr_memsize_i => (others => '0'),
    tcr_tdiv_i => (others => '0'),
    tvr_i => (others => '0')
    );
    
    -- Output registers (WB slave -> user design)
    
    type t_sysc_out_registers is record
      rstr_trig_o                              : std_logic_vector(27 downto 0);
      rstr_trig_wr_o                           : std_logic;
      rstr_rst_o                               : std_logic;
      gpsr_led_stat_o                          : std_logic;
      gpsr_led_link_o                          : std_logic;
      gpsr_fmc_scl_o                           : std_logic;
      gpsr_fmc_scl_load_o                      : std_logic;
      gpsr_fmc_sda_o                           : std_logic;
      gpsr_fmc_sda_load_o                      : std_logic;
      gpsr_net_rst_o                           : std_logic;
      gpcr_led_stat_o                          : std_logic;
      gpcr_led_link_o                          : std_logic;
      gpcr_fmc_scl_o                           : std_logic;
      gpcr_fmc_sda_o                           : std_logic;
      tcr_enable_o                             : std_logic;
      end record;
    
    constant c_sysc_out_registers_init_value: t_sysc_out_registers := (
      rstr_trig_o => (others => '0'),
      rstr_trig_wr_o => '0',
      rstr_rst_o => '0',
      gpsr_led_stat_o => '0',
      gpsr_led_link_o => '0',
      gpsr_fmc_scl_o => '0',
      gpsr_fmc_scl_load_o => '0',
      gpsr_fmc_sda_o => '0',
      gpsr_fmc_sda_load_o => '0',
      gpsr_net_rst_o => '0',
      gpcr_led_stat_o => '0',
      gpcr_led_link_o => '0',
      gpcr_fmc_scl_o => '0',
      gpcr_fmc_sda_o => '0',
      tcr_enable_o => '0'
      );
    function "or" (left, right: t_sysc_in_registers) return t_sysc_in_registers;
    function f_x_to_zero (x:std_logic) return std_logic;
end package;

package body sysc_wbgen2_pkg is
function f_x_to_zero (x:std_logic) return std_logic is
begin
if(x = 'X' or x = 'U') then
return '0';
else
return x;
end if; 
end function;
function "or" (left, right: t_sysc_in_registers) return t_sysc_in_registers is
variable tmp: t_sysc_in_registers;
begin
tmp.gpsr_fmc_scl_i := left.gpsr_fmc_scl_i or right.gpsr_fmc_scl_i;
tmp.gpsr_fmc_sda_i := left.gpsr_fmc_sda_i or right.gpsr_fmc_sda_i;
tmp.gpsr_btn1_i := left.gpsr_btn1_i or right.gpsr_btn1_i;
tmp.gpsr_btn2_i := left.gpsr_btn2_i or right.gpsr_btn2_i;
tmp.hwfr_memsize_i := left.hwfr_memsize_i or right.hwfr_memsize_i;
tmp.tcr_tdiv_i := left.tcr_tdiv_i or right.tcr_tdiv_i;
tmp.tvr_i := left.tvr_i or right.tvr_i;
return tmp;
end function;
end package body;
