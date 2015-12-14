-- File Name         : /home/mkreider/hdlprojects/bel_projects/ip_cores/wr-cores/modules/wr_eca/eca_ac_wbm_auto_pkg.vhd
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
package eca_ac_wbm_auto_pkg is

   --+******************************************************************************************+
   --|  ------------------------------- WB Slaves - Adress Maps --------------------------------|
   --+******************************************************************************************+

   --| WBS Adr ------------------------------ slave --------------------------------------------|
      constant c_slave_STATUS_GET      : natural   := 16#00#;  -- r     0x000000ff, Shows if the device is rdy/busy
      constant c_slave_MAX_MACROS_GET  : natural   := 16#04#;  -- r     0xffffffff, Shows maximum number of macros
      constant c_slave_MAX_SPACE_GET   : natural   := 16#08#;  -- r     0xffffffff, Shows maximum memory space
      constant c_slave_ENABLE_GET      : natural   := 16#0c#;  -- rw    0x00000001, Turns device on/off
      constant c_slave_ENABLE_SET      : natural   := 16#10#;  -- rw    0x00000001, ""
      constant c_slave_ENABLE_CLR      : natural   := 16#14#;  -- rw    0x00000001, ""
      constant c_slave_EXEC_OWR        : natural   := 16#18#;  -- wfs   0x000000ff, Executes macro at idx
      constant c_slave_LAST_EXEC_GET   : natural   := 16#1c#;  -- r     0x000000ff, Shows idx of last executed macro
      constant c_slave_REC_OWR         : natural   := 16#20#;  -- wfs   0x000000ff, Records macro at idx
      constant c_slave_LAST_REC_GET    : natural   := 16#24#;  -- r     0x000000ff, Shows idx of last recorded macro
      constant c_slave_MACRO_QTY_GET   : natural   := 16#28#;  -- r     0x000000ff, Shows the number of macros in the ram
      constant c_slave_SPACE_LEFT_GET  : natural   := 16#2c#;  -- r     0x0000ffff, Shows number of free spaces in the RAM
      constant c_slave_CLEAR_ALL_OWR   : natural   := 16#30#;  -- wsp   0x00000001, Clears all macros
      constant c_slave_CLEAR_IDX_OWR   : natural   := 16#34#;  -- wfs   0x000000ff, Clears macro at idx
      constant c_slave_REC_FIFO_OWR    : natural   := 16#38#;  -- wf    0xffffffff, Recording fifo. 3 word sequences: #ADR# #VAL# #META#

   --+******************************************************************************************+
   --|  ------------------------- WB Slaves - Control Register Records -------------------------|
   --+******************************************************************************************+

   --| WBS Register Record ------------ slave --------------------------------------------------|
   
   type t_slave_regs_o is record
      ENABLE         : std_logic_vector(0 downto 0);  -- Turns device on/off
      EXEC           : std_logic_vector(7 downto 0);  -- Executes macro at idx
      EXEC_WE        : std_logic;                     -- WE flag
      REC            : std_logic_vector(7 downto 0);  -- Records macro at idx
      REC_WE         : std_logic;                     -- WE flag
      CLEAR_ALL      : std_logic_vector(0 downto 0);  -- Clears all macros
      CLEAR_IDX      : std_logic_vector(7 downto 0);  -- Clears macro at idx
      CLEAR_IDX_WE   : std_logic;                     -- WE flag
      REC_FIFO       : std_logic_vector(31 downto 0); -- Recording fifo. 3 word sequences: #ADR# #VAL# #META#
      REC_FIFO_WE    : std_logic;                     -- WE flag
   end record t_slave_regs_o;

   
   type t_slave_regs_i is record
      STALL          : std_logic;                     -- Stall control for outside entity
      ERR            : std_logic;                     -- Error control for outside entity
      STATUS         : std_logic_vector(7 downto 0);  -- Shows if the device is rdy/busy
      MAX_MACROS     : std_logic_vector(31 downto 0); -- Shows maximum number of macros
      MAX_SPACE      : std_logic_vector(31 downto 0); -- Shows maximum memory space
      LAST_EXEC      : std_logic_vector(7 downto 0);  -- Shows idx of last executed macro
      LAST_REC       : std_logic_vector(7 downto 0);  -- Shows idx of last recorded macro
      MACRO_QTY      : std_logic_vector(7 downto 0);  -- Shows the number of macros in the ram
      SPACE_LEFT     : std_logic_vector(15 downto 0); -- Shows number of free spaces in the RAM
   end record t_slave_regs_i;

   --| Component ---------------------- eca_ac_wbm_auto ----------------------------------------|
   component eca_ac_wbm_auto is
   Port(
      clk_sys_i      : in  std_logic;
      rst_n_i        : in  std_logic;

      slave_regs_i   : in  t_slave_regs_i;
      slave_regs_o   : out t_slave_regs_o;
      
      slave_i        : in  t_wishbone_slave_in  := ('0', '0', x"00000000", x"F", '0', x"00000000");
      slave_o        : out t_wishbone_slave_out
      
   );
   end component;

   constant c_eca_ac_wbm_slave_sdb : t_sdb_device := (
   abi_class     => x"0000", -- undocumented device
   abi_ver_major => x"01",
   abi_ver_minor => x"00",
   wbd_endian    => c_sdb_endian_big,
   wbd_width     => x"7", -- 8/16/32-bit port granularity
   sdb_component => (
   addr_first    => x"0000000000000000",
   addr_last     => x"000000000000003f",
   product => (
   vendor_id     => x"0000000000000651",
   device_id     => x"18415778",
   version       => x"00000001",
   date          => x"20150108",
   name          => "ECA ACTCHN WBM     ")));
   
end eca_ac_wbm_auto_pkg;
package body eca_ac_wbm_auto_pkg is
end eca_ac_wbm_auto_pkg;
