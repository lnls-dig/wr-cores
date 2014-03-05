library IEEE;
--! Standard packages    
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library work;
use work.wishbone_pkg.all;

package wr_tlu_pkg is


subtype t_trigger is std_logic_vector(7 downto 0);
type    t_trigger_array is array (natural range <>) of t_trigger  ; 

 constant c_xwr_wb_timestamp_latch_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000007ff",
    product => (
    vendor_id     => x"0000000000000651", -- GSI
    device_id     => x"10051981",
    version       => x"00000002",
    date          => x"20120308",
    name          => "GSI_TM_LATCH_V2    ")));


 
component wr_tlu is
   generic( g_num_triggers : natural := 1;
            g_fifo_depth   : natural := 32;
            g_auto_msg     : boolean := false);  
   port (
      clk_ref_i         : in  std_logic;    -- tranceiver clock domain
      rst_ref_n_i       : in  std_logic;
      clk_sys_i         : in  std_logic;    -- local clock domain
      rst_sys_n_i       : in  std_logic;

      triggers_i        : in  t_trigger_array(g_num_triggers-1 downto 0);  -- trigger vectors for latch. meant to be used with 8bits from lvds derserializer for 1GhZ res
                                                                -- if you only have a normal signal, connect it as triggers_i(m) => (others => s_my_signal)     
      
      tm_tai_cyc_i      : in  std_logic_vector(63 downto 0);   -- TAI Timestamp in 8ns cycles  

      ctrl_slave_i      : in  t_wishbone_slave_in;             -- Wishbone slave interface (sys_clk domain)
      ctrl_slave_o      : out t_wishbone_slave_out;
      
      irq_master_o      : out t_wishbone_master_out;           -- msi irq src 
      irq_master_i      : in  t_wishbone_master_in
   );              

end component;



end wr_tlu_pkg;
