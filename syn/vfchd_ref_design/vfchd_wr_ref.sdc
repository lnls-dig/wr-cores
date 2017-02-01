# Clock inputs
create_clock -name clk_10m_ext -period 100.0 [get_ports dio5_clk_i]
create_clock -name clk_125m    -period   8.0 [get_ports clk_board_125m_i]
create_clock -name clk_20m     -period  50.0 [get_ports clk_board_20m_i]

# Derive the PLL Output clocks automatically
derive_pll_clocks
derive_clock_uncertainty

# splitting of PHY clocks based on pexarria5 project from GSI
set_clock_groups -asynchronous                                                       \
    -group { clk_10m_ext                                                             \
             cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_ext_ref_pll|* }             \
    -group { clk_125m                                                                \
             cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_sys_clk_pll|*|general[0]*   \
             cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_sys_clk_pll|*|general[1]* } \
    -group { clk_20m                                                                 \
             cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_dmtd_clk_pll|* }            \
    -group { cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_phy|*.cdr_refclk*           \
             cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_phy|*.cmu_pll.*             \
             cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_phy|*|av_tx_pma|*           \
             cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_phy|*|inst_av_pcs|*|tx* }   \
    -group { cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_phy|*|clk90bdes             \
             cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_phy|*|clk90b                \
             cmp_xwrc_board_vfchd|cmp_xwrc_platform|*cmp_phy|*|rcvdclkpma }

# False paths from/to all otherwise unconstrained I/O
set_false_path -from [get_ports *]
set_false_path -to [get_ports *]
