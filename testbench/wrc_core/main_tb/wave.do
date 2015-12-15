onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/DUT/PERIPH/rst_n_i
add wave -noupdate /main/DUT/rst_net_n
add wave -noupdate /main/DUT/rst_wrc_n
add wave -noupdate /main/DUT/clk_sys_i
add wave -noupdate -group Minic /main/DUT/MINI_NIC/src_o
add wave -noupdate -group Minic /main/DUT/MINI_NIC/src_i
add wave -noupdate -group Minic /main/DUT/MINI_NIC/snk_o
add wave -noupdate -group Minic /main/DUT/MINI_NIC/snk_i
add wave -noupdate -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/nrx_avail
add wave -noupdate -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/nrx_bufstart
add wave -noupdate -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/nrx_bufsize
add wave -noupdate -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/nrx_mem_a_saved
add wave -noupdate -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/nrx_buf_full
add wave -noupdate -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/nrx_valid
add wave -noupdate -group Minic -expand /main/DUT/MINI_NIC/wb_i
add wave -noupdate -group Minic -expand /main/DUT/MINI_NIC/wb_o
add wave -noupdate -expand -group EP_INT -expand /main/DUT/U_Endpoint/snk_i
add wave -noupdate -expand -group EP_INT -expand /main/DUT/U_Endpoint/src_o
add wave -noupdate -expand -group EP_INT -group TX_PCS -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_fab_i
add wave -noupdate -expand -group EP_INT -group RX_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_RX_PCS/pcs_fab_o
add wave -noupdate -expand -group EP_INT -expand -group CRC -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/state
add wave -noupdate -expand -group EP_INT -expand -group CRC /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/sreg_enable
add wave -noupdate -expand -group EP_INT -expand -group CRC /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/q_data
add wave -noupdate -expand -group EP_INT -expand -group CRC /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/q_dvalid_out
add wave -noupdate -expand -group EP_INT -expand -group CRC /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/q_dvalid_in
add wave -noupdate -expand -group EP_INT -expand -group CRC /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/qfull
add wave -noupdate -group ext_snk /main/DUT/ext_snk_adr_i
add wave -noupdate -group ext_snk /main/DUT/ext_snk_dat_i
add wave -noupdate -group ext_snk /main/DUT/ext_snk_sel_i
add wave -noupdate -group ext_snk /main/DUT/ext_snk_cyc_i
add wave -noupdate -group ext_snk /main/DUT/ext_snk_stb_i
add wave -noupdate -group ext_snk /main/DUT/ext_snk_ack_o
add wave -noupdate -group ext_snk /main/DUT/ext_snk_err_o
add wave -noupdate -group ext_snk /main/DUT/ext_snk_stall_o
add wave -noupdate -group ext_src /main/DUT/ext_src_adr_o
add wave -noupdate -group ext_src /main/DUT/ext_src_dat_o
add wave -noupdate -group ext_src /main/DUT/ext_src_sel_o
add wave -noupdate -group ext_src /main/DUT/ext_src_cyc_o
add wave -noupdate -group ext_src /main/DUT/ext_src_stb_o
add wave -noupdate -group ext_src /main/DUT/ext_src_ack_i
add wave -noupdate -group ext_src /main/DUT/ext_src_err_i
add wave -noupdate -group ext_src /main/DUT/ext_src_stall_i
add wave -noupdate -expand -group EP_EXT -expand -group src /main/EP/src_cyc_o
add wave -noupdate -expand -group EP_EXT -expand -group src /main/EP/src_stb_o
add wave -noupdate -expand -group EP_EXT -expand -group src /main/EP/src_sel_o
add wave -noupdate -expand -group EP_EXT -expand -group src /main/EP/src_adr_o
add wave -noupdate -expand -group EP_EXT -expand -group src /main/EP/src_dat_o
add wave -noupdate -expand -group EP_EXT -expand -group src /main/EP/src_stall_i
add wave -noupdate -expand -group EP_EXT -expand -group src /main/EP/src_ack_i
add wave -noupdate -expand -group EP_EXT -expand -group src /main/EP/src_err_i
add wave -noupdate -expand -group EP_EXT -expand -group snk /main/EP/snk_cyc_i
add wave -noupdate -expand -group EP_EXT -expand -group snk /main/EP/snk_stb_i
add wave -noupdate -expand -group EP_EXT -expand -group snk /main/EP/snk_sel_i
add wave -noupdate -expand -group EP_EXT -expand -group snk /main/EP/snk_adr_i
add wave -noupdate -expand -group EP_EXT -expand -group snk /main/EP/snk_dat_i
add wave -noupdate -expand -group EP_EXT -expand -group snk /main/EP/snk_stall_o
add wave -noupdate -expand -group EP_EXT -expand -group snk /main/EP/snk_ack_o
add wave -noupdate -expand -group EP_EXT -expand -group snk /main/EP/snk_err_o
add wave -noupdate -expand -group EP_EXT -expand -group snk /main/EP/snk_rty_o
add wave -noupdate -expand -group EP_EXT -group TX_PCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_fab_i
add wave -noupdate -expand -group EP_EXT -group RX_PCS -expand /main/EP/U_PCS_1000BASEX/gen_8bit/U_RX_PCS/pcs_fab_o
add wave -noupdate -expand -group EP_EXT -expand -group RX_PATH -expand -subitemconfig {/main/EP/U_Rx_Path/fab_pipe(4) -expand /main/EP/U_Rx_Path/fab_pipe(5) -expand} /main/EP/U_Rx_Path/fab_pipe
add wave -noupdate -expand -group EP_EXT -expand -group RX_PATH /main/EP/U_Rx_Path/dreq_pipe
add wave -noupdate -expand -group EP_EXT -expand -group RX_PATH -expand -group CRC -height 16 /main/EP/U_Rx_Path/U_crc_size_checker/state
add wave -noupdate -expand -group EP_EXT -expand -group RX_PATH -expand -group CRC -expand /main/EP/U_Rx_Path/U_crc_size_checker/q_data
add wave -noupdate -expand -group EP_EXT -expand -group RX_PATH -expand -group CRC /main/EP/U_Rx_Path/U_crc_size_checker/sreg_enable
add wave -noupdate -expand -group EP_EXT -expand -group RX_PATH -expand -group CRC /main/EP/U_Rx_Path/U_crc_size_checker/q_dvalid_out
add wave -noupdate -expand -group EP_EXT -expand -group RX_PATH -expand -group CRC /main/EP/U_Rx_Path/U_crc_size_checker/q_dvalid_in
add wave -noupdate -expand -group EP_EXT -expand -group RX_PATH -expand -group CRC /main/EP/U_Rx_Path/U_crc_size_checker/qfull
add wave -noupdate -group WB /main/EP/wb_cyc_i
add wave -noupdate -group WB /main/EP/wb_stb_i
add wave -noupdate -group WB /main/EP/wb_we_i
add wave -noupdate -group WB /main/EP/wb_sel_i
add wave -noupdate -group WB /main/EP/wb_adr_i
add wave -noupdate -group WB /main/EP/wb_dat_i
add wave -noupdate -group WB /main/EP/wb_dat_o
add wave -noupdate -group WB /main/EP/wb_ack_o
add wave -noupdate -group WB /main/EP/wb_stall_o
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/clk_sys_i
add wave -noupdate -expand -group LBK -expand /main/WRF_LBK/X_LOOPBACK/wb_i
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/wb_o
add wave -noupdate -expand -group LBK -height 16 /main/WRF_LBK/X_LOOPBACK/lbk_rxfsm
add wave -noupdate -expand -group LBK -height 16 /main/WRF_LBK/X_LOOPBACK/lbk_txfsm
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/wrf_snk_i
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/wrf_snk_o
add wave -noupdate -expand -group LBK -expand /main/WRF_LBK/X_LOOPBACK/wrf_src_o
add wave -noupdate -expand -group LBK -expand /main/WRF_LBK/X_LOOPBACK/wrf_src_i
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/rcv_cnt
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/drp_cnt
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/fwd_cnt
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/ack_cnt
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/fsize
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/txsize
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/tx_cnt
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/ffifo_empty
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/ffifo_full
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/frame_wr
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/frame_rd
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/frame_in
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/frame_out
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/sfifo_empty
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/sfifo_full
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/fsize_wr
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/fsize_rd
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/fsize_in
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/fsize_out
add wave -noupdate -expand -group LBK /main/WRF_LBK/X_LOOPBACK/regs_fromwb.mcr_ena_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 2} {1607388000000 fs} 1}
configure wave -namecolwidth 204
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {0 fs} {8282223600 ps}
