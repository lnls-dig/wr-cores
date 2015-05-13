onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/DUT/PERIPH/rst_n_i
add wave -noupdate /main/DUT/rst_net_n
add wave -noupdate /main/DUT/rst_wrc_n
add wave -noupdate /main/DUT/clk_sys_i
add wave -noupdate -group WR-CORE /main/DUT/wb_adr_i
add wave -noupdate -group WR-CORE /main/DUT/wb_dat_i
add wave -noupdate -group WR-CORE /main/DUT/wb_dat_o
add wave -noupdate -group WR-CORE /main/DUT/wb_sel_i
add wave -noupdate -group WR-CORE /main/DUT/wb_we_i
add wave -noupdate -group WR-CORE /main/DUT/wb_cyc_i
add wave -noupdate -group WR-CORE /main/DUT/wb_stb_i
add wave -noupdate -group WR-CORE /main/DUT/wb_ack_o
add wave -noupdate -group WR-CORE /main/DUT/wb_err_o
add wave -noupdate -group WR-CORE /main/DUT/wb_rty_o
add wave -noupdate -group WR-CORE /main/DUT/wb_stall_o
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_dat_i
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_adr_i
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_sel_i
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_cyc_i
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_stb_i
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_we_i
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_stall_o
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_ack_o
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_err_o
add wave -noupdate -expand -group Endpoint -group {FABRIC IF} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/snk_rty_o
add wave -noupdate -expand -group Endpoint -expand -group {FABRIC RX} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/src_dat_o
add wave -noupdate -expand -group Endpoint -expand -group {FABRIC RX} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/src_adr_o
add wave -noupdate -expand -group Endpoint -expand -group {FABRIC RX} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/src_sel_o
add wave -noupdate -expand -group Endpoint -expand -group {FABRIC RX} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/src_cyc_o
add wave -noupdate -expand -group Endpoint -expand -group {FABRIC RX} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/src_stb_o
add wave -noupdate -expand -group Endpoint -expand -group {FABRIC RX} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/src_we_o
add wave -noupdate -expand -group Endpoint -expand -group {FABRIC RX} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/src_stall_i
add wave -noupdate -expand -group Endpoint -expand -group {FABRIC RX} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/src_ack_i
add wave -noupdate -expand -group Endpoint -expand -group {FABRIC RX} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/src_err_i
add wave -noupdate -expand -group Endpoint -expand -group {HEADER PROCESSOR} -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Header_Processor/state
add wave -noupdate -expand -group Endpoint -expand -group {HEADER PROCESSOR} -radix unsigned /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Header_Processor/counter
add wave -noupdate -expand -group Endpoint -expand -group {HEADER PROCESSOR} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Header_Processor/sof_p1
add wave -noupdate -expand -group Endpoint -expand -group {HEADER PROCESSOR} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Header_Processor/sof_reg
add wave -noupdate -expand -group Endpoint -expand -group {HEADER PROCESSOR} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Header_Processor/eof_p1
add wave -noupdate -expand -group Endpoint -expand -group {HEADER PROCESSOR} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Header_Processor/tx_en
add wave -noupdate -expand -group Endpoint -expand -group {HEADER PROCESSOR} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Header_Processor/bitsel_d
add wave -noupdate -expand -group Endpoint -expand -group {HEADER PROCESSOR} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Header_Processor/pcs_busy_i
add wave -noupdate -expand -group Endpoint -expand -group {HEADER PROCESSOR} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Header_Processor/src_dreq_i
add wave -noupdate -expand -group Endpoint -group {TX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/fab_pipe(0)
add wave -noupdate -expand -group Endpoint -group {TX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/fab_pipe(1)
add wave -noupdate -expand -group Endpoint -group {TX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/fab_pipe(2)
add wave -noupdate -expand -group Endpoint -group {TX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/fab_pipe(3)
add wave -noupdate -expand -group Endpoint -group {TX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/fab_pipe(4)
add wave -noupdate -expand -group Endpoint -group {TX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/dreq_pipe
add wave -noupdate -expand -group Endpoint -expand -group TX_CRC -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Insert_CRC/state
add wave -noupdate -expand -group Endpoint -expand -group TX_CRC /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/U_Insert_CRC/in_payload
add wave -noupdate -expand -group Endpoint -expand -group TX_PCS -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_fab_i
add wave -noupdate -expand -group Endpoint -expand -group TX_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_error_o
add wave -noupdate -expand -group Endpoint -expand -group TX_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_busy_o
add wave -noupdate -expand -group Endpoint -expand -group TX_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_dreq_o
add wave -noupdate -expand -group Endpoint -expand -group TX_PCS -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_state
add wave -noupdate -expand -group Endpoint -expand -group TX_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_almost_full
add wave -noupdate -expand -group Endpoint -group RX_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_RX_PCS/pcs_busy_o
add wave -noupdate -expand -group Endpoint -group RX_PCS -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_RX_PCS/pcs_fab_o
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(0)
add wave -noupdate -expand -group {RX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(1)
add wave -noupdate -expand -group {RX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(2)
add wave -noupdate -expand -group {RX PATH} -expand -group PFilter /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_packet_filter/U_packet_filter/rst_n_rx_i
add wave -noupdate -expand -group {RX PATH} -expand -group PFilter /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_packet_filter/U_packet_filter/drop_o
add wave -noupdate -expand -group {RX PATH} -expand -group PFilter /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_packet_filter/U_packet_filter/pclass_o
add wave -noupdate -expand -group {RX PATH} -expand -group PFilter /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_packet_filter/U_packet_filter/done_int
add wave -noupdate -expand -group {RX PATH} -expand -group PFilter /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_packet_filter/U_packet_filter/done_o
add wave -noupdate -expand -group {RX PATH} -expand -group PFilter /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_packet_filter/U_packet_filter/regs
add wave -noupdate -expand -group {RX PATH} -expand -group PFilter /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_packet_filter/U_packet_filter/pfcr0_enable_rxclk
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(3)
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/dreq_pipe(3)
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Rx_Clock_Align_FIFO/full_o
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Rx_Clock_Align_FIFO/empty_o
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Rx_Clock_Align_FIFO/almostfull_o
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Rx_Clock_Align_FIFO/fifo_we
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Rx_Clock_Align_FIFO/rx_rdreq
add wave -noupdate -expand -group {RX PATH} -radix unsigned /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Rx_Clock_Align_FIFO/U_FIFO/U_Inferred_FIFO/rd_count_o
add wave -noupdate -expand -group {RX PATH} -radix unsigned /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Rx_Clock_Align_FIFO/U_FIFO/U_Inferred_FIFO/wr_count_o
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(4)
add wave -noupdate -expand -group {RX PATH} -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Insert_OOB/state
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/dreq_pipe(4)
add wave -noupdate -expand -group {RX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(5)
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/dreq_pipe(5)
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/size_check_ok
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/crc_match
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/q_bytesel
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/oob_in
add wave -noupdate -expand -group {RX PATH} -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_crc_size_checker/state
add wave -noupdate -expand -group {RX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(6)
add wave -noupdate -expand -group {RX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(7)
add wave -noupdate -expand -group {RX PATH} -radix unsigned /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/level_o
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/BUF_FIFO/empty_o
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/BUF_FIFO/full_o
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/BUF_FIFO/almost_empty_o
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/BUF_FIFO/almost_full_o
add wave -noupdate -expand -group {RX PATH} -radix unsigned /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/BUF_FIFO/count_o
add wave -noupdate -expand -group {RX PATH} -expand -group rx_buf -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/state
add wave -noupdate -expand -group {RX PATH} -expand -group rx_buf /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/in_prev_addr
add wave -noupdate -expand -group {RX PATH} -expand -group rx_buf /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/q_in
add wave -noupdate -expand -group {RX PATH} -expand -group rx_buf /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/q_in_valid
add wave -noupdate -expand -group {RX PATH} -expand -group rx_buf /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/q_drop
add wave -noupdate -expand -group {RX PATH} -expand -group rx_buf /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/BUF_FIFO/U_Inferred_FIFO/we_int
add wave -noupdate -expand -group {RX PATH} -expand -group rx_buf -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_rx_buffer/U_Rx_Buffer/fab_to_encode
add wave -noupdate -expand -group {RX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(8)
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/dreq_pipe(8)
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Gen_Status/mbuf_valid_i
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Gen_Status/mbuf_drop_i
add wave -noupdate -expand -group {RX PATH} /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Gen_Status/mbuf_is_pause_i
add wave -noupdate -expand -group {RX PATH} -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_Gen_Status/state
add wave -noupdate -expand -group {RX PATH} -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/fab_pipe(9)
add wave -noupdate /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_RX_Wishbone_Master/snk_dreq_o
add wave -noupdate /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_RX_Wishbone_Master/sof_reg
add wave -noupdate -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_RX_Wishbone_Master/state
add wave -noupdate -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_RX_Wishbone_Master/src_wb_o
add wave -noupdate -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/U_RX_Wishbone_Master/src_wb_i
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/clk_sys_i
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/clk_rx_i
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/regs_i.pfcr0_enable_o
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/pfilter_drop
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/mbuf_we
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/mbuf_pf_drop
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/mbuf_valid
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/mbuf_full
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/mbuf_rd
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/do_write
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/pointer
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/pointer_zero
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/pointer_full
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/empty
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/valid_count
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/we_i
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/rd_i
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/gen_sregs(2)/U_SRLx/gen_srl32/U_SRLC32/A
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/gen_sregs(2)/U_SRLx/gen_srl32/U_SRLC32/D
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/gen_sregs(2)/U_SRLx/gen_srl32/U_SRLC32/Q
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/gen_sregs(2)/U_SRLx/gen_srl32/U_SRLC32/CLK
add wave -noupdate -expand -group MBUF /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/gen_sregs(2)/U_SRLx/gen_srl32/U_SRLC32/CE
add wave -noupdate -expand -group MBUF -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Rx_Path/gen_with_match_buff/U_match_buffer/gen_sregs(2)/U_SRLx/gen_srl32/U_SRLC32/SHIFT_REG
add wave -noupdate -group FMux -expand /main/DUT/U_WBP_Mux/ep_src_o
add wave -noupdate -group FMux /main/DUT/U_WBP_Mux/ep_src_i
add wave -noupdate -group FMux /main/DUT/U_WBP_Mux/ep_snk_o
add wave -noupdate -group FMux -expand /main/DUT/U_WBP_Mux/ep_snk_i
add wave -noupdate -group FMux /main/DUT/U_WBP_Mux/mux_src_o
add wave -noupdate -group FMux /main/DUT/U_WBP_Mux/mux_src_i
add wave -noupdate -group FMux /main/DUT/U_WBP_Mux/mux_snk_o
add wave -noupdate -group FMux -expand -subitemconfig {/main/DUT/U_WBP_Mux/mux_snk_i(1) -expand /main/DUT/U_WBP_Mux/mux_snk_i(0) -expand} /main/DUT/U_WBP_Mux/mux_snk_i
add wave -noupdate -group FMux /main/DUT/U_WBP_Mux/mux_class_i
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {9719712580 fs} 0}
configure wave -namecolwidth 209
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
WaveRestoreZoom {0 fs} {67940302500 fs}
