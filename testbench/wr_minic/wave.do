onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/txp_cg_newframe
add wave -noupdate /main/rxp_cg_newframe
add wave -noupdate /main/txp_cg_size
add wave -noupdate /main/rxp_cg_size
add wave -noupdate /main/DUT/g_interface_mode
add wave -noupdate /main/DUT/g_address_granularity
add wave -noupdate /main/DUT/g_buffer_little_endian
add wave -noupdate /main/DUT/clk_sys_i
add wave -noupdate /main/DUT/rst_n_i
add wave -noupdate -group SRC /main/DUT/src_dat_o
add wave -noupdate -group SRC /main/DUT/src_adr_o
add wave -noupdate -group SRC /main/DUT/src_sel_o
add wave -noupdate -group SRC /main/DUT/src_cyc_o
add wave -noupdate -group SRC /main/DUT/src_stb_o
add wave -noupdate -group SRC /main/DUT/src_we_o
add wave -noupdate -group SRC /main/DUT/src_stall_i
add wave -noupdate -group SRC /main/DUT/src_err_i
add wave -noupdate -group SRC /main/DUT/src_ack_i
add wave -noupdate -expand -group SNK /main/DUT/snk_dat_i
add wave -noupdate -expand -group SNK /main/DUT/snk_adr_i
add wave -noupdate -expand -group SNK /main/DUT/snk_sel_i
add wave -noupdate -expand -group SNK /main/DUT/snk_cyc_i
add wave -noupdate -expand -group SNK /main/DUT/snk_stb_i
add wave -noupdate -expand -group SNK /main/DUT/snk_we_i
add wave -noupdate -expand -group SNK /main/DUT/snk_stall_o
add wave -noupdate -expand -group SNK /main/DUT/snk_err_o
add wave -noupdate -expand -group SNK /main/DUT/snk_ack_o
add wave -noupdate -group TXTSU /main/DUT/txtsu_port_id_i
add wave -noupdate -group TXTSU /main/DUT/txtsu_frame_id_i
add wave -noupdate -group TXTSU /main/DUT/txtsu_tsval_i
add wave -noupdate -group TXTSU /main/DUT/txtsu_ack_o
add wave -noupdate -group WB -expand /main/DUT/wb_out
add wave -noupdate -group WB -expand /main/DUT/wb_in
add wave -noupdate -expand -group TX_PATH -height 16 /main/DUT/ntx_state
add wave -noupdate -expand -group TX_PATH -radix unsigned /main/DUT/ntx_ack_count
add wave -noupdate -expand -group TX_PATH /main/DUT/ntx_flush_last
add wave -noupdate -expand -group TX_PATH /main/DUT/tx_fifo_d
add wave -noupdate -expand -group TX_PATH /main/DUT/tx_fifo_q
add wave -noupdate -expand -group TX_PATH /main/DUT/tx_fifo_we
add wave -noupdate -expand -group TX_PATH /main/DUT/tx_fifo_rd
add wave -noupdate -expand -group TX_PATH /main/DUT/tx_fifo_empty
add wave -noupdate -expand -group TX_PATH /main/DUT/tx_fifo_full
add wave -noupdate -expand -group TX_PATH /main/DUT/txf_ferror
add wave -noupdate -expand -group TX_PATH /main/DUT/txf_fnew
add wave -noupdate -expand -group TX_PATH /main/DUT/txf_data
add wave -noupdate -expand -group TX_PATH /main/DUT/txf_type
add wave -noupdate -expand -group TX_PATH /main/DUT/ntx_stored_dat
add wave -noupdate -expand -group TX_PATH /main/DUT/ntx_stored_type
add wave -noupdate -expand -group TX_PATH /main/DUT/irq_tx
add wave -noupdate -expand -group TX_PATH /main/DUT/irq_tx_ack
add wave -noupdate -expand -group TX_PATH /main/DUT/irq_tx_mask
add wave -noupdate -expand -group TX_PATH /main/DUT/ntx_newpacket
add wave -noupdate -expand -group RX_PATH /main/DUT/regs_out
add wave -noupdate -expand -group RX_PATH -radix unsigned /main/DUT/RX_FIFO/count_o
add wave -noupdate -expand -group RX_PATH -height 16 /main/DUT/nrx_state
add wave -noupdate -expand -group RX_PATH /main/DUT/nrx_sof
add wave -noupdate -expand -group RX_PATH /main/DUT/nrx_eof
add wave -noupdate -expand -group RX_PATH /main/DUT/rxf_data
add wave -noupdate -expand -group RX_PATH /main/DUT/rxf_type
add wave -noupdate -expand -group RX_PATH /main/DUT/rx_fifo_we
add wave -noupdate -expand -group RX_PATH /main/DUT/rx_fifo_q
add wave -noupdate -expand -group RX_PATH /main/DUT/rx_fifo_rd
add wave -noupdate -expand -group RX_PATH /main/DUT/rx_fifo_empty
add wave -noupdate -expand -group RX_PATH /main/DUT/rx_fifo_full
add wave -noupdate -expand -group RX_PATH /main/DUT/rx_fifo_afull
add wave -noupdate -expand -group RX_PATH /main/DUT/irq_rx_ack
add wave -noupdate -expand -group RX_PATH /main/DUT/irq_rx
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {40485000000 fs} 1} {{Cursor 2} {7845093670430 fs} 0}
configure wave -namecolwidth 208
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
WaveRestoreZoom {0 fs} {15051471750 ps}
