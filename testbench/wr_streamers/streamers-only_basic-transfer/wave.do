onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/U_TX_Streamer/clk_sys_i
add wave -noupdate /main/U_TX_Streamer/rst_n_i
add wave -noupdate -divider {TX streamer - User side}
add wave -noupdate /main/U_TX_Streamer/tx_data_i
add wave -noupdate /main/U_TX_Streamer/tx_reset_seq_i
add wave -noupdate /main/U_TX_Streamer/tx_valid_i
add wave -noupdate /main/U_TX_Streamer/tx_dreq_o
add wave -noupdate /main/U_TX_Streamer/tx_last_p1_i
add wave -noupdate /main/U_TX_Streamer/tx_flush_p1_i
add wave -noupdate -divider Wishbone
add wave -noupdate /main/mac/g_data_width
add wave -noupdate /main/mac/g_addr_width
add wave -noupdate /main/mac/adr
add wave -noupdate /main/mac/dat_o
add wave -noupdate /main/mac/dat_i
add wave -noupdate /main/mac/sel
add wave -noupdate /main/mac/ack
add wave -noupdate /main/mac/stall
add wave -noupdate /main/mac/err
add wave -noupdate /main/mac/rty
add wave -noupdate /main/mac/cyc
add wave -noupdate /main/mac/stb
add wave -noupdate /main/mac/we
add wave -noupdate -divider {RX streamer - user side}
add wave -noupdate /main/U_RX_Streamer/rx_data_o
add wave -noupdate /main/U_RX_Streamer/rx_valid_o
add wave -noupdate /main/U_RX_Streamer/rx_dreq_i
add wave -noupdate /main/U_RX_Streamer/rx_latency_o
add wave -noupdate /main/U_RX_Streamer/rx_latency_valid_o
add wave -noupdate /main/U_RX_Streamer/rx_first_p1_o
add wave -noupdate /main/U_RX_Streamer/rx_last_p1_o
add wave -noupdate /main/U_RX_Streamer/rx_lost_p1_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {1370 ns} 0}
configure wave -namecolwidth 150
configure wave -valuecolwidth 152
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
WaveRestoreZoom {0 ns} {10500 ns}
