onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -position end  sim:/main/current_test
add wave -noupdate /main/U_TX_Streamer/tx_valid_i
add wave -noupdate /main/U_TX_Streamer/tx_data_i
add wave -noupdate /main/U_TX_Streamer/tx_last_p1_i
add wave -noupdate /main/U_TX_Streamer/tx_flush_p1_i
add wave -position end  sim:/main/U_TX_Streamer/U_Wrapped_Streamer/fab_src.sof
add wave -position end  sim:/main/U_TX_Streamer/U_Wrapped_Streamer/fab_src.eof
add wave -noupdate /main/U_TX_Streamer/tx_frame_p1_o
add wave -noupdate /main/U_TX_Streamer/tx_dreq_o
#add wave -noupdate /main/U_TX_Streamer/tx_reset_seq_i
add wave -noupdate /main/U_RX_Streamer/rx_frame_p1_o
add wave -noupdate /main/U_RX_Streamer/rx_dreq_i
add wave -noupdate /main/U_RX_Streamer/rx_valid_o
add wave -noupdate /main/U_RX_Streamer/rx_data_o
add wave -noupdate /main/U_RX_Streamer/rx_first_p1_o
add wave -noupdate /main/U_RX_Streamer/rx_last_p1_o
add wave -noupdate /main/U_RX_Streamer/rx_lost_p1_o
add wave -position end  sim:/main/drop_frm
add wave -position end  sim:/main/rx_streamer_lost_frm
add wave -position end  sim:/main/rx_streamer_lost_frm_cnt
add wave -noupdate /main/U_RX_Streamer/rx_latency_o
add wave -noupdate /main/U_RX_Streamer/rx_latency_valid_o
add wave -position end  sim:/main/rx_streamer_lost_blks
add wave -position end  sim:/main/fab_data_from_tx
add wave -position end  sim:/main/fab_data_to_rx
add wave -noupdate /main/mac/adr
#add wave -noupdate /main/mac/dat_o
#add wave -noupdate /main/mac/dat_i
#add wave -noupdate /main/mac/sel
#add wave -noupdate /main/mac/ack
#add wave -noupdate /main/mac/stall
add wave -noupdate /main/mac/err
add wave -noupdate /main/mac/rty
#add wave -noupdate /main/mac/cyc
#add wave -noupdate /main/mac/stb
#add wave -noupdate /main/mac/we
add wave -position end  sim:/main/delay_link
add wave -position end  sim:/main/tx_wb_cyc
add wave -position end  sim:/main/rx_wb_cyc
add wave -position end  sim:/main/tx_wb_ack
add wave -position end  sim:/main/rx_wb_ack
add wave -position end  sim:/main/rx_wb_stall
add wave -position end  sim:/main/tx_wb_stall
add wave -position end  sim:/main/tx_wb_stb
add wave -position end  sim:/main/rx_wb_stb
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {867 ns} 0}
configure wave -namecolwidth 150
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
WaveRestoreZoom {0 ns} {915 ns}
