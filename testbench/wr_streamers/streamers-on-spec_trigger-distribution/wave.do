onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/link_up_a
add wave -noupdate /main/link_up_b
add wave -noupdate -divider {SPEC A-common}
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_Pulse_Stamper/clk_ref_i
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_Pulse_Stamper/clk_sys_i
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_Pulse_Stamper/rst_n_i
add wave -noupdate -divider {SPEC A - WR timing}
add wave -noupdate /main/SPEC_A/dio_p_i(1)
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_Pulse_Stamper/tm_time_valid_i
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_Pulse_Stamper/tm_tai_i
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_Pulse_Stamper/tm_cycles_i
add wave -noupdate -divider {SPEC A - pulse stamper}
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_Pulse_Stamper/tag_tai_o
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_Pulse_Stamper/tag_cycles_o
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_Pulse_Stamper/tag_valid_o
add wave -noupdate -divider {SPEC A - TX Streamer}
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_TX_Streamer/tx_data_i
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_TX_Streamer/tx_valid_i
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_TX_Streamer/tx_dreq_o
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_TX_Streamer/src_i
add wave -noupdate -radix hexadecimal /main/SPEC_A/U_TX_Streamer/src_o
add wave -noupdate -divider {SPEC A - PHY}
add wave -noupdate -radix hexadecimal /main/SPEC_A/phy_tx_data
add wave -noupdate -radix hexadecimal /main/SPEC_A/phy_tx_k
add wave -noupdate -radix hexadecimal /main/SPEC_A/phy_tx_disparity
add wave -noupdate -radix hexadecimal /main/SPEC_A/phy_tx_enc_err
add wave -noupdate -divider {SPEC B - PHY}
add wave -noupdate -radix hexadecimal /main/SPEC_B/phy_rx_data
add wave -noupdate -radix hexadecimal /main/SPEC_B/phy_rx_rbclk
add wave -noupdate -radix hexadecimal /main/SPEC_B/phy_rx_k
add wave -noupdate -radix hexadecimal /main/SPEC_B/phy_rx_enc_err
add wave -noupdate -divider {SPEC B - WR timing}
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Pulse_Stamper/tm_time_valid_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Pulse_Stamper/tm_tai_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Pulse_Stamper/tm_cycles_i
add wave -noupdate -divider {SPEC B - RX streamer}
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_RX_Streamer/snk_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_RX_Streamer/snk_o
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_RX_Streamer/rx_data_o
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_RX_Streamer/rx_valid_o
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_RX_Streamer/rx_dreq_i
add wave -noupdate -divider {SPEC B - Timestamp adder}
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Add_Delay1/valid_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Add_Delay1/a_tai_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Add_Delay1/a_cycles_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Add_Delay1/b_tai_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Add_Delay1/b_cycles_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Add_Delay1/q_tai_o
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Add_Delay1/q_cycles_o
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Add_Delay1/valid_o
add wave -noupdate -divider {SPEC B - pulse generator}
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Pulse_Generator/trig_tai_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Pulse_Generator/trig_cycles_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Pulse_Generator/trig_valid_i
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Pulse_Generator/trig_ready_o
add wave -noupdate -radix hexadecimal /main/SPEC_B/U_Pulse_Generator/pulse_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {593239379560 fs} 1} {{Cursor 2} {538394383110 fs} 0}
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
WaveRestoreZoom {0 fs} {839137496540 fs}
