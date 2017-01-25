onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/DUT/PERIPH/rst_n_i
add wave -noupdate /main/DUT/rst_net_n
add wave -noupdate /main/DUT/rst_wrc_n
add wave -noupdate /main/DUT/clk_sys_i
add wave -noupdate /main/clk_ref
add wave -noupdate /main/DUT/phy_tx_data_o
add wave -noupdate /main/DUT/phy_tx_k_o
add wave -noupdate /main/phy_rbclk
add wave -noupdate /main/DUT/phy_rx_data_i
add wave -noupdate /main/DUT/phy_rx_k_i
add wave -noupdate -expand -group WRPC_EB /main/DUT/U_Endpoint/src_o
add wave -noupdate -expand -group WRPC_EB /main/DUT/U_Endpoint/src_i
add wave -noupdate -expand -group WRPC_EB /main/DUT/U_Endpoint/snk_o
add wave -noupdate -expand -group WRPC_EB /main/DUT/U_Endpoint/snk_i
add wave -noupdate -expand -group WRPC_EB /main/DUT/U_Endpoint/wb_i
add wave -noupdate -expand -group WRPC_EB /main/DUT/U_Endpoint/wb_o
add wave -noupdate -expand -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_clk_i
add wave -noupdate -expand -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_data_o
add wave -noupdate -expand -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_k_o
add wave -noupdate -expand -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_disparity_i
add wave -noupdate -expand -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_enc_err_i
add wave -noupdate -expand -group EP_PCS -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_state
add wave -noupdate -expand -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_fab_i
add wave -noupdate -expand -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_busy_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 2} {49772000000 fs} 0} {{Cursor 2} {78532322210 fs} 0}
configure wave -namecolwidth 270
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
WaveRestoreZoom {0 fs} {3592204717500 fs}
