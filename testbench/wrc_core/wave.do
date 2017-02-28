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
add wave -noupdate -expand -group WRPC_EP -expand /main/DUT/U_Endpoint/src_o
add wave -noupdate -expand -group WRPC_EP /main/DUT/U_Endpoint/src_i
add wave -noupdate -expand -group WRPC_EP /main/DUT/U_Endpoint/snk_o
add wave -noupdate -expand -group WRPC_EP -expand /main/DUT/U_Endpoint/snk_i
add wave -noupdate -expand -group WRPC_EP /main/DUT/U_Endpoint/U_Wrapped_Endpoint/regs_fromwb
add wave -noupdate -expand -group WRPC_EP /main/DUT/U_Endpoint/U_Wrapped_Endpoint/regs_towb
add wave -noupdate -expand -group WRPC_EP /main/DUT/U_Endpoint/U_Wrapped_Endpoint/pfilter_done_o
add wave -noupdate -expand -group WRPC_EP /main/DUT/U_Endpoint/U_Wrapped_Endpoint/pfilter_drop_o
add wave -noupdate -expand -group WRPC_EP /main/DUT/U_Endpoint/U_Wrapped_Endpoint/pfilter_pclass_o
add wave -noupdate -expand -group WRPC_EP /main/DUT/U_Endpoint/wb_i
add wave -noupdate -expand -group WRPC_EP /main/DUT/U_Endpoint/wb_o
add wave -noupdate -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_clk_i
add wave -noupdate -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_data_o
add wave -noupdate -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_k_o
add wave -noupdate -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_disparity_i
add wave -noupdate -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_enc_err_i
add wave -noupdate -group EP_PCS -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_state
add wave -noupdate -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_fab_i
add wave -noupdate -group EP_PCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_busy_o
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/src_i
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/src_o
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/wb_i
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/wb_o
add wave -noupdate -expand -group Minic -height 16 /main/DUT/MINI_NIC/U_Wrapped_Minic/ntx_state
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/tx_fifo_d
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/tx_fifo_empty
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/tx_fifo_full
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/tx_fifo_q
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/tx_fifo_rd
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/tx_fifo_we
add wave -noupdate -expand -group Minic /main/DUT/MINI_NIC/U_Wrapped_Minic/tx_status_word
add wave -noupdate -group EP /main/EP/snk_ack_o
add wave -noupdate -group EP /main/EP/snk_adr_i
add wave -noupdate -group EP /main/EP/snk_cyc_i
add wave -noupdate -group EP /main/EP/snk_dat_i
add wave -noupdate -group EP /main/EP/snk_err_o
add wave -noupdate -group EP /main/EP/snk_rty_o
add wave -noupdate -group EP /main/EP/snk_sel_i
add wave -noupdate -group EP /main/EP/snk_stall_o
add wave -noupdate -group EP /main/EP/snk_stb_i
add wave -noupdate -group EP /main/EP/snk_we_i
add wave -noupdate -group EP /main/EP/src_ack_i
add wave -noupdate -group EP /main/EP/src_adr_o
add wave -noupdate -group EP /main/EP/src_cyc_o
add wave -noupdate -group EP /main/EP/src_dat_o
add wave -noupdate -group EP /main/EP/src_err_i
add wave -noupdate -group EP /main/EP/src_in
add wave -noupdate -group EP /main/EP/src_out
add wave -noupdate -group EP /main/EP/src_sel_o
add wave -noupdate -group EP /main/EP/src_stall_i
add wave -noupdate -group EP /main/EP/src_stb_o
add wave -noupdate -group EP /main/EP/src_we_o
add wave -noupdate -group EP /main/EP/regs_fromwb
add wave -noupdate -group EP /main/EP/regs_towb
add wave -noupdate -group EP /main/EP/wb_ack_o
add wave -noupdate -group EP /main/EP/wb_adr_i
add wave -noupdate -group EP /main/EP/wb_cyc_i
add wave -noupdate -group EP /main/EP/wb_dat_i
add wave -noupdate -group EP /main/EP/wb_dat_o
add wave -noupdate -group EP /main/EP/wb_in
add wave -noupdate -group EP /main/EP/wb_out
add wave -noupdate -group EP /main/EP/wb_sel_i
add wave -noupdate -group EP /main/EP/wb_stall_o
add wave -noupdate -group EP /main/EP/wb_stb_i
add wave -noupdate -group EP /main/EP/wb_we_i
add wave -noupdate -group DUT->EP-TXpath -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/dreq_pipe
add wave -noupdate -group DUT->EP-TXpath /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/pcs_busy_i
add wave -noupdate -group DUT->EP-TXpath /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/snk_i
add wave -noupdate -group DUT->EP-TXpath /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/snk_o
add wave -noupdate -group DUT->EP-TXpath /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/pcs_fab_o
add wave -noupdate -group DUT->EP-TXpath /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/pcs_dreq_i
add wave -noupdate -group DUT->EP-TXpath /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/ep_ctrl_i
add wave -noupdate -group DUT->EP-TXpath /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/pcs_error_i
add wave -noupdate -group DUT->EP-TXpath -expand /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/fab_pipe
add wave -noupdate -group DUT->EP-TXpath /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_Tx_Path/pcs_fab_o
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/an_tx_en_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/an_tx_en_synced
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/an_tx_val_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/clk_sys_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_almost_empty
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_almost_full
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_clear_n
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_empty
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_enough_data
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_fab
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_packed_in
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_packed_out
add wave -noupdate -group DUT->EP-TxPCS -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_state
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_rd
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_ready
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_wr
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/mdio_mcr_pdown_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/mdio_mcr_pdown_synced
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/mdio_wr_spec_tx_cal_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_busy_o
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_dreq_o
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_error_o
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_fab_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_clk_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_data_o
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_disparity_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_enc_err_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_k_o
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/rmon_tx_underrun
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/rst_n_i
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/timestamp_trigger_p_a_o
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_busy
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_catch_disparity
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_cntr
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_cr_alternate
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_error
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_is_k
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_odata_reg
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_odd_length
add wave -noupdate -group DUT->EP-TxPCS -height 16 /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_state
add wave -noupdate -group DUT->EP-TxPCS /main/DUT/U_Endpoint/U_Wrapped_Endpoint/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_rdreq_toggle
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/an_tx_en_i
add wave -noupdate -group EP->txPCS -height 16 /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_state
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/an_tx_en_synced
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/an_tx_val_i
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/clk_sys_i
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_almost_empty
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_almost_full
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_clear_n
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_empty
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_enough_data
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_fab
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_packed_in
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_packed_out
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_rd
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_ready
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_wr
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/mdio_mcr_pdown_i
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/mdio_mcr_pdown_synced
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/mdio_wr_spec_tx_cal_i
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_busy_o
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_dreq_o
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_error_o
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_fab_i
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_clk_i
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_data_o
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_disparity_i
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_enc_err_i
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_k_o
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/rmon_tx_underrun
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/rst_n_i
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/timestamp_trigger_p_a_o
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_busy
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_catch_disparity
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_cntr
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_cr_alternate
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_error
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_is_k
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_odata_reg
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_odd_length
add wave -noupdate -group EP->txPCS /main/EP/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_rdreq_toggle
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/clk_sys_i
add wave -noupdate -expand -group DUT->MUX -height 16 /main/DUT/U_WBP_Mux/demux
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/dmux_others
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/dmux_sel
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/dmux_sel_zero
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/dmux_select
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/dmux_snd_stat
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/dmux_status_reg
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/ep_snk_i
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/ep_snk_o
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/ep_snk_out_stall
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/ep_src_i
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/ep_src_o
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/ep_stall_mask
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/g_muxed_ports
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/mux
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/mux_class_i
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/mux_cycs
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/mux_rrobin
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/mux_select
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/mux_snk_i
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/mux_snk_o
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/mux_src_i
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/mux_src_o
add wave -noupdate -expand -group DUT->MUX /main/DUT/U_WBP_Mux/rst_n_i
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 2} {402323834470 fs} 1} {{Cursor 2} {1203233669880 fs} 0} {{Cursor 3} {1419940330 fs} 0}
configure wave -namecolwidth 363
configure wave -valuecolwidth 163
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
WaveRestoreZoom {1202933155800 fs} {1203674570090 fs}
