onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/rst_n_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/clk_sys_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_fab_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_error_o
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_busy_o
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/pcs_dreq_o
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/mdio_mcr_pdown_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/mdio_wr_spec_tx_cal_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/an_tx_en_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/an_tx_val_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/timestamp_trigger_p_a_o
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/rmon_o
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_clk_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_data_o
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_k_o
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_disparity_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/phy_tx_enc_err_i
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_is_k
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_catch_disparity
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_odata_reg
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_state
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_cntr
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_cr_alternate
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_packed_in
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_packed_out
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_empty
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_almost_empty
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_almost_full
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_enough_data
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_wr
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_rd
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_ready
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_clear_n
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/fifo_fab
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_rdreq_toggle
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_odd_length
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_busy
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/tx_error
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/reset_synced_txclk
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/mdio_mcr_pdown_synced
add wave -noupdate -expand -group tx-pcs /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/s_one
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_data_width
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_size
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_show_ahead
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_rd_empty
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_rd_full
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_rd_almost_empty
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_rd_almost_full
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_rd_count
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_wr_empty
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_wr_full
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_wr_almost_empty
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_wr_almost_full
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_with_wr_count
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_almost_empty_threshold
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/g_almost_full_threshold
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/rst_n_i
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/clk_wr_i
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/d_i
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/we_i
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/wr_empty_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/wr_full_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/wr_almost_empty_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/wr_almost_full_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/wr_count_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/clk_rd_i
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/q_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/rd_i
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/rd_empty_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/rd_full_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/rd_almost_empty_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/rd_almost_full_o
add wave -noupdate -expand -group tx-fifo /main/U_Wrapped_EP/DUT/U_PCS_1000BASEX/gen_8bit/U_TX_PCS/U_TX_FIFO/rd_count_o
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/clk_sys_i
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/rst_n_i
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/snk_fab_i
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/snk_dreq_o
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/src_fab_o
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/src_dreq_i
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/state
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/crc_gen_reset
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/crc_gen_enable
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/crc_value
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/odd_length
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/embed_valid
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/embed_eof
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/stored_msb
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/in_payload
add wave -noupdate -expand -group crc-insert /main/U_Wrapped_EP/DUT/U_Tx_Path/U_Insert_CRC/src_dreq_d0
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/clk_sys_i
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/rst_n_i
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/snk_fab_i
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/snk_dreq_o
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/src_fab_o
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/src_dreq_i
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/inject_mem_addr_i
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/inject_mem_data_o
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/regs_i
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/state
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/counter
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/vut_rd_vid
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/vut_wr_vid
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/vut_untag
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/vut_stored_tag
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/vut_stored_ethertype
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/mem_addr_muxed
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/mem_rdata
add wave -noupdate -group tx-vlan /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_vlan_unit/U_VLAN_Unit/src_dreq_d0
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/clk_sys_i
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/rst_n_i
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/snk_fab_i
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/snk_dreq_o
add wave -noupdate -expand -group inect -expand /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/src_fab_o
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/src_dreq_i
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/inject_req_i
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/inject_ready_o
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/inject_packet_sel_i
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/inject_user_value_i
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/mem_addr_o
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/mem_data_i
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/state
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/counter
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/within_packet
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/select_inject
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/inj_src
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/template_user
add wave -noupdate -expand -group inect /main/U_Wrapped_EP/DUT/U_Tx_Path/gen_with_injection/U_Injector/template_last
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {189924568970 fs} 0}
configure wave -namecolwidth 413
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
WaveRestoreZoom {178205818970 fs} {211018318970 fs}
