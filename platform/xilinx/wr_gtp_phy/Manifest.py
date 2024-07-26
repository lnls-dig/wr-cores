files = [
        "gtp_bitslide.vhd",
        ];


xilinx_ip_gthe3 = [
    "xilinx-ip/gthe3/gtwizard_ultrascale_v1_6_gthe3_channel.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_gthe3_channel_wrapper.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_example_wrapper.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_example_bit_sync.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_example_wrapper_functions.v",
    "xilinx-ip/gthe3/wr_gth_wrapper.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_gtwizard_top.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_example_top.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_gtwizard_gthe3.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_example_reset_sync.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_example_gtwiz_userclk_tx.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_example_init.v",
    "xilinx-ip/gthe3/wr_gth_wrapper_example_gtwiz_userclk_rx.v"
];

xilinx_ip_gthe4 = [
    "xilinx-ip/gthe4/gtwizard_ultrascale_2.v",
    "xilinx-ip/gthe4/gtwizard_ultrascale_2_gtwizard_top.v",
    "xilinx-ip/gthe4/gtwizard_ultrascale_2.xdc",
    "xilinx-ip/gthe4/gtwizard_ultrascale_2_gthe4_channel_wrapper.v",
    "xilinx-ip/gthe4/gtwizard_ultrascale_2_gtwizard_gthe4.v",
    "xilinx-ip/gthe4/gtwizard_ultrascale_2_ooc.xdc",
    "xilinx-ip/gthe4/gtwizard_ultrascale_v1_7_gthe4_channel.v"
];

# Common files between gthe3 and gthe4
xilinx_ip_common = [
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_bit_sync.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gthe4_cpll_cal_tx.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gthe4_cpll_cal_rx.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gtwiz_reset.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gtwiz_userdata_rx.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gte4_drp_arb.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_reset_inv_sync.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gtwiz_buffbypass_tx.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gtwiz_userclk_tx.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gthe3_cpll_cal.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gthe4_cal_freqcnt.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gthe4_delay_powergood.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gthe4_cpll_cal.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gtwiz_userclk_rx.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_reset_sync.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gtwiz_userdata_tx.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gtwiz_buffbypass_rx.v",
    "xilinx-ip/common/gtwizard_ultrascale_v1_7_gthe3_cal_freqcnt.v"
];

xilinx_ip_gthe4_lp = [
    "xilinx-ip/gthe4_lp/gtwizard_ultrascale_2_gtwizard_top.v",
    "xilinx-ip/gthe4_lp/gtwizard_ultrascale_2.xdc",
    "xilinx-ip/gthe4_lp/gtwizard_ultrascale_2_gtwizard_gthe4.v",
    "xilinx-ip/gthe4_lp/gtwizard_ultrascale_v1_7_gthe4_channel.v"
];

xilinx_ip_gthe4_lp_125 = [
    "xilinx-ip/gthe4_lp/phy_ref_clk_125/gtwizard_ultrascale_2.v",
    "xilinx-ip/gthe4_lp/phy_ref_clk_125/gtwizard_ultrascale_2_gthe4_channel_wrapper.v",
    "xilinx-ip/gthe4_lp/phy_ref_clk_125/gtwizard_ultrascale_2_ooc.xdc",
];

xilinx_ip_gthe4_lp_100 = [
    "xilinx-ip/gthe4_lp/phy_ref_clk_100/gtwizard_ultrascale_2.v",
    "xilinx-ip/gthe4_lp/phy_ref_clk_100/gtwizard_ultrascale_2_gthe4_channel_wrapper.v",
    "xilinx-ip/gthe4_lp/phy_ref_clk_100/gtwizard_ultrascale_2_ooc.xdc",
];

xilinx_ip_gthe4_common_lp = [
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_bit_sync.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gte4_drp_arb.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gthe3_cal_freqcnt.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gthe3_cpll_cal.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gthe4_cal_freqcnt.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gthe4_cpll_cal.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gthe4_cpll_cal_rx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gthe4_cpll_cal_tx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gthe4_delay_powergood.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtwiz_buffbypass_rx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtwiz_buffbypass_tx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtwiz_reset.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtwiz_userclk_rx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtwiz_userclk_tx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtwiz_userdata_rx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtwiz_userdata_tx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtye4_cal_freqcnt.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtye4_cpll_cal.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtye4_cpll_cal_rx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtye4_cpll_cal_tx.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_gtye4_delay_powergood.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_reset_inv_sync.v",
    "xilinx-ip/gthe4_lp/common/gtwizard_ultrascale_v1_7_reset_sync.v",
];

if globals().get('wrcore_platform') is False:
    # No platform (avoid inclusion of xdc files)
    pass
elif (syn_device[0:4].upper()=="XC6S"): # Spartan6
    files.extend(["spartan6/wr_gtp_phy_spartan6.vhd",
        "spartan6/whiterabbitgtp_wrapper_tile_spartan6.vhd",
        "spartan6/gtp_phase_align.vhd"])
elif (syn_device[0:4].upper()=="XC6V"): # Virtex6
    files.extend(["virtex6/wr_gtx_phy_virtex6.vhd",
        "virtex6/whiterabbitgtx_wrapper_gtx.vhd",
        "virtex6/gtp_phase_align_virtex6.vhd",
        "virtex6/gtx_reset.vhd",
        "common/lpdc_mdio_regs.vhd",
        "virtex6-low-phase-drift/gtx_comma_detect_lp.vhd",
        "virtex6-low-phase-drift/gtx_tx_reset_lp.vhd",
        "virtex6-low-phase-drift/whiterabbitgtx_wrapper_gtx_lp.vhd",
        "virtex6-low-phase-drift/wr_gtx_phy_virtex6_lp.vhd"])

elif (syn_device[0:4].upper()=="XC5V"): # Virtex5
    files.extend(["virtex5/wr_gtp_phy_virtex5.vhd",
        "virtex5/whiterabbit_gtp_wrapper_tile_virtex5.vhd",
        "spartan6/gtp_phase_align.vhd",
        "virtex5/v5_gtp_align_detect.vhd",
        "virtex5/v5_gtp_comma_detect.vhd"
    ])

elif (syn_device[0:4].upper()=="XC7A" or # Family 7 GTP (Artix7)
      syn_device.upper()=="XC7Z015"):
    files.extend([
        "family7-gtp/wr_gtp_phy_family7.vhd",
        "family7-gtp/whiterabbit_gtpe2_channel_wrapper.vhd",
        "family7-gtp/whiterabbit_gtpe2_channel_wrapper_gt.vhd",
        "family7-gtp/whiterabbit_gtpe2_channel_wrapper_gtrxreset_seq.vhd" ]);
elif (syn_device[0:4].upper()=="XC7K" or # Family 7 GTX (Kintex7 and Virtex7 585, 2000, X485 and ZYNQ Z030,Z035 Z045)
        syn_device[0:7].upper()=="XC7V585" or
        syn_device[0:8].upper()=="XC7V2000" or
        syn_device[0:8].upper()=="XC7VX485" or
        syn_device[0:6].upper()=="XC7Z03" or
        syn_device[0:7].upper()=="XC7Z045"):
    files.extend([
        "family7-gtx/wr_gtx_phy_family7.vhd",
        "family7-gtx/whiterabbit_gtxe2_channel_wrapper_gt.vhd",
        "common/lpdc_mdio_regs.vhd",
        "kintex7-lp/whiterabbit_gtxe2_channel_wrapper_kintex7_lp.vhd",
        "kintex7-lp/wr_gtx_phy_kintex7_lp.vhd",
        "kintex7-lp/wr_gtx_phy_kintex7_lp_qpll.vhd",
        "kintex7-lp/gtx_comma_detect_kintex7_lp.vhd",
        "kintex7-lp/gtx_idle_detect_kintex7_lp.vhd",
        "family7-gtx-lp/whiterabbit_gtxe2_channel_wrapper_gt_lp.vhd",
        "family7-gtx-lp/gtx_comma_detect_lp.vhd",
        "family7-gtx-lp/wr_gtx_phy_family7_lp.vhd" ]);
elif (syn_device[0:4].upper()=="XC7V"): # Family 7 GTH (other Virtex7 devices)
    files.extend(["family7-gth/wr_gth_phy_family7.vhd",
        "whiterabbit_gthe2_channel_wrapper_gt.vhd",
        "whiterabbit_gthe2_channel_wrapper_gtrxreset_seq.vhd",
        "whiterabbit_gthe2_channel_wrapper_sync_block.vhd" ]);
elif (syn_device[0:4].upper()=="XCKU"): # Kintex Ultrascale GTH
    files.extend(["family7-gthe3/wr_gthe3_phy_family7.vhd",
        "family7-gthe3/wr_gthe3_phy_family7_xilinx_ip.vhd",
        "family7-gthe3/wr_gthe3_reset.vhd",
        "family7-gthe3/wr_gthe3_rx_buffer_bypass.vhd",
        "family7-gthe3/wr_gthe3_tx_buffer_bypass.vhd",
        "family7-gthe3/wr_gthe3_wrapper.vhd",
        "family7-gthe3/gc_reset_synchronizer.vhd" ])
    files.extend( xilinx_ip_gthe3 );
    files.extend( xilinx_ip_common );
elif (syn_device[0:4].upper()=="XCZU"): # Zynq Ultrascale GTH
    files.extend([
        "family7-gthe4/wr_gthe4_phy_family7_xilinx_ip.vhd",
        ]);
    files.extend( xilinx_ip_gthe4 );
    files.extend( xilinx_ip_common );
elif (syn_device[0:6].upper()=="XCAU10" or # Artix Ultrascale+ AU10P AU15P GTH
      syn_device[0:6].upper()=="XCAU15"):  # use Low Phase Drift implementation
    files.extend([
        "family7-gthe4-lp/wr_gthe4_phy_family7_lp.vhd",
        "family7-gtx-lp/gtx_comma_detect_lp.vhd",
        "common/lpdc_mdio_regs.vhd",
        ]);
    files.extend( xilinx_ip_gthe4_lp );             # Note that gthe4 depend on Vivado version
    files.extend( xilinx_ip_gthe4_common_lp );      # and instantiate its specific common files 
    # PHY reference clock defaults to 125 MHz; check if 100 MHz is defined
    try:
        if (phy_ref_clk=="100"):
            files.extend( xilinx_ip_gthe4_lp_100 );
        else:
            files.extend( xilinx_ip_gthe4_lp_125 ); # if phy_clk_ref exists but is other than "100"
    except:
        files.extend( xilinx_ip_gthe4_lp_125 );     # if phy_clk_ref does not exists
