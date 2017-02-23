# Quartus II: Tcl Preflow File for VFC-HD WR PTP core reference design

# Load Quartus II Tcl Project package
package require ::quartus::project

# Borrowed and adjusted from GSI bel-projects
proc qmegawiz {files} {
  set dir [file dirname [info script]]
  post_message "Testing for megawizard regeneration in $dir:$files"

  set device  [ get_global_assignment -name DEVICE ]
  set family  [ get_global_assignment -name FAMILY ]

  foreach i $files {
    if {![file exists "$dir/$i.qip"] || [file mtime "$dir/$i.txt"] > [file mtime "$dir/$i.qip"]} {
      post_message -type info "Regenerating $i using qmegawiz"
      file delete "$dir/$i.qip"
      file copy -force "$dir/$i.txt" "$dir/$i.vhd"

      set sf [open "| qmegawiz -silent \"-defaultfamily:$family\" \"-defaultdevice:$device\" \"$dir/$i.vhd\" " "r"]
      while {[gets $sf line] >= 0} { post_message -type info "$line" }
      if {[catch {close $sf} err]} {
	post_message -type error "Executing qmegawiz: $err"
	exit 1
      }
      if {![file exists "$dir/$i.qip"]} {
	post_message -type error "Executing qmegawiz: did not create $dir/$i.qip!"
	exit 1
      }

      file mtime "$dir/$i.qip" [file mtime "$dir/$i.vhd"]
    }
      set_global_assignment -name QIP_FILE "$dir/$i.qip"
  }
}

# procedure to delete an existing global assignment
proc del_global_assignment {name value} {
    set_global_assignment -name $name -remove $value
}

# procedure to check if signal is input
proc is_input {signal} {
    return [regexp -nocase {.+_i(\[[0123456789]+\])?$} $signal]
}

# simple procedure to take care of location assignments
proc loc {pin signal {iostandard "default"} {pullup "off"}} {
    set_location_assignment "PIN_${pin}" -to $signal
    if {![string equal $iostandard "default"]} {
	set_instance_assignment -name IO_STANDARD $iostandard -to $signal
    } else {
	set_instance_assignment -name IO_STANDARD "2.5 V" -to $signal
	set_instance_assignment -name CURRENT_STRENGTH_NEW 12MA -to $signal
	if {![is_input $signal]} {
	    set_instance_assignment -name SLEW_RATE 1 -to $signal
	}
    }
    if {![string equal $pullup "off"]} {
	set_instance_assignment -name WEAK_PULL_UP_RESISTOR ON -to $signal
    }
}

# SCRIPT EXECUTION STARTS HERE
post_message "Executing WR VFC-HD reference design pre-flow script"

set project_name "vfchd_wr_ref"

set make_assignments 0

# Make sure that the right project is open
if {[is_project_open]} {
    if {[string compare $quartus(project) $project_name]} {
	project_close
	project_open $project_name
    }
} else {
    project_open $project_name
}

# Remove unused megafunctions to reduce number of warnings
del_global_assignment QIP_FILE "../../ip_cores/general-cores/platform/altera/networks/arria5_networks.qip"
del_global_assignment QIP_FILE "../../ip_cores/general-cores/platform/altera/wb_pcie/arria5_pcie.qip"

# Regenerate required megafunctions if necessary
source ../../platform/altera/wr_arria5_phy/wr_arria5_phy.tcl
source ../../platform/altera/wr_arria5_pll_default/wr_arria5_pll_default.tcl

# Global assignments
set_global_assignment -name OPTIMIZE_IOC_REGISTER_PLACEMENT_FOR_TIMING "PACK ALL IO REGISTERS"
set_global_assignment -name IO_PLACEMENT_OPTIMIZATION ON
set_global_assignment -name SYNCHRONIZER_IDENTIFICATION "FORCED IF ASYNCHRONOUS"

# I/O configuration
loc AW25 areset_n_i
loc AF8  clk_board_125m_i "LVDS"
loc AD20 clk_board_20m_i

loc AF25 dac_dmtd_sync_n_o
loc AC24 dac_ref_sync_n_o
loc AH26 dac_sclk_o
loc AG26 dac_din_o

loc AE1  sfp_rx_i "1.5-V PCML"
loc AD3  sfp_tx_o "1.5-V PCML"

loc AN25 i2c_mux_sda_b
loc AM25 i2c_mux_scl_b

loc AT26 io_exp_irq_bsteth_n_i "default" "on"
loc AK25 io_exp_irq_los_n_i    "default" "on"

loc AD26 eeprom_sda_b
loc AH25 eeprom_scl_b

loc AV27 onewire_b

loc AW28 vme_write_n_i
loc AW30 vme_lword_n_b
loc AK21 vme_iackout_n_o
loc AM21 vme_iackin_n_i
loc AN21 vme_iack_n_i
loc AL22 vme_dtack_oe_o
loc AP22 vme_ds_n_i[0]
loc AN22 vme_ds_n_i[1]
loc AW20 vme_data_oe_n_o
loc AW19 vme_data_dir_o
loc AE29 vme_as_n_i
loc AK27 vme_addr_oe_n_o
loc AJ27 vme_addr_dir_o
loc AK22 vme_irq_n_o[1]
loc AT21 vme_irq_n_o[2]
loc AR21 vme_irq_n_o[3]
loc AH22 vme_irq_n_o[4]
loc AG22 vme_irq_n_o[5]
loc AU20 vme_irq_n_o[6]
loc AT20 vme_irq_n_o[7]
loc AD24 vme_data_b[0]
loc AD23 vme_data_b[1]
loc AU24 vme_data_b[2]
loc AT24 vme_data_b[3]
loc AL24 vme_data_b[4]
loc AK24 vme_data_b[5]
loc AF24 vme_data_b[6]
loc AE24 vme_data_b[7]
loc AH24 vme_data_b[8]
loc AG24 vme_data_b[9]
loc AW24 vme_data_b[10]
loc AW23 vme_data_b[11]
loc AP24 vme_data_b[12]
loc AN24 vme_data_b[13]
loc AU23 vme_data_b[14]
loc AT23 vme_data_b[15]
loc AP23 vme_data_b[16]
loc AN23 vme_data_b[17]
loc AE23 vme_data_b[18]
loc AD22 vme_data_b[19]
loc AL23 vme_data_b[20]
loc AK23 vme_data_b[21]
loc AU22 vme_data_b[22]
loc AT22 vme_data_b[23]
loc AW22 vme_data_b[24]
loc AV22 vme_data_b[25]
loc AW21 vme_data_b[26]
loc AV21 vme_data_b[27]
loc AH23 vme_data_b[28]
loc AG23 vme_data_b[29]
loc AF22 vme_data_b[30]
loc AE22 vme_data_b[31]
loc AD29 vme_am_i[0]
loc AH30 vme_am_i[1]
loc AG30 vme_am_i[2]
loc AV31 vme_am_i[3]
loc AU31 vme_am_i[4]
loc AW31 vme_am_i[5]
loc AL30 vme_addr_b[1]
loc AK30 vme_addr_b[2]
loc AT30 vme_addr_b[3]
loc AR30 vme_addr_b[4]
loc AV30 vme_addr_b[5]
loc AU30 vme_addr_b[6]
loc AU29 vme_addr_b[7]
loc AT29 vme_addr_b[8]
loc AP30 vme_addr_b[9]
loc AN30 vme_addr_b[10]
loc AP29 vme_addr_b[11]
loc AN29 vme_addr_b[12]
loc AC29 vme_addr_b[13]
loc AB29 vme_addr_b[14]
loc AG28 vme_addr_b[15]
loc AF28 vme_addr_b[16]
loc AL29 vme_addr_b[17]
loc AK29 vme_addr_b[18]
loc AJ28 vme_addr_b[19]
loc AH28 vme_addr_b[20]
loc AE28 vme_addr_b[21]
loc AD28 vme_addr_b[22]
loc AB28 vme_addr_b[23]
loc AB27 vme_addr_b[24]
loc AM28 vme_addr_b[25]
loc AD27 vme_addr_b[26]
loc AC27 vme_addr_b[27]
loc AR28 vme_addr_b[28]
loc AP28 vme_addr_b[29]
loc AV28 vme_addr_b[30]
loc AU28 vme_addr_b[31]

loc AM27 fmc_enable_n_o
loc C20  dio_led_term_o
loc D20  dio_led_out_o
loc D25  dio4_i "LVDS"
# VFC-HD has a discrete diff receiver for the FMC-DIO clock
# so the signal arrives to the FPGA as CMOS (single-ended)
loc AK34 dio5_clk_i
loc M29  dio4_oe_n_o
loc C28  dio5_oe_n_o
loc B27  dio4_term_en_o
loc C27  dio5_term_en_o

loc AH21 vfchd_gpio1_o
loc AG21 vfchd_gpio2_o
loc AL26 vfchd_gpio3_o
loc AV24 vfchd_gpio4_o

# Commit assignments
export_assignments

# SCRIPT EXECUTION ENDS HERE
post_message "WR VFC-HD reference design pre-flow script execution complete"
