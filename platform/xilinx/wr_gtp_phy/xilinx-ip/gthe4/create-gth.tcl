# Regeneration of the gthe4 IP
# Open a vivado project with an ZynqUS+ target
# in the TCL console
#  source ./create-gth.tcl
# Check the differences
# Cleanup (comment out unused gthe3, gtye3 and gtye4 instantiations; this
#  avoids adding extra unused files)
# Copy the files

set module_name {gtwizard_ultrascale_2}

create_ip -name gtwizard_ultrascale -vendor xilinx.com -library ip -version 1.7 -module_name $module_name -dir .

set_property CONFIG.preset {GTH-Gigabit_Ethernet} [get_ips $module_name]

set_property -dict [list \
  CONFIG.RX_BUFFER_MODE {0} \
  CONFIG.TX_BUFFER_MODE {0} \
  CONFIG.RX_SLIDE_MODE {PCS} \
  CONFIG.LOCATE_IN_SYSTEM_IBERT_CORE {EXAMPLE_DESIGN} \
  CONFIG.LOCATE_RX_USER_CLOCKING {CORE} \
  CONFIG.LOCATE_TX_USER_CLOCKING {CORE} \
] [get_ips $module_name]

generate_target all [get_ips $module_name]
catch { config_ip_cache -export [get_ips $module_name] }
export_ip_user_files -of_objects [get_ips $module_name] -no_script -sync -force -quiet
create_ip_run [get_ips $module_name]

unset module_name
