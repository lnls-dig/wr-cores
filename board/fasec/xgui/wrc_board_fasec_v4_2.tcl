# Definitional proc to organize widgets for parameters.
proc init_gui { IPINST } {
  ipgui::add_param $IPINST -name "Component_Name"
  #Adding Page
  ipgui::add_page $IPINST -name "Page 0"


}

proc update_PARAM_VALUE.g_aux_clks { PARAM_VALUE.g_aux_clks } {
	# Procedure called to update g_aux_clks when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.g_aux_clks { PARAM_VALUE.g_aux_clks } {
	# Procedure called to validate g_aux_clks
	return true
}

proc update_PARAM_VALUE.g_diag_id { PARAM_VALUE.g_diag_id } {
	# Procedure called to update g_diag_id when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.g_diag_id { PARAM_VALUE.g_diag_id } {
	# Procedure called to validate g_diag_id
	return true
}

proc update_PARAM_VALUE.g_diag_ro_vector_width { PARAM_VALUE.g_diag_ro_vector_width } {
	# Procedure called to update g_diag_ro_vector_width when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.g_diag_ro_vector_width { PARAM_VALUE.g_diag_ro_vector_width } {
	# Procedure called to validate g_diag_ro_vector_width
	return true
}

proc update_PARAM_VALUE.g_diag_rw_vector_width { PARAM_VALUE.g_diag_rw_vector_width } {
	# Procedure called to update g_diag_rw_vector_width when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.g_diag_rw_vector_width { PARAM_VALUE.g_diag_rw_vector_width } {
	# Procedure called to validate g_diag_rw_vector_width
	return true
}

proc update_PARAM_VALUE.g_diag_ver { PARAM_VALUE.g_diag_ver } {
	# Procedure called to update g_diag_ver when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.g_diag_ver { PARAM_VALUE.g_diag_ver } {
	# Procedure called to validate g_diag_ver
	return true
}

proc update_PARAM_VALUE.g_dpram_initf { PARAM_VALUE.g_dpram_initf } {
	# Procedure called to update g_dpram_initf when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.g_dpram_initf { PARAM_VALUE.g_dpram_initf } {
	# Procedure called to validate g_dpram_initf
	return true
}

proc update_PARAM_VALUE.g_fabric_iface { PARAM_VALUE.g_fabric_iface } {
	# Procedure called to update g_fabric_iface when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.g_fabric_iface { PARAM_VALUE.g_fabric_iface } {
	# Procedure called to validate g_fabric_iface
	return true
}

proc update_PARAM_VALUE.g_simulation { PARAM_VALUE.g_simulation } {
	# Procedure called to update g_simulation when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.g_simulation { PARAM_VALUE.g_simulation } {
	# Procedure called to validate g_simulation
	return true
}

proc update_PARAM_VALUE.g_with_external_clock_input { PARAM_VALUE.g_with_external_clock_input } {
	# Procedure called to update g_with_external_clock_input when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.g_with_external_clock_input { PARAM_VALUE.g_with_external_clock_input } {
	# Procedure called to validate g_with_external_clock_input
	return true
}


proc update_MODELPARAM_VALUE.g_simulation { MODELPARAM_VALUE.g_simulation PARAM_VALUE.g_simulation } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.g_simulation}] ${MODELPARAM_VALUE.g_simulation}
}

proc update_MODELPARAM_VALUE.g_with_external_clock_input { MODELPARAM_VALUE.g_with_external_clock_input PARAM_VALUE.g_with_external_clock_input } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.g_with_external_clock_input}] ${MODELPARAM_VALUE.g_with_external_clock_input}
}

proc update_MODELPARAM_VALUE.g_aux_clks { MODELPARAM_VALUE.g_aux_clks PARAM_VALUE.g_aux_clks } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.g_aux_clks}] ${MODELPARAM_VALUE.g_aux_clks}
}

proc update_MODELPARAM_VALUE.g_fabric_iface { MODELPARAM_VALUE.g_fabric_iface PARAM_VALUE.g_fabric_iface } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.g_fabric_iface}] ${MODELPARAM_VALUE.g_fabric_iface}
}

proc update_MODELPARAM_VALUE.g_dpram_initf { MODELPARAM_VALUE.g_dpram_initf PARAM_VALUE.g_dpram_initf } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.g_dpram_initf}] ${MODELPARAM_VALUE.g_dpram_initf}
}

proc update_MODELPARAM_VALUE.g_diag_id { MODELPARAM_VALUE.g_diag_id PARAM_VALUE.g_diag_id } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.g_diag_id}] ${MODELPARAM_VALUE.g_diag_id}
}

proc update_MODELPARAM_VALUE.g_diag_ver { MODELPARAM_VALUE.g_diag_ver PARAM_VALUE.g_diag_ver } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.g_diag_ver}] ${MODELPARAM_VALUE.g_diag_ver}
}

proc update_MODELPARAM_VALUE.g_diag_ro_vector_width { MODELPARAM_VALUE.g_diag_ro_vector_width PARAM_VALUE.g_diag_ro_vector_width } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.g_diag_ro_vector_width}] ${MODELPARAM_VALUE.g_diag_ro_vector_width}
}

proc update_MODELPARAM_VALUE.g_diag_rw_vector_width { MODELPARAM_VALUE.g_diag_rw_vector_width PARAM_VALUE.g_diag_rw_vector_width } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.g_diag_rw_vector_width}] ${MODELPARAM_VALUE.g_diag_rw_vector_width}
}

