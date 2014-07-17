action = "simulation"
target = "xilinx"
fetchto = "../../ip_cores"
vlog_opt="+incdir+../../sim +incdir+gn4124_bfm"

files = [ "main.sv" ]

modules = { "local" :  [ "../..", "../../top/spec_1_1/wr_core_demo", "../../../general-cores", "../../../gn4124-core", "../../../etherbone-core", "gn4124_bfm"] }

