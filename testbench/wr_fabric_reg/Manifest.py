action = "simulation"
files = "main.sv"
syn_device = "xc6slx45t"
syn_grade = "-3"
syn_package = "fgg484"
sim_tool = "modelsim"
top_module = "main"
fetchto = "../../ip_cores"

target = "xilinx"

vlog_opt="+incdir+../../sim"

include_dirs = [ "../../sim" ]

modules ={"local" : ["../../",
			"../../ip_cores/general-cores",
			"../../ip_cores/etherbone-core",
			"../../ip_cores/gn4124-core"]}
    
