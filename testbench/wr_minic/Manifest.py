action = "simulation"
target = "xilinx"
syn_device = "xc6slx45t"
syn_grade = "-3"
syn_package = "fgg484"
sim_tool = "modelsim"
top_module = "main"

include_dirs = [ "../../sim" ]

files = [ "main.sv" ]

modules = { "local" : [ "../../",
                     "../../ip_cores/general-cores"
                     ]};
