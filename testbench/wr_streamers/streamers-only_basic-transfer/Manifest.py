action= "simulation"
target= "xilinx"
syn_device="xc6slx45t"
sim_tool="modelsim"
top_module="main"

fetchto="../../../ip_cores"

include_dirs=["../../../sim"]

modules = { "local" : ["../../..",
                       "../../../modules/wr_streamers",
                       "../../../ip_cores/general-cores"]}
					  
files = ["main.sv"]

