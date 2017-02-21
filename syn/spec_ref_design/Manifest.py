target = "xilinx"
action = "synthesis"

syn_device = "xc6slx45t"
syn_grade = "-3"
syn_package = "fgg484"

syn_top = "spec_wr_ref_top"
syn_project = "spec_wr_ref.xise"

syn_tool = "ise"

modules = { "local" : "../../top/spec_ref_design/"}
