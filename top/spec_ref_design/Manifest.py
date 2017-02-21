fetchto = "../../ip_cores"

files = [
    "spec_wr_ref_top.vhd",
    "spec_wr_ref_top.ucf",
]

modules = {
    "local" : [
        "../../",
        "../../board/spec",
    ],
    "git" : [
        "git://ohwr.org/hdl-core-lib/general-cores.git",
        "git://ohwr.org/hdl-core-lib/gn4124-core.git",
        "git://ohwr.org/hdl-core-lib/etherbone-core.git",
    ],
}
