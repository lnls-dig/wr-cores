fetchto = "../../ip_cores"

files = [
    "vfchd_wr_ref_top.vhd",
    "vfchd_i2cmux/vfchd_i2cmux_pkg.vhd",
    "vfchd_i2cmux/I2cMuxAndExpReqArbiter.v",
    "vfchd_i2cmux/I2cMuxAndExpMaster.v",
    "vfchd_i2cmux/SfpIdReader.v",
]

modules = {
    "local" : [
        "../../",
        "../../board/vfchd",
    ],
    "git" : [
        "git://ohwr.org/hdl-core-lib/general-cores.git",
        "git://ohwr.org/hdl-core-lib/vme64x-core.git",
        "git://ohwr.org/hdl-core-lib/etherbone-core.git",
    ],
}
