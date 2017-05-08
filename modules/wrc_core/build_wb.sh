#!/bin/bash

mkdir -p doc
wbgen2 -D ./doc/wrc_syscon.html -p wrc_syscon_pkg.vhd -H record -V wrc_syscon_wb.vhd -C wrc_syscon_regs.h --cstyle defines --lang vhdl -K ../../sim/wrc_syscon_regs.vh wrc_syscon_wb.wb
wbgen2 -D ./doc/wrc_diags.html -p wrc_diags_pkg.vhd -H record -V wrc_diags_wb.vhd -C wrc_diags_regs.h --cstyle struct --lang vhdl -K ../../sim/wrc_diags_regs.vh wrc_diags_wb.wb
