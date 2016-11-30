#!/bin/bash

mkdir -p doc
wbgen2  -C ./doc/wr_transmission.h -D ./doc/wr_transmission_wb.html -p wr_transmission_wbgen2_pkg.vhd -H record -V wr_transmission_wb.vhd --cstyle defines --lang vhdl -K ../../sim/wr_transmission_wb.svh wr_transmission_wb.wb