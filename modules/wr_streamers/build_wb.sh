#!/bin/bash

mkdir -p doc
wbgen2  -C ./doc/wr_streamers.h -D ./doc/wr_streamers_wb.html -p wr_streamers_wbgen2_pkg.vhd -H record -V wr_streamers_wb.vhd --cstyle struct --lang vhdl -K ../../sim/wr_streamers_wb.svh wr_streamers_wb.wb