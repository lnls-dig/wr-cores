#! /bin/sh

set -e

for i in 				\
	eca_internals_pkg.vhd		\
	eca_compact.vhd			\
	eca_compact_tb.vhd		\
	eca_bitonic_swap.vhd		\
	eca_bitonic_helper.vhd		\
	eca_bitonic.vhd			\
	eca_bitonic_tb.vhd		\
	eca_sdp.vhd			\
	eca_tdp.vhd			\
	eca_fifo.vhd			\
	eca_fifo_tb.vhd			\
	eca_piso_fifo.vhd		\
	eca_piso_fifo_tb.vhd		\
	eca_rmw.vhd			\
	eca_rmw_tb.vhd			\
	eca_free.vhd			\
	eca_free_tb.vhd			\
	eca_data.vhd			\
	eca_scan.vhd			\
	eca_scan_tb.vhd			\
	eca_tag_channel.vhd		\
	eca_tag_channel_tb.vhd		\
	eca_tb.vhd;			\
do echo $i; ghdl -a --std=93 --ieee=standard --syn-binding  $i
done

echo link
ghdl -e --std=93 --ieee=standard --syn-binding eca_tb

echo run
./eca_tb --wave=testbench.ghw

gtkwave testbench.ghw wave.gtkw
