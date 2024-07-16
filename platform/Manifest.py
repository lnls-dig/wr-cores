import logging

if 'target' not in globals():
        logging.info("'target' is not defined, no platform selected")
elif target=="altera":
	modules = {"local" : "altera"}
elif target=="xilinx":
	modules = {"local" : "xilinx"}
