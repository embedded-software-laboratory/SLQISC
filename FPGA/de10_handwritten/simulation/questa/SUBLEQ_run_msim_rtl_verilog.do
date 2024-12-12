transcript on
if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

vlog -vlog01compat -work work +incdir+C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten/sdram {C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten/sdram/sdram_write.v}
vlog -vlog01compat -work work +incdir+C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten/sdram {C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten/sdram/sdram_read.v}
vlog -vlog01compat -work work +incdir+C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten/sdram {C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten/sdram/sdram_intialize.v}
vlog -vlog01compat -work work +incdir+C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten/sdram {C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten/sdram/sdram_controller.v}
vlog -vlog01compat -work work +incdir+C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten {C:/Users/voelker/Documents/SLQISC/FPGA/de10_handwritten/SUBLEQ.v}

