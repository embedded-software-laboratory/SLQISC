# SLQISC

A collection of tools and implementations around the SUBLEQ (Subtract and Branch if Less than or Equal to Zero) one-instruction set computer. The project is designed for learning and teaching purposes.

## Circuit

Multiple variants of a circuit modeling a functional SUBLEQ one instruction CPU using [Logisim Evolution](https://github.com/logisim-evolution/logisim-evolution) can be found in [/Circuit](/Circuit/).

For further information, read the corresponding [README](/Circuit/README.md).

## Assembler

A [Haskell Cabal](https://www.haskell.org/cabal/) [project](/Subleq/) implementing an assembler and simulator to compile programs into Subleq instructions, which can then be executed on a SUBLEQ CPU.
The simulator also provides a debug environment to simplify the development of demonstration programs for the OISC.

For further information, read the corresponding [README](/Subleq/README.md).

## FPGA

The files located in the [/FPGA](/FPGA/) directory are generated from the Logisim Evolution circuit. The FPGA design is implemented in Verilog.

These generated files represent a hardware realization of the SUBLEQ CPU and can be used for synthesis and deployment on supported FPGA platforms. In particular, the circuit can be flashed onto a [Terasic DE10-Lite](https://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&No=1021) evaluation board using the [Quartus Prime](https://www.intel.de/content/www/de/de/products/details/fpga/development-tools/quartus-prime.html) software, allowing subleq programs to be executed on a physical system rather than only in simulation.
