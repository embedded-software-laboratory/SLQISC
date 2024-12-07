--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : Negator                                                      ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF Negator IS 

BEGIN

   minDataX <= std_logic_vector(unsigned(NOT(dataX)) + 1);

END platformIndependent;
