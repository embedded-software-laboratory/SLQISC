--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : Negator                                                      ==
--==                                                                          ==
--==============================================================================


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;


ENTITY Negator IS
   GENERIC ( nrOfBits : INTEGER );
   PORT ( dataX    : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          minDataX : OUT std_logic_vector( (nrOfBits - 1) DOWNTO 0 ) );
END ENTITY Negator;
