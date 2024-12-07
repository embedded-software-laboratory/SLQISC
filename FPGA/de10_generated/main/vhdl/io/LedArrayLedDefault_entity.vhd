--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : LedArrayLedDefault                                           ==
--==                                                                          ==
--==============================================================================


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;


ENTITY LedArrayLedDefault IS
   GENERIC ( activeLow : INTEGER;
             nrOfLeds  : INTEGER );
   PORT ( internalLeds : IN  std_logic_vector( (nrOfLeds - 1) DOWNTO 0 );
          externalLeds : OUT std_logic_vector( (nrOfLeds - 1) DOWNTO 0 ) );
END ENTITY LedArrayLedDefault;
