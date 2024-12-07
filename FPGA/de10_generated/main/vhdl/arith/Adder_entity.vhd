--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : Adder                                                        ==
--==                                                                          ==
--==============================================================================


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;


ENTITY Adder IS
   GENERIC ( extendedBits : INTEGER;
             nrOfBits     : INTEGER );
   PORT ( carryIn  : IN  std_logic;
          dataA    : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          dataB    : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          carryOut : OUT std_logic;
          result   : OUT std_logic_vector( (nrOfBits - 1) DOWNTO 0 ) );
END ENTITY Adder;
