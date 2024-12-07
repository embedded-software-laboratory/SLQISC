--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : REGISTER_FLIP_FLOP                                           ==
--==                                                                          ==
--==============================================================================


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;


ENTITY REGISTER_FLIP_FLOP IS
   GENERIC ( invertClock : INTEGER;
             nrOfBits    : INTEGER );
   PORT ( clock       : IN  std_logic;
          clockEnable : IN  std_logic;
          d           : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          reset       : IN  std_logic;
          tick        : IN  std_logic;
          q           : OUT std_logic_vector( (nrOfBits - 1) DOWNTO 0 ) );
END ENTITY REGISTER_FLIP_FLOP;
