--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : Multiplexer_bus_8                                            ==
--==                                                                          ==
--==============================================================================


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;


ENTITY Multiplexer_bus_8 IS
   GENERIC ( nrOfBits : INTEGER );
   PORT ( enable  : IN  std_logic;
          muxIn_0 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          muxIn_1 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          muxIn_2 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          muxIn_3 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          muxIn_4 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          muxIn_5 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          muxIn_6 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          muxIn_7 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
          sel     : IN  std_logic_vector( 2 DOWNTO 0 );
          muxOut  : OUT std_logic_vector( (nrOfBits - 1) DOWNTO 0 ) );
END ENTITY Multiplexer_bus_8;
