--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : OR_GATE                                                      ==
--==                                                                          ==
--==============================================================================


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;


ENTITY OR_GATE IS
   GENERIC ( BubblesMask : std_logic_vector );
   PORT ( input1 : IN  std_logic;
          input2 : IN  std_logic;
          result : OUT std_logic );
END ENTITY OR_GATE;
