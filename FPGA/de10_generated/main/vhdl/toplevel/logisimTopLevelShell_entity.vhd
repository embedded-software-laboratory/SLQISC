--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : logisimTopLevelShell                                         ==
--==                                                                          ==
--==============================================================================


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;


ENTITY logisimTopLevelShell IS
   PORT ( fpgaGlobalClock                    : IN  std_logic;
          n_RESET_0                          : IN  std_logic;
          externalLeds0                      : OUT std_logic_vector( 9 DOWNTO 0 );
          n_Hex_Digit_Display_1_DecimalPoint : OUT std_logic;
          n_Hex_Digit_Display_1_Segment_A    : OUT std_logic;
          n_Hex_Digit_Display_1_Segment_B    : OUT std_logic;
          n_Hex_Digit_Display_1_Segment_C    : OUT std_logic;
          n_Hex_Digit_Display_1_Segment_D    : OUT std_logic;
          n_Hex_Digit_Display_1_Segment_E    : OUT std_logic;
          n_Hex_Digit_Display_1_Segment_F    : OUT std_logic;
          n_Hex_Digit_Display_1_Segment_G    : OUT std_logic;
          n_Hex_Digit_Display_2_DecimalPoint : OUT std_logic;
          n_Hex_Digit_Display_2_Segment_A    : OUT std_logic;
          n_Hex_Digit_Display_2_Segment_B    : OUT std_logic;
          n_Hex_Digit_Display_2_Segment_C    : OUT std_logic;
          n_Hex_Digit_Display_2_Segment_D    : OUT std_logic;
          n_Hex_Digit_Display_2_Segment_E    : OUT std_logic;
          n_Hex_Digit_Display_2_Segment_F    : OUT std_logic;
          n_Hex_Digit_Display_2_Segment_G    : OUT std_logic;
          n_Hex_Digit_Display_3_DecimalPoint : OUT std_logic;
          n_Hex_Digit_Display_3_Segment_A    : OUT std_logic;
          n_Hex_Digit_Display_3_Segment_B    : OUT std_logic;
          n_Hex_Digit_Display_3_Segment_C    : OUT std_logic;
          n_Hex_Digit_Display_3_Segment_D    : OUT std_logic;
          n_Hex_Digit_Display_3_Segment_E    : OUT std_logic;
          n_Hex_Digit_Display_3_Segment_F    : OUT std_logic;
          n_Hex_Digit_Display_3_Segment_G    : OUT std_logic;
          n_Hex_Digit_Display_4_DecimalPoint : OUT std_logic;
          n_Hex_Digit_Display_4_Segment_A    : OUT std_logic;
          n_Hex_Digit_Display_4_Segment_B    : OUT std_logic;
          n_Hex_Digit_Display_4_Segment_C    : OUT std_logic;
          n_Hex_Digit_Display_4_Segment_D    : OUT std_logic;
          n_Hex_Digit_Display_4_Segment_E    : OUT std_logic;
          n_Hex_Digit_Display_4_Segment_F    : OUT std_logic;
          n_Hex_Digit_Display_4_Segment_G    : OUT std_logic );
END ENTITY logisimTopLevelShell;
