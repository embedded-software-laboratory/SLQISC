--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : LogisimCounter                                               ==
--==                                                                          ==
--==============================================================================


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;


ENTITY LogisimCounter IS
   GENERIC ( invertClock : INTEGER;
             maxVal      : std_logic_vector;
             mode        : INTEGER;
             width       : INTEGER );
   PORT ( clear      : IN  std_logic;
          clock      : IN  std_logic;
          enable     : IN  std_logic;
          load       : IN  std_logic;
          loadData   : IN  std_logic_vector( (width - 1) DOWNTO 0 );
          tick       : IN  std_logic;
          upNotDown  : IN  std_logic;
          compareOut : OUT std_logic;
          countValue : OUT std_logic_vector( (width - 1) DOWNTO 0 ) );
END ENTITY LogisimCounter;
