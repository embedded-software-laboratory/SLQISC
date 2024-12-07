--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : Comparator                                                   ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF Comparator IS 

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_signedGreater   : std_logic;
   SIGNAL s_signedLess      : std_logic;
   SIGNAL s_unsignedGreater : std_logic;
   SIGNAL s_unsignedLess    : std_logic;

BEGIN

   s_signedLess      <= '1' WHEN signed(dataA) < signed(dataB) ELSE '0';
   s_unsignedLess    <= '1' WHEN unsigned(dataA) < unsigned(dataB) ELSE '0';
   s_signedGreater   <= '1' WHEN signed(dataA) > signed(dataB) ELSE '0';
   s_unsignedGreater <= '1' WHEN unsigned(dataA) > unsigned(dataB) ELSE '0';

   aEqualsB      <= '1' WHEN dataA = dataB ELSE '0';
   aGreaterThanB <= s_signedGreater WHEN twosComplement = 1 ELSE s_unsignedGreater;
   aLessThanB    <= s_signedLess WHEN twosComplement = 1 ELSE s_unsignedLess;

END platformIndependent;
