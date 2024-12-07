--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : LedArrayLedDefault                                           ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF LedArrayLedDefault IS 

BEGIN
   genLeds : FOR n IN (nrOfLeds-1) DOWNTO 0 GENERATE
      externalLeds(n) <= NOT(internalLeds(n)) WHEN activeLow = 1 ELSE internalLeds(n);
   END GENERATE;

END platformIndependent;
