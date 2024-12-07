--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : synthesizedClockGenerator                                    ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF synthesizedClockGenerator IS 

BEGIN

   --------------------------------------------------------------------------------
   -- Here the update logic is defined. Loop back the global clock.              --
   --------------------------------------------------------------------------------
    SynthesizedClock  <=  FPGAClock;
END platformIndependent;
