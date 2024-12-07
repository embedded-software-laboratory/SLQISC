--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : Multiplexer_bus_2                                            ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF Multiplexer_bus_2 IS 

BEGIN

   makeMux : PROCESS(enable,
                     muxIn_0,
                     muxIn_1,
                     sel) IS
   BEGIN
      IF (enable = '0') THEN
         muxOut <= (OTHERS => '0');
                        ELSE
         CASE (sel) IS
            WHEN '0' => muxOut <= muxIn_0;
            WHEN OTHERS  => muxOut <= muxIn_1;
         END CASE;
      END IF;
   END PROCESS makeMux;

END platformIndependent;
