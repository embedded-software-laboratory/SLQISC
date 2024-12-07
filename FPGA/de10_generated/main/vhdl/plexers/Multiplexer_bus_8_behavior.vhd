--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : Multiplexer_bus_8                                            ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF Multiplexer_bus_8 IS 

BEGIN

   makeMux : PROCESS(enable,
                     muxIn_0,
                     muxIn_1,
                     muxIn_2,
                     muxIn_3,
                     muxIn_4,
                     muxIn_5,
                     muxIn_6,
                     muxIn_7,
                     sel) IS
   BEGIN
      IF (enable = '0') THEN
         muxOut <= (OTHERS => '0');
                        ELSE
         CASE (sel) IS
            WHEN "000" => muxOut <= muxIn_0;
            WHEN "001" => muxOut <= muxIn_1;
            WHEN "010" => muxOut <= muxIn_2;
            WHEN "011" => muxOut <= muxIn_3;
            WHEN "100" => muxOut <= muxIn_4;
            WHEN "101" => muxOut <= muxIn_5;
            WHEN "110" => muxOut <= muxIn_6;
            WHEN OTHERS  => muxOut <= muxIn_7;
         END CASE;
      END IF;
   END PROCESS makeMux;

END platformIndependent;
