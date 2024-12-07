--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : Decoder_8                                                    ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF Decoder_8 IS 

BEGIN

   decoderOut_0 <= '1' WHEN sel = "000" AND
                            enable = '1' ELSE '0';

   decoderOut_1 <= '1' WHEN sel = "001" AND
                            enable = '1' ELSE '0';

   decoderOut_2 <= '1' WHEN sel = "010" AND
                            enable = '1' ELSE '0';

   decoderOut_3 <= '1' WHEN sel = "011" AND
                            enable = '1' ELSE '0';

   decoderOut_4 <= '1' WHEN sel = "100" AND
                            enable = '1' ELSE '0';

   decoderOut_5 <= '1' WHEN sel = "101" AND
                            enable = '1' ELSE '0';

   decoderOut_6 <= '1' WHEN sel = "110" AND
                            enable = '1' ELSE '0';

   decoderOut_7 <= '1' WHEN sel = "111" AND
                            enable = '1' ELSE '0';
END platformIndependent;
