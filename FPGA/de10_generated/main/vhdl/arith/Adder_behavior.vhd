--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : Adder                                                        ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF Adder IS 

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_extendedDataA : std_logic_vector( (extendedBits - 1) DOWNTO 0 );
   SIGNAL s_extendedDataB : std_logic_vector( (extendedBits - 1) DOWNTO 0 );
   SIGNAL s_sumResult     : std_logic_vector( (extendedBits - 1) DOWNTO 0 );

BEGIN

   s_extendedDataA <= "0"&dataA;
   s_extendedDataB <= "0"&dataB;
   s_sumResult     <= std_logic_vector(unsigned(s_extendedDataA) +
                                        unsigned(s_extendedDataB) +
                                        (""&carryIn));
   result   <= s_sumResult( (nrOfBits-1) DOWNTO 0 );
   carryOut <= s_sumResult(extendedBits-1);

END platformIndependent;
