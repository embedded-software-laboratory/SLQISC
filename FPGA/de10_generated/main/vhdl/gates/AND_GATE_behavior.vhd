--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : AND_GATE                                                     ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF AND_GATE IS 

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_realInput1 : std_logic;
   SIGNAL s_realInput2 : std_logic;

BEGIN

   --------------------------------------------------------------------------------
   -- Here the bubbles are processed                                             --
   --------------------------------------------------------------------------------
   s_realInput1 <= input1 WHEN BubblesMask(0) = '0' ELSE NOT(input1);
   s_realInput2 <= input2 WHEN BubblesMask(1) = '0' ELSE NOT(input2);

   --------------------------------------------------------------------------------
   -- Here the functionality is defined                                          --
   --------------------------------------------------------------------------------
   result <= s_realInput1 AND 
             s_realInput2;

END platformIndependent;
