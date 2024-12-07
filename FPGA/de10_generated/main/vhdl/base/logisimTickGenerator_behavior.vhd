--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : logisimTickGenerator                                         ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF logisimTickGenerator IS 

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_countNext : std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
   SIGNAL s_countReg  : std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
   SIGNAL s_tickNext  : std_logic;
   SIGNAL s_tickReg   : std_logic;

BEGIN

   --------------------------------------------------------------------------------
   -- Here the output is defined                                                 --
   --------------------------------------------------------------------------------
    FPGATick  <=  s_tickReg;

   --------------------------------------------------------------------------------
   -- Here the update logic is defined                                           --
   --------------------------------------------------------------------------------
   s_tickNext   <= '1' WHEN s_countReg = std_logic_vector(to_unsigned(0, nrOfBits)) ELSE '0';
   s_countNext  <= (OTHERS => '0') WHEN s_tickReg /= '0' AND s_tickReg /= '1' ELSE -- For simulation only!
                   std_logic_vector(to_unsigned((reloadValue-1), nrOfBits)) WHEN s_tickNext = '1' ELSE
                   std_logic_vector(unsigned(s_countReg)-1);

   --------------------------------------------------------------------------------
   -- Here the flipflops are defined                                             --
   --------------------------------------------------------------------------------
   makeFlipFlops : PROCESS(FPGAClock) IS
   BEGIN
      IF (rising_edge(FPGAClock)) THEN
         s_tickReg  <= s_tickNext;
         s_countReg <= s_countNext;
      END IF;
   END PROCESS makeFlipFlops;

END platformIndependent;
