--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : REGISTER_FLIP_FLOP                                           ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF REGISTER_FLIP_FLOP IS 

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_clock        : std_logic;
   SIGNAL s_currentState : std_logic_vector( (nrOfBits - 1) DOWNTO 0 );

BEGIN

   q       <= s_currentState;
   s_clock <= clock WHEN invertClock = 0 ELSE NOT(clock);

   makeMemory : PROCESS(s_clock, reset, clockEnable, tick, d) IS
   BEGIN
      IF (reset = '1') THEN s_currentState <= (OTHERS => '0');
   ELSIF (rising_Edge(s_clock)) THEN
      IF (clockEnable = '1' AND tick = '1') THEN
         s_currentState <= d;
      END IF;
      END IF;
   END PROCESS makeMemory;

END platformIndependent;
