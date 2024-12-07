--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : LogisimClockComponent                                        ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF LogisimClockComponent IS 

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_bufferRegs    : std_logic_vector( 1 DOWNTO 0 );
   SIGNAL s_counterIsZero : std_logic;
   SIGNAL s_counterNext   : std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
   SIGNAL s_counterValue  : std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
   SIGNAL s_derivedClock  : std_logic_vector( (phase - 1) DOWNTO 0 );
   SIGNAL s_outputRegs    : std_logic_vector( 3 DOWNTO 0 );

BEGIN
   --------------------------------------------------------------------------------
   -- The output signals are defined here; we synchronize them all on the main   --
   -- clock                                                                      --
   --------------------------------------------------------------------------------

   clockBus <= globalClock&s_outputRegs;

   makeOutputs : PROCESS(globalClock) IS
   BEGIN
      IF (rising_edge(globalClock)) THEN
         s_bufferRegs(0)  <= s_derivedClock(phase - 1);
         s_bufferRegs(1)  <= NOT(s_derivedClock(phase - 1));
         s_outputRegs(0)  <= s_bufferRegs(0);
         s_outputRegs(1)  <= s_bufferRegs(1);
         s_outputRegs(2)  <= NOT(s_bufferRegs(0)) AND s_derivedClock(phase - 1);
         s_outputRegs(3)  <= s_bufferRegs(0) AND NOT(s_derivedClock(phase - 1));
      END IF;
   END PROCESS makeOutputs;

   --------------------------------------------------------------------------------
   -- The control signals are defined here                                       --
   --------------------------------------------------------------------------------
   s_counterIsZero <= '1' WHEN s_counterValue = std_logic_vector(to_unsigned(0,nrOfBits)) ELSE '0';
   s_counterNext   <= std_logic_vector(unsigned(s_counterValue) - 1)
                         WHEN s_counterIsZero = '0' ELSE
                      std_logic_vector(to_unsigned((lowTicks-1), nrOfBits))
                         WHEN s_derivedClock(0) = '1' ELSE
                      std_logic_vector(to_unsigned((highTicks-1), nrOfBits));

   --------------------------------------------------------------------------------
   -- The state registers are defined here                                       --
   --------------------------------------------------------------------------------
   makeDerivedClock : PROCESS(globalClock, clockTick, s_counterIsZero, s_derivedClock) IS
   BEGIN
      IF (rising_edge(globalClock)) THEN
         IF (s_derivedClock(0) /= '0' AND s_derivedClock(0) /= '1') THEN --For simulation only
            s_derivedClock <= (OTHERS => '1');
         ELSIF (clockTick = '1') THEN
            FOR n IN phase-1 DOWNTO 1 LOOP
              s_derivedClock(n) <= s_derivedClock(n-1);
            END LOOP;
            s_derivedClock(0) <= s_derivedClock(0) XOR s_counterIsZero;
         END IF;
      END IF;
   END PROCESS makeDerivedClock;

   makeCounter : PROCESS(globalClock, clockTick, s_counterNext, s_derivedClock) IS
   BEGIN
      IF (rising_edge(globalClock)) THEN
         IF (s_derivedClock(0) /= '0' AND s_derivedClock(0) /= '1') THEN --For simulation only
            s_counterValue <= (OTHERS => '0');
         ELSIF (clockTick = '1') THEN
            s_counterValue <= s_counterNext;
         END IF;
      END IF;
   END PROCESS makeCounter;

END platformIndependent;
