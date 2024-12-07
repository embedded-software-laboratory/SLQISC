--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : LogisimCounter                                               ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF LogisimCounter IS 

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_carry            : std_logic;
   SIGNAL s_clock            : std_logic;
   SIGNAL s_counterValue     : std_logic_vector( (width - 1) DOWNTO 0 );
   SIGNAL s_nextCounterValue : std_logic_vector( (width - 1) DOWNTO 0 );
   SIGNAL s_realEnable       : std_logic;

BEGIN

   --------------------------------------------------------------------------------
   -- Functionality of the counter:                                              --
   --   Load Count | mode                                                        --
   --                                                                            --
   -- -----------+-------------------                                            --
   --     0    0   | halt                                                        --
   --     0    1   | count                                                       --
   -- up (default)                                                               --
   --     1    0   | load                                                        --
   --     1    1   | count down                                                  --
   --------------------------------------------------------------------------------

   compareOut   <= s_carry;
   countValue   <= s_counterValue;

   s_clock      <= clock WHEN invertClock = 0 ELSE NOT(clock);

   makeCarry : PROCESS(upNotDown, s_counterValue) IS
   BEGIN
      IF (upNotDown = '0') THEN
         IF (s_counterValue = std_logic_vector(to_unsigned(0,width))) THEN
            s_carry <= '1';
         ELSE
            s_carry <= '0';
         END IF; -- Down counting
      ELSE
         IF (s_counterValue = maxVal) THEN
            s_carry <= '1';
         ELSE
            s_carry <= '0';
         END IF; -- Up counting
      END IF;
   END PROCESS makeCarry;

   s_realEnable <= '0' WHEN (load = '0' AND enable = '0') -- Counter disabled
                          OR (mode = 1 AND s_carry = '1' AND load = '0') -- Stay at value situation
                        ELSE tick;

   makeNextValue : PROCESS(load ,upNotDown ,s_counterValue ,loadData , s_carry) IS
      VARIABLE v_downcount : std_logic;
   BEGIN
      v_downcount := NOT(upNotDown);
      IF ((load = '1') OR -- load condition
          (mode = 3 AND s_carry = '1')    -- Wrap load condition
         ) THEN s_nextCounterValue <= loadData;
      ELSE
         CASE (mode) IS
            WHEN  0    => IF (s_carry = '1') THEN
                             IF (v_downcount = '1') THEN
                                s_nextCounterValue <= maxVal;
                             ELSE
                                s_nextCounterValue <= (OTHERS => '0');
                             END IF;
                          ELSE
                             IF (v_downcount = '1') THEN
                                s_nextCounterValue <= std_logic_vector(unsigned(s_counterValue) - 1);
                             ELSE
                                s_nextCounterValue <= std_logic_vector(unsigned(s_counterValue) + 1);
                             END IF;
                          END IF;
           WHEN OTHERS => IF (v_downcount = '1') THEN
                              s_nextCounterValue <= std_logic_vector(unsigned(s_counterValue) - 1);
                          ELSE
                              s_nextCounterValue <= std_logic_vector(unsigned(s_counterValue) + 1);
                          END IF;
         END CASE;
      END IF;
   END PROCESS makeNextValue;

   makeFlops : PROCESS(s_clock, s_realEnable, clear, s_nextCounterValue ) IS
   BEGIN
      IF (clear = '1') THEN s_counterValue <= (OTHERS => '0');
      ELSIF (rising_edge(s_clock)) THEN
         IF (s_realEnable = '1') THEN s_counterValue <= s_nextCounterValue;
         END IF;
      END IF;
   END PROCESS makeFlops;

END platformIndependent;
