--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : logisimTopLevelShell                                         ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF logisimTopLevelShell IS 

   -----------------------------------------------------------------------------
   -- Here all used components are defined                                    --
   -----------------------------------------------------------------------------
      COMPONENT synthesizedClockGenerator
         PORT ( FPGAClock        : IN  std_logic;
                SynthesizedClock : OUT std_logic );
      END COMPONENT;

      COMPONENT logisimTickGenerator
         GENERIC ( nrOfBits    : INTEGER;
                   reloadValue : INTEGER );
         PORT ( FPGAClock : IN  std_logic;
                FPGATick  : OUT std_logic );
      END COMPONENT;

      COMPONENT LogisimClockComponent
         GENERIC ( highTicks : INTEGER;
                   lowTicks  : INTEGER;
                   nrOfBits  : INTEGER;
                   phase     : INTEGER );
         PORT ( clockTick   : IN  std_logic;
                globalClock : IN  std_logic;
                clockBus    : OUT std_logic_vector( 4 DOWNTO 0 ) );
      END COMPONENT;

      COMPONENT LedArrayLedDefault
         GENERIC ( activeLow : INTEGER;
                   nrOfLeds  : INTEGER );
         PORT ( internalLeds : IN  std_logic_vector( (nrOfLeds - 1) DOWNTO 0 );
                externalLeds : OUT std_logic_vector( (nrOfLeds - 1) DOWNTO 0 ) );
      END COMPONENT;

      COMPONENT main
         PORT ( logisimClockTree0    : IN  std_logic_vector( 4 DOWNTO 0 );
                logisimInputBubbles  : IN  std_logic_vector( 0 DOWNTO 0 );
                logisimOutputBubbles : OUT std_logic_vector( 39 DOWNTO 0 ) );
      END COMPONENT;

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_fpgaTick             : std_logic;
   SIGNAL s_internalLeds0        : std_logic_vector( 9 DOWNTO 0 );
   SIGNAL s_logisimClockTree0    : std_logic_vector( 4 DOWNTO 0 );
   SIGNAL s_logisimInputBubbles  : std_logic_vector( 0 DOWNTO 0 );
   SIGNAL s_logisimOutputBubbles : std_logic_vector( 39 DOWNTO 0 );
   SIGNAL s_synthesizedClock     : std_logic;

BEGIN

   --------------------------------------------------------------------------------
   -- All signal adaptations are performed here                                  --
   --------------------------------------------------------------------------------
   n_Hex_Digit_Display_1_DecimalPoint <=  NOT s_logisimOutputBubbles(7);
   n_Hex_Digit_Display_1_Segment_A    <=  NOT s_logisimOutputBubbles(0);
   n_Hex_Digit_Display_1_Segment_B    <=  NOT s_logisimOutputBubbles(1);
   n_Hex_Digit_Display_1_Segment_C    <=  NOT s_logisimOutputBubbles(2);
   n_Hex_Digit_Display_1_Segment_D    <=  NOT s_logisimOutputBubbles(3);
   n_Hex_Digit_Display_1_Segment_E    <=  NOT s_logisimOutputBubbles(4);
   n_Hex_Digit_Display_1_Segment_F    <=  NOT s_logisimOutputBubbles(5);
   n_Hex_Digit_Display_1_Segment_G    <=  NOT s_logisimOutputBubbles(6);
   n_Hex_Digit_Display_2_DecimalPoint <=  NOT s_logisimOutputBubbles(15);
   n_Hex_Digit_Display_2_Segment_A    <=  NOT s_logisimOutputBubbles(8);
   n_Hex_Digit_Display_2_Segment_B    <=  NOT s_logisimOutputBubbles(9);
   n_Hex_Digit_Display_2_Segment_C    <=  NOT s_logisimOutputBubbles(10);
   n_Hex_Digit_Display_2_Segment_D    <=  NOT s_logisimOutputBubbles(11);
   n_Hex_Digit_Display_2_Segment_E    <=  NOT s_logisimOutputBubbles(12);
   n_Hex_Digit_Display_2_Segment_F    <=  NOT s_logisimOutputBubbles(13);
   n_Hex_Digit_Display_2_Segment_G    <=  NOT s_logisimOutputBubbles(14);
   n_Hex_Digit_Display_3_DecimalPoint <=  NOT s_logisimOutputBubbles(23);
   n_Hex_Digit_Display_3_Segment_A    <=  NOT s_logisimOutputBubbles(16);
   n_Hex_Digit_Display_3_Segment_B    <=  NOT s_logisimOutputBubbles(17);
   n_Hex_Digit_Display_3_Segment_C    <=  NOT s_logisimOutputBubbles(18);
   n_Hex_Digit_Display_3_Segment_D    <=  NOT s_logisimOutputBubbles(19);
   n_Hex_Digit_Display_3_Segment_E    <=  NOT s_logisimOutputBubbles(20);
   n_Hex_Digit_Display_3_Segment_F    <=  NOT s_logisimOutputBubbles(21);
   n_Hex_Digit_Display_3_Segment_G    <=  NOT s_logisimOutputBubbles(22);
   n_Hex_Digit_Display_4_DecimalPoint <=  NOT s_logisimOutputBubbles(31);
   n_Hex_Digit_Display_4_Segment_A    <=  NOT s_logisimOutputBubbles(24);
   n_Hex_Digit_Display_4_Segment_B    <=  NOT s_logisimOutputBubbles(25);
   n_Hex_Digit_Display_4_Segment_C    <=  NOT s_logisimOutputBubbles(26);
   n_Hex_Digit_Display_4_Segment_D    <=  NOT s_logisimOutputBubbles(27);
   n_Hex_Digit_Display_4_Segment_E    <=  NOT s_logisimOutputBubbles(28);
   n_Hex_Digit_Display_4_Segment_F    <=  NOT s_logisimOutputBubbles(29);
   n_Hex_Digit_Display_4_Segment_G    <=  NOT s_logisimOutputBubbles(30);
   s_logisimInputBubbles(0)           <=  NOT n_RESET_0;

   --------------------------------------------------------------------------------
   -- The clock tree components are defined here                                 --
   --------------------------------------------------------------------------------
   BASE_0 : synthesizedClockGenerator
      PORT MAP ( FPGAClock        => fpgaGlobalClock,
                 SynthesizedClock => s_synthesizedClock );

   BASE_1 : logisimTickGenerator
      GENERIC MAP ( nrOfBits    => 6,
                    reloadValue => 47 )
      PORT MAP ( FPGAClock => s_synthesizedClock,
                 FPGATick  => s_fpgaTick );

   BASE_2 : LogisimClockComponent
      GENERIC MAP ( highTicks => 1,
                    lowTicks  => 1,
                    nrOfBits  => 1,
                    phase     => 1 )
      PORT MAP ( clockBus    => s_logisimClockTree0,
                 clockTick   => s_fpgaTick,
                 globalClock => s_synthesizedClock );

   --------------------------------------------------------------------------------
   -- The toplevel component is connected here                                   --
   --------------------------------------------------------------------------------
   CIRCUIT_0 : main
      PORT MAP ( logisimClockTree0    => s_logisimClockTree0,
                 logisimInputBubbles  => s_logisimInputBubbles,
                 logisimOutputBubbles => s_logisimOutputBubbles );

   --------------------------------------------------------------------------------
   -- The Led arrays are connected here                                          --
   --------------------------------------------------------------------------------
   array0 : LedArrayLedDefault
      GENERIC MAP ( nrOfLeds  => 10,
                    activeLow => 1 )
   PORT MAP ( externalLeds => externalLeds0,
              internalLeds => s_internalLeds0 );

   s_internalLeds0(0) <= s_logisimOutputBubbles(32);
   s_internalLeds0(1) <= s_logisimOutputBubbles(33);
   s_internalLeds0(2) <= s_logisimOutputBubbles(34);
   s_internalLeds0(3) <= s_logisimOutputBubbles(35);
   s_internalLeds0(4) <= s_logisimOutputBubbles(36);
   s_internalLeds0(5) <= s_logisimOutputBubbles(37);
   s_internalLeds0(6) <= s_logisimOutputBubbles(38);
   s_internalLeds0(7) <= s_logisimOutputBubbles(39);
   s_internalLeds0(8) <= '0';
   s_internalLeds0(9) <= '0';

END platformIndependent;
