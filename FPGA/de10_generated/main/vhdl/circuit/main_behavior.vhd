--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : main                                                         ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF main IS 

   -----------------------------------------------------------------------------
   -- Here all used components are defined                                    --
   -----------------------------------------------------------------------------

      COMPONENT AND_GATE
         GENERIC ( BubblesMask : std_logic_vector );
         PORT ( input1 : IN  std_logic;
                input2 : IN  std_logic;
                result : OUT std_logic );
      END COMPONENT;

      COMPONENT OR_GATE
         GENERIC ( BubblesMask : std_logic_vector );
         PORT ( input1 : IN  std_logic;
                input2 : IN  std_logic;
                result : OUT std_logic );
      END COMPONENT;

      COMPONENT Multiplexer_bus_2
         GENERIC ( nrOfBits : INTEGER );
         PORT ( enable  : IN  std_logic;
                muxIn_0 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                muxIn_1 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                sel     : IN  std_logic;
                muxOut  : OUT std_logic_vector( (nrOfBits - 1) DOWNTO 0 ) );
      END COMPONENT;

      COMPONENT Multiplexer_bus_8
         GENERIC ( nrOfBits : INTEGER );
         PORT ( enable  : IN  std_logic;
                muxIn_0 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                muxIn_1 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                muxIn_2 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                muxIn_3 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                muxIn_4 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                muxIn_5 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                muxIn_6 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                muxIn_7 : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                sel     : IN  std_logic_vector( 2 DOWNTO 0 );
                muxOut  : OUT std_logic_vector( (nrOfBits - 1) DOWNTO 0 ) );
      END COMPONENT;

      COMPONENT Decoder_8
         PORT ( enable       : IN  std_logic;
                sel          : IN  std_logic_vector( 2 DOWNTO 0 );
                decoderOut_0 : OUT std_logic;
                decoderOut_1 : OUT std_logic;
                decoderOut_2 : OUT std_logic;
                decoderOut_3 : OUT std_logic;
                decoderOut_4 : OUT std_logic;
                decoderOut_5 : OUT std_logic;
                decoderOut_6 : OUT std_logic;
                decoderOut_7 : OUT std_logic );
      END COMPONENT;

      COMPONENT Negator
         GENERIC ( nrOfBits : INTEGER );
         PORT ( dataX    : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                minDataX : OUT std_logic_vector( (nrOfBits - 1) DOWNTO 0 ) );
      END COMPONENT;

      COMPONENT Comparator
         GENERIC ( nrOfBits       : INTEGER;
                   twosComplement : INTEGER );
         PORT ( dataA         : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                dataB         : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                aEqualsB      : OUT std_logic;
                aGreaterThanB : OUT std_logic;
                aLessThanB    : OUT std_logic );
      END COMPONENT;

      COMPONENT Adder
         GENERIC ( extendedBits : INTEGER;
                   nrOfBits     : INTEGER );
         PORT ( carryIn  : IN  std_logic;
                dataA    : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                dataB    : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                carryOut : OUT std_logic;
                result   : OUT std_logic_vector( (nrOfBits - 1) DOWNTO 0 ) );
      END COMPONENT;

      COMPONENT REGISTER_FLIP_FLOP
         GENERIC ( invertClock : INTEGER;
                   nrOfBits    : INTEGER );
         PORT ( clock       : IN  std_logic;
                clockEnable : IN  std_logic;
                d           : IN  std_logic_vector( (nrOfBits - 1) DOWNTO 0 );
                reset       : IN  std_logic;
                tick        : IN  std_logic;
                q           : OUT std_logic_vector( (nrOfBits - 1) DOWNTO 0 ) );
      END COMPONENT;

      COMPONENT LogisimCounter
         GENERIC ( invertClock : INTEGER;
                   maxVal      : std_logic_vector;
                   mode        : INTEGER;
                   width       : INTEGER );
         PORT ( clear      : IN  std_logic;
                clock      : IN  std_logic;
                enable     : IN  std_logic;
                load       : IN  std_logic;
                loadData   : IN  std_logic_vector( (width - 1) DOWNTO 0 );
                tick       : IN  std_logic;
                upNotDown  : IN  std_logic;
                compareOut : OUT std_logic;
                countValue : OUT std_logic_vector( (width - 1) DOWNTO 0 ) );
      END COMPONENT;

      COMPONENT RAMCONTENTS_RAM_1
         PORT ( address : IN  std_logic_vector( 13 DOWNTO 0 );
                clock   : IN  std_logic;
                dataIn  : IN  std_logic_vector( 15 DOWNTO 0 );
                oe      : IN  std_logic;
                tick    : IN  std_logic;
                we      : IN  std_logic;
                dataOut : OUT std_logic_vector( 15 DOWNTO 0 ) );
      END COMPONENT;

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_logisimBus12 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus13 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus14 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus17 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus2  : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus21 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus23 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus24 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus27 : std_logic_vector( 13 DOWNTO 0 );
   SIGNAL s_logisimBus29 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus32 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus33 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus38 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus39 : std_logic_vector( 13 DOWNTO 0 );
   SIGNAL s_logisimBus40 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus41 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus46 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus49 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus5  : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus52 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus53 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus54 : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_logisimBus55 : std_logic_vector( 2 DOWNTO 0 );
   SIGNAL s_logisimBus6  : std_logic_vector( 2 DOWNTO 0 );
   SIGNAL s_logisimBus8  : std_logic_vector( 13 DOWNTO 0 );
   SIGNAL s_logisimNet0  : std_logic;
   SIGNAL s_logisimNet1  : std_logic;
   SIGNAL s_logisimNet10 : std_logic;
   SIGNAL s_logisimNet11 : std_logic;
   SIGNAL s_logisimNet15 : std_logic;
   SIGNAL s_logisimNet16 : std_logic;
   SIGNAL s_logisimNet19 : std_logic;
   SIGNAL s_logisimNet20 : std_logic;
   SIGNAL s_logisimNet22 : std_logic;
   SIGNAL s_logisimNet25 : std_logic;
   SIGNAL s_logisimNet26 : std_logic;
   SIGNAL s_logisimNet28 : std_logic;
   SIGNAL s_logisimNet30 : std_logic;
   SIGNAL s_logisimNet31 : std_logic;
   SIGNAL s_logisimNet35 : std_logic;
   SIGNAL s_logisimNet36 : std_logic;
   SIGNAL s_logisimNet4  : std_logic;
   SIGNAL s_logisimNet43 : std_logic;
   SIGNAL s_logisimNet44 : std_logic;
   SIGNAL s_logisimNet45 : std_logic;
   SIGNAL s_logisimNet47 : std_logic;
   SIGNAL s_logisimNet48 : std_logic;
   SIGNAL s_logisimNet50 : std_logic;
   SIGNAL s_logisimNet51 : std_logic;
   SIGNAL s_logisimNet56 : std_logic;
   SIGNAL s_logisimNet7  : std_logic;
   SIGNAL s_logisimNet9  : std_logic;

BEGIN

   --------------------------------------------------------------------------------
   -- All clock generator connections are defined here                           --
   --------------------------------------------------------------------------------
   s_logisimNet7 <= logisimClockTree0(0);

   --------------------------------------------------------------------------------
   -- Here all in-lined components are defined                                   --
   --------------------------------------------------------------------------------

   -- Constant
    s_logisimBus52(15 DOWNTO 0)  <=  X"FFFF";


   -- Constant
    s_logisimNet20  <=  '0';


   -- Constant
    s_logisimNet56  <=  '1';


   -- Constant
    s_logisimBus55(2 DOWNTO 0)  <=  "111";


   -- Constant
    s_logisimBus54(15 DOWNTO 0)  <=  X"0003";


   -- Bit Extender
    s_logisimBus29(0)  <=  s_logisimBus6(0);
    s_logisimBus29(1)  <=  s_logisimBus6(1);
    s_logisimBus29(2)  <=  s_logisimBus6(2);
    s_logisimBus29(3)  <=  '0';
    s_logisimBus29(4)  <=  '0';
    s_logisimBus29(5)  <=  '0';
    s_logisimBus29(6)  <=  '0';
    s_logisimBus29(7)  <=  '0';
    s_logisimBus29(8)  <=  '0';
    s_logisimBus29(9)  <=  '0';
    s_logisimBus29(10)  <=  '0';
    s_logisimBus29(11)  <=  '0';
    s_logisimBus29(12)  <=  '0';
    s_logisimBus29(13)  <=  '0';
    s_logisimBus29(14)  <=  '0';
    s_logisimBus29(15)  <=  '0';


   -- Constant
    s_logisimBus27(13 DOWNTO 0)  <=  "11"&X"FFF";


   -- Constant
    s_logisimNet51  <=  '0';


   -- Constant
    s_logisimNet31  <=  '1';


   -- Constant
    s_logisimBus53(15 DOWNTO 0)  <=  X"0000";


   -- NOT Gate
   s_logisimNet45 <=  NOT s_logisimNet4;

   -- ROM: ROM_1
   WITH (s_logisimBus8) SELECT s_logisimBus38 <=
      X"008F" WHEN "00"&X"000",
      X"008F" WHEN "00"&X"001",
      X"0003" WHEN "00"&X"002",
      X"008E" WHEN "00"&X"003",
      X"0089" WHEN "00"&X"004",
      X"0006" WHEN "00"&X"005",
      X"0089" WHEN "00"&X"006",
      X"008F" WHEN "00"&X"007",
      X"0009" WHEN "00"&X"008",
      X"0089" WHEN "00"&X"009",
      X"0089" WHEN "00"&X"00A",
      X"000C" WHEN "00"&X"00B",
      X"0013" WHEN "00"&X"00C",
      X"0013" WHEN "00"&X"00D",
      X"000F" WHEN "00"&X"00E",
      X"0012" WHEN "00"&X"00F",
      X"0012" WHEN "00"&X"010",
      X"0014" WHEN "00"&X"011",
      X"008E" WHEN "00"&X"014",
      X"0089" WHEN "00"&X"015",
      X"0017" WHEN "00"&X"016",
      X"0089" WHEN "00"&X"017",
      X"0012" WHEN "00"&X"018",
      X"0029" WHEN "00"&X"019",
      X"0089" WHEN "00"&X"01A",
      X"0089" WHEN "00"&X"01B",
      X"001D" WHEN "00"&X"01C",
      X"008A" WHEN "00"&X"01D",
      X"0012" WHEN "00"&X"01E",
      X"0020" WHEN "00"&X"01F",
      X"008F" WHEN "00"&X"020",
      X"0089" WHEN "00"&X"021",
      X"0023" WHEN "00"&X"022",
      X"0089" WHEN "00"&X"023",
      X"0013" WHEN "00"&X"024",
      X"0026" WHEN "00"&X"025",
      X"0089" WHEN "00"&X"026",
      X"0089" WHEN "00"&X"027",
      X"0017" WHEN "00"&X"028",
      X"008F" WHEN "00"&X"029",
      X"008F" WHEN "00"&X"02A",
      X"002C" WHEN "00"&X"02B",
      X"0013" WHEN "00"&X"02C",
      X"0089" WHEN "00"&X"02D",
      X"002F" WHEN "00"&X"02E",
      X"0089" WHEN "00"&X"02F",
      X"008F" WHEN "00"&X"030",
      X"0032" WHEN "00"&X"031",
      X"0089" WHEN "00"&X"032",
      X"0089" WHEN "00"&X"033",
      X"0035" WHEN "00"&X"034",
      X"0090" WHEN "00"&X"035",
      X"0090" WHEN "00"&X"036",
      X"0038" WHEN "00"&X"037",
      X"008D" WHEN "00"&X"038",
      X"0089" WHEN "00"&X"039",
      X"003B" WHEN "00"&X"03A",
      X"0089" WHEN "00"&X"03B",
      X"0090" WHEN "00"&X"03C",
      X"003E" WHEN "00"&X"03D",
      X"0089" WHEN "00"&X"03E",
      X"0089" WHEN "00"&X"03F",
      X"0041" WHEN "00"&X"040",
      X"008B" WHEN "00"&X"041",
      X"0090" WHEN "00"&X"042",
      X"0044" WHEN "00"&X"043",
      X"008F" WHEN "00"&X"044",
      X"0090" WHEN "00"&X"045",
      X"007A" WHEN "00"&X"046",
      X"008F" WHEN "00"&X"047",
      X"008F" WHEN "00"&X"048",
      X"004A" WHEN "00"&X"049",
      X"008D" WHEN "00"&X"04A",
      X"0089" WHEN "00"&X"04B",
      X"004D" WHEN "00"&X"04C",
      X"0089" WHEN "00"&X"04D",
      X"008F" WHEN "00"&X"04E",
      X"0050" WHEN "00"&X"04F",
      X"0089" WHEN "00"&X"050",
      X"0089" WHEN "00"&X"051",
      X"0053" WHEN "00"&X"052",
      X"008B" WHEN "00"&X"053",
      X"008F" WHEN "00"&X"054",
      X"0056" WHEN "00"&X"055",
      X"008E" WHEN "00"&X"056",
      X"008F" WHEN "00"&X"057",
      X"005C" WHEN "00"&X"058",
      X"0089" WHEN "00"&X"059",
      X"0089" WHEN "00"&X"05A",
      X"0056" WHEN "00"&X"05B",
      X"008E" WHEN "00"&X"05C",
      X"0089" WHEN "00"&X"05D",
      X"005F" WHEN "00"&X"05E",
      X"0089" WHEN "00"&X"05F",
      X"008F" WHEN "00"&X"060",
      X"0062" WHEN "00"&X"061",
      X"0089" WHEN "00"&X"062",
      X"0089" WHEN "00"&X"063",
      X"0065" WHEN "00"&X"064",
      X"008A" WHEN "00"&X"065",
      X"008F" WHEN "00"&X"066",
      X"0068" WHEN "00"&X"067",
      X"0090" WHEN "00"&X"068",
      X"0090" WHEN "00"&X"069",
      X"006B" WHEN "00"&X"06A",
      X"008F" WHEN "00"&X"06B",
      X"0090" WHEN "00"&X"06C",
      X"006E" WHEN "00"&X"06D",
      X"008F" WHEN "00"&X"06E",
      X"008F" WHEN "00"&X"06F",
      X"0071" WHEN "00"&X"070",
      X"0090" WHEN "00"&X"071",
      X"008F" WHEN "00"&X"072",
      X"007D" WHEN "00"&X"073",
      X"008B" WHEN "00"&X"074",
      X"008E" WHEN "00"&X"075",
      X"0077" WHEN "00"&X"076",
      X"0089" WHEN "00"&X"077",
      X"0089" WHEN "00"&X"078",
      X"008D" WHEN "00"&X"07A",
      X"FFFF" WHEN "00"&X"07B",
      X"007D" WHEN "00"&X"07C",
      X"008B" WHEN "00"&X"07D",
      X"008D" WHEN "00"&X"07E",
      X"0080" WHEN "00"&X"07F",
      X"008E" WHEN "00"&X"080",
      X"008E" WHEN "00"&X"081",
      X"0083" WHEN "00"&X"082",
      X"008C" WHEN "00"&X"083",
      X"008E" WHEN "00"&X"084",
      X"0086" WHEN "00"&X"085",
      X"0089" WHEN "00"&X"086",
      X"0089" WHEN "00"&X"087",
      X"0001" WHEN "00"&X"08A",
      X"FFFF" WHEN "00"&X"08B",
      X"FFFE" WHEN "00"&X"08C",
      X"0002" WHEN "00"&X"08D",
      X"0002" WHEN "00"&X"08E",
      X"0000" WHEN OTHERS;

   -- Hex Digit Display: Hex_Digit_Display_1
   WITH (s_logisimBus33(15 DOWNTO 12)) SELECT logisimOutputBubbles(6 DOWNTO 0) <=
      "011"&X"F" WHEN X"0",
      "000"&X"6" WHEN X"1",
      "101"&X"B" WHEN X"2",
      "100"&X"F" WHEN X"3",
      "110"&X"6" WHEN X"4",
      "110"&X"D" WHEN X"5",
      "111"&X"D" WHEN X"6",
      "000"&X"7" WHEN X"7",
      "111"&X"F" WHEN X"8",
      "110"&X"7" WHEN X"9",
      "111"&X"7" WHEN X"A",
      "111"&X"C" WHEN X"B",
      "011"&X"9" WHEN X"C",
      "101"&X"E" WHEN X"D",
      "111"&X"9" WHEN X"E",
      "111"&X"1" WHEN OTHERS;
   logisimOutputBubbles(7) <= s_logisimNet20;

   -- Hex Digit Display: Hex_Digit_Display_2
   WITH (s_logisimBus33(11 DOWNTO 8)) SELECT logisimOutputBubbles(14 DOWNTO 8) <=
      "011"&X"F" WHEN X"0",
      "000"&X"6" WHEN X"1",
      "101"&X"B" WHEN X"2",
      "100"&X"F" WHEN X"3",
      "110"&X"6" WHEN X"4",
      "110"&X"D" WHEN X"5",
      "111"&X"D" WHEN X"6",
      "000"&X"7" WHEN X"7",
      "111"&X"F" WHEN X"8",
      "110"&X"7" WHEN X"9",
      "111"&X"7" WHEN X"A",
      "111"&X"C" WHEN X"B",
      "011"&X"9" WHEN X"C",
      "101"&X"E" WHEN X"D",
      "111"&X"9" WHEN X"E",
      "111"&X"1" WHEN OTHERS;
   logisimOutputBubbles(15) <= s_logisimNet20;

   -- Hex Digit Display: Hex_Digit_Display_3
   WITH (s_logisimBus33(7 DOWNTO 4)) SELECT logisimOutputBubbles(22 DOWNTO 16) <=
      "011"&X"F" WHEN X"0",
      "000"&X"6" WHEN X"1",
      "101"&X"B" WHEN X"2",
      "100"&X"F" WHEN X"3",
      "110"&X"6" WHEN X"4",
      "110"&X"D" WHEN X"5",
      "111"&X"D" WHEN X"6",
      "000"&X"7" WHEN X"7",
      "111"&X"F" WHEN X"8",
      "110"&X"7" WHEN X"9",
      "111"&X"7" WHEN X"A",
      "111"&X"C" WHEN X"B",
      "011"&X"9" WHEN X"C",
      "101"&X"E" WHEN X"D",
      "111"&X"9" WHEN X"E",
      "111"&X"1" WHEN OTHERS;
   logisimOutputBubbles(23) <= s_logisimNet20;

   -- Hex Digit Display: Hex_Digit_Display_4
   WITH (s_logisimBus33(3 DOWNTO 0)) SELECT logisimOutputBubbles(30 DOWNTO 24) <=
      "011"&X"F" WHEN X"0",
      "000"&X"6" WHEN X"1",
      "101"&X"B" WHEN X"2",
      "100"&X"F" WHEN X"3",
      "110"&X"6" WHEN X"4",
      "110"&X"D" WHEN X"5",
      "111"&X"D" WHEN X"6",
      "000"&X"7" WHEN X"7",
      "111"&X"F" WHEN X"8",
      "110"&X"7" WHEN X"9",
      "111"&X"7" WHEN X"A",
      "111"&X"C" WHEN X"B",
      "011"&X"9" WHEN X"C",
      "101"&X"E" WHEN X"D",
      "111"&X"9" WHEN X"E",
      "111"&X"1" WHEN OTHERS;
   logisimOutputBubbles(31) <= s_logisimNet20;

   -- Button: RESET
   s_logisimNet19 <= logisimInputBubbles(0);

   -- LED: LED_1
   logisimOutputBubbles(32) <= s_logisimNet35;

   -- LED: LED_2
   logisimOutputBubbles(33) <= s_logisimNet25;

   -- LED: LED_3
   logisimOutputBubbles(34) <= s_logisimNet10;

   -- LED: LED_4
   logisimOutputBubbles(35) <= s_logisimNet43;

   -- LED: LED_5
   logisimOutputBubbles(36) <= s_logisimNet36;

   -- LED: LED_6
   logisimOutputBubbles(37) <= s_logisimNet26;

   -- LED: LED_7
   logisimOutputBubbles(38) <= s_logisimNet28;

   -- LED: LED_8
   logisimOutputBubbles(39) <= s_logisimNet44;

   --------------------------------------------------------------------------------
   -- Here all normal components are defined                                     --
   --------------------------------------------------------------------------------
   GATES_1 : AND_GATE
      GENERIC MAP ( BubblesMask => "00" )
      PORT MAP ( input1 => s_logisimNet15,
                 input2 => s_logisimNet16,
                 result => s_logisimNet50 );

   GATES_2 : OR_GATE
      GENERIC MAP ( BubblesMask => "10" )
      PORT MAP ( input1 => s_logisimNet4,
                 input2 => s_logisimNet22,
                 result => s_logisimNet48 );

   PLEXERS_3 : Multiplexer_bus_2
      GENERIC MAP ( nrOfBits => 16 )
      PORT MAP ( enable  => '1',
                 muxIn_0 => s_logisimBus21(15 DOWNTO 0),
                 muxIn_1 => s_logisimBus13(15 DOWNTO 0),
                 muxOut  => s_logisimBus49(15 DOWNTO 0),
                 sel     => s_logisimNet30 );

   PLEXERS_4 : Multiplexer_bus_8
      GENERIC MAP ( nrOfBits => 16 )
      PORT MAP ( enable  => '1',
                 muxIn_0 => s_logisimBus5(15 DOWNTO 0),
                 muxIn_1 => s_logisimBus5(15 DOWNTO 0),
                 muxIn_2 => s_logisimBus5(15 DOWNTO 0),
                 muxIn_3 => s_logisimBus32(15 DOWNTO 0),
                 muxIn_4 => s_logisimBus14(15 DOWNTO 0),
                 muxIn_5 => s_logisimBus14(15 DOWNTO 0),
                 muxIn_6 => s_logisimBus14(15 DOWNTO 0),
                 muxIn_7 => X"0000",
                 muxOut  => s_logisimBus46(15 DOWNTO 0),
                 sel     => s_logisimBus6(2 DOWNTO 0) );

   PLEXERS_5 : Multiplexer_bus_2
      GENERIC MAP ( nrOfBits => 14 )
      PORT MAP ( enable  => '1',
                 muxIn_0 => s_logisimBus8(13 DOWNTO 0),
                 muxIn_1 => s_logisimBus46(13 DOWNTO 0),
                 muxOut  => s_logisimBus39(13 DOWNTO 0),
                 sel     => s_logisimNet4 );

   PLEXERS_6 : Multiplexer_bus_2
      GENERIC MAP ( nrOfBits => 16 )
      PORT MAP ( enable  => '1',
                 muxIn_0 => s_logisimBus38(15 DOWNTO 0),
                 muxIn_1 => s_logisimBus12(15 DOWNTO 0),
                 muxOut  => s_logisimBus23(15 DOWNTO 0),
                 sel     => s_logisimNet4 );

   PLEXERS_7 : Decoder_8
      PORT MAP ( decoderOut_0 => s_logisimNet35,
                 decoderOut_1 => s_logisimNet25,
                 decoderOut_2 => s_logisimNet10,
                 decoderOut_3 => s_logisimNet43,
                 decoderOut_4 => s_logisimNet36,
                 decoderOut_5 => s_logisimNet26,
                 decoderOut_6 => s_logisimNet28,
                 decoderOut_7 => s_logisimNet44,
                 enable       => '1',
                 sel          => s_logisimBus6(2 DOWNTO 0) );

   PLEXERS_8 : Decoder_8
      PORT MAP ( decoderOut_0 => OPEN,
                 decoderOut_1 => s_logisimNet47,
                 decoderOut_2 => s_logisimNet0,
                 decoderOut_3 => s_logisimNet11,
                 decoderOut_4 => s_logisimNet9,
                 decoderOut_5 => s_logisimNet1,
                 decoderOut_6 => s_logisimNet22,
                 decoderOut_7 => s_logisimNet16,
                 enable       => '1',
                 sel          => s_logisimBus6(2 DOWNTO 0) );

   ARITH_9 : Negator
      GENERIC MAP ( nrOfBits => 16 )
      PORT MAP ( dataX    => s_logisimBus24(15 DOWNTO 0),
                 minDataX => s_logisimBus41(15 DOWNTO 0) );

   ARITH_10 : Comparator
      GENERIC MAP ( nrOfBits       => 16,
                    twosComplement => 1 )
      PORT MAP ( aEqualsB      => s_logisimNet15,
                 aGreaterThanB => OPEN,
                 aLessThanB    => OPEN,
                 dataA         => s_logisimBus14(15 DOWNTO 0),
                 dataB         => s_logisimBus52(15 DOWNTO 0) );

   ARITH_11 : Comparator
      GENERIC MAP ( nrOfBits       => 16,
                    twosComplement => 1 )
      PORT MAP ( aEqualsB      => OPEN,
                 aGreaterThanB => s_logisimNet30,
                 aLessThanB    => OPEN,
                 dataA         => s_logisimBus12(15 DOWNTO 0),
                 dataB         => s_logisimBus53(15 DOWNTO 0) );

   ARITH_12 : Adder
      GENERIC MAP ( extendedBits => 17,
                    nrOfBits     => 16 )
      PORT MAP ( carryIn  => '0',
                 carryOut => OPEN,
                 dataA    => s_logisimBus41(15 DOWNTO 0),
                 dataB    => s_logisimBus40(15 DOWNTO 0),
                 result   => s_logisimBus12(15 DOWNTO 0) );

   ARITH_13 : Adder
      GENERIC MAP ( extendedBits => 17,
                    nrOfBits     => 16 )
      PORT MAP ( carryIn  => '0',
                 carryOut => OPEN,
                 dataA    => s_logisimBus17(15 DOWNTO 0),
                 dataB    => s_logisimBus54(15 DOWNTO 0),
                 result   => s_logisimBus13(15 DOWNTO 0) );

   ARITH_14 : Adder
      GENERIC MAP ( extendedBits => 17,
                    nrOfBits     => 16 )
      PORT MAP ( carryIn  => '0',
                 carryOut => OPEN,
                 dataA    => s_logisimBus17(15 DOWNTO 0),
                 dataB    => s_logisimBus29(15 DOWNTO 0),
                 result   => s_logisimBus5(15 DOWNTO 0) );

   SubLoc : REGISTER_FLIP_FLOP
      GENERIC MAP ( invertClock => 1,
                    nrOfBits    => 16 )
      PORT MAP ( clock       => logisimClockTree0(4),
                 clockEnable => s_logisimNet47,
                 d           => s_logisimBus2(15 DOWNTO 0),
                 q           => s_logisimBus32(15 DOWNTO 0),
                 reset       => s_logisimNet19,
                 tick        => logisimClockTree0(3) );

   MinLoc : REGISTER_FLIP_FLOP
      GENERIC MAP ( invertClock => 1,
                    nrOfBits    => 16 )
      PORT MAP ( clock       => logisimClockTree0(4),
                 clockEnable => s_logisimNet0,
                 d           => s_logisimBus2(15 DOWNTO 0),
                 q           => s_logisimBus14(15 DOWNTO 0),
                 reset       => s_logisimNet19,
                 tick        => logisimClockTree0(3) );

   Adr : REGISTER_FLIP_FLOP
      GENERIC MAP ( invertClock => 1,
                    nrOfBits    => 16 )
      PORT MAP ( clock       => logisimClockTree0(4),
                 clockEnable => s_logisimNet11,
                 d           => s_logisimBus2(15 DOWNTO 0),
                 q           => s_logisimBus21(15 DOWNTO 0),
                 reset       => s_logisimNet19,
                 tick        => logisimClockTree0(3) );

   Sub : REGISTER_FLIP_FLOP
      GENERIC MAP ( invertClock => 1,
                    nrOfBits    => 16 )
      PORT MAP ( clock       => logisimClockTree0(4),
                 clockEnable => s_logisimNet9,
                 d           => s_logisimBus2(15 DOWNTO 0),
                 q           => s_logisimBus24(15 DOWNTO 0),
                 reset       => s_logisimNet19,
                 tick        => logisimClockTree0(3) );

   Min : REGISTER_FLIP_FLOP
      GENERIC MAP ( invertClock => 1,
                    nrOfBits    => 16 )
      PORT MAP ( clock       => logisimClockTree0(4),
                 clockEnable => s_logisimNet1,
                 d           => s_logisimBus2(15 DOWNTO 0),
                 q           => s_logisimBus40(15 DOWNTO 0),
                 reset       => s_logisimNet19,
                 tick        => logisimClockTree0(3) );

   MEMORY_20 : REGISTER_FLIP_FLOP
      GENERIC MAP ( invertClock => 1,
                    nrOfBits    => 16 )
      PORT MAP ( clock       => logisimClockTree0(4),
                 clockEnable => s_logisimNet50,
                 d           => s_logisimBus24(15 DOWNTO 0),
                 q           => s_logisimBus33(15 DOWNTO 0),
                 reset       => s_logisimNet19,
                 tick        => logisimClockTree0(3) );

   PC : REGISTER_FLIP_FLOP
      GENERIC MAP ( invertClock => 0,
                    nrOfBits    => 16 )
      PORT MAP ( clock       => logisimClockTree0(4),
                 clockEnable => s_logisimNet16,
                 d           => s_logisimBus49(15 DOWNTO 0),
                 q           => s_logisimBus17(15 DOWNTO 0),
                 reset       => s_logisimNet19,
                 tick        => logisimClockTree0(2) );

   MEMORY_22 : LogisimCounter
      GENERIC MAP ( invertClock => 0,
                    maxVal      => "111",
                    mode        => 0,
                    width       => 3 )
      PORT MAP ( clear      => s_logisimNet19,
                 clock      => logisimClockTree0(4),
                 compareOut => OPEN,
                 countValue => s_logisimBus6(2 DOWNTO 0),
                 enable     => s_logisimNet4,
                 load       => s_logisimNet45,
                 loadData   => s_logisimBus55(2 DOWNTO 0),
                 tick       => logisimClockTree0(2),
                 upNotDown  => s_logisimNet56 );

   RAM_1 : RAMCONTENTS_RAM_1
      PORT MAP ( address => s_logisimBus39(13 DOWNTO 0),
                 clock   => logisimClockTree0(4),
                 dataIn  => s_logisimBus23(15 DOWNTO 0),
                 dataOut => s_logisimBus2(15 DOWNTO 0),
                 oe      => '1',
                 tick    => logisimClockTree0(2),
                 we      => s_logisimNet48 );

   MEMORY_24 : LogisimCounter
      GENERIC MAP ( invertClock => 0,
                    maxVal      => "11"&X"FFF",
                    mode        => 1,
                    width       => 14 )
      PORT MAP ( clear      => s_logisimNet19,
                 clock      => logisimClockTree0(4),
                 compareOut => s_logisimNet4,
                 countValue => s_logisimBus8(13 DOWNTO 0),
                 enable     => s_logisimNet45,
                 load       => s_logisimNet51,
                 loadData   => s_logisimBus27(13 DOWNTO 0),
                 tick       => logisimClockTree0(2),
                 upNotDown  => s_logisimNet31 );


END platformIndependent;
