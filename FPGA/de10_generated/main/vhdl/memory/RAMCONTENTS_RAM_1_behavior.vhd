--==============================================================================
--== Logisim-evolution goes FPGA automatic generated VHDL code                ==
--== https://github.com/logisim-evolution/                                    ==
--==                                                                          ==
--==                                                                          ==
--== Project   : slqisc_flash_de0                                             ==
--== Component : RAMCONTENTS_RAM_1                                            ==
--==                                                                          ==
--==============================================================================

ARCHITECTURE platformIndependent OF RAMCONTENTS_RAM_1 IS 

--------------------------------------------------------------------------------
-- Here all private types are defined                                         --
--------------------------------------------------------------------------------
   TYPE memoryArray IS ARRAY ( 16384 DOWNTO 0 ) OF std_logic_vector( 15 DOWNTO 0);

--------------------------------------------------------------------------------
-- All used signals are defined here                                          --
--------------------------------------------------------------------------------
   SIGNAL s_addressReg    : std_logic_vector( 13 DOWNTO 0 );
   SIGNAL s_dataInReg     : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_dataOutReg    : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_memContents   : memoryArray;
   SIGNAL s_oe            : std_logic;
   SIGNAL s_oeReg         : std_logic;
   SIGNAL s_ramdataOut    : std_logic_vector( 15 DOWNTO 0 );
   SIGNAL s_tickDelayLine : std_logic_vector( 2 DOWNTO 0 );
   SIGNAL s_we            : std_logic;
   SIGNAL s_weReg         : std_logic;

BEGIN

   --------------------------------------------------------------------------------
   -- The control signals are defined here                                       --
   --------------------------------------------------------------------------------
   s_oe <= s_tickDelayLine(2) AND s_oeReg;
   s_we <= s_tickDelayLine(0) AND s_weReg;

   --------------------------------------------------------------------------------
   -- The input registers are defined here                                       --
   --------------------------------------------------------------------------------
   inputRegs : PROCESS(clock, tick, address, dataIn, we, oe) IS
   BEGIN
      IF (rising_edge(clock)) THEN
         IF (tick = '1') THEN
             s_dataInReg  <= dataIn;
             s_addressReg <= address;
             s_weReg      <= we;
             s_oeReg      <= oe;
         END IF;
      END IF;
   END PROCESS inputRegs;

   tickPipeReg : PROCESS(clock) IS
   BEGIN
      IF (rising_edge(clock)) THEN
          s_tickDelayLine(0)          <= tick;
          s_tickDelayLine(2 DOWNTO 1) <= s_tickDelayLine(1 DOWNTO 0);
      END IF;
   END PROCESS tickPipeReg;

   --------------------------------------------------------------------------------
   -- The actual memorie(s) is(are) defined here                                 --
   --------------------------------------------------------------------------------
   mem : PROCESS(clock , s_we, s_dataInReg, s_addressReg) IS
   BEGIN
      IF (rising_edge(clock)) THEN
         IF (s_we = '1') THEN
            s_memContents(to_integer(unsigned(s_addressReg))) <= s_dataInReg;
         END IF;
         s_ramdataOut <= s_memContents(to_integer(unsigned(s_addressReg)));
      END IF;
   END PROCESS mem;

   --------------------------------------------------------------------------------
   -- The output register is defined here                                        --
   --------------------------------------------------------------------------------
   res : PROCESS(clock, s_oe, s_ramdataOut) IS
   BEGIN
      IF (rising_edge(clock)) THEN
         IF (s_oe = '1') THEN
           dataOut <= s_ramdataOut;
         END IF;
      END IF;
   END PROCESS res;

END platformIndependent;
