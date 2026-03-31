# Circuit

A collection of [logisim-evolution](https://github.com/logisim-evolution/logisim-evolution) project files that simulate various varieties of a Subleq-OISC.

## Important Circuits
The most importent circuits provided in this directory are:
- slqisc.circ: A basic version without input
- slqisc_in.circ: An upgraded version that supports input
- slqisc_flash: A version with a separate ROM block which initialised the RAM at execution start
- slqisc_flash_de0: A version of slqisc_flash that is suitable to be flashed on the Terasic DE0 and DE10 boards

## Circuit and logic

A brief explanation of the basic circuit `slqisc.circ`.

### Circuit

The circuit consists of a register bank (SubLoc, MinLoc, …, Min), which is used to store data for each phase of the subleq instruction. It is coordinated through a decoder by a simple counter (CTR3), which always loops after 7. A multiplexer, which is also connected to the counter, is used to determine which data source should be used as input for the RAM. There is also a register for the program counter (PC) and an adder to add the CTR3 to the PC in order to read each address sequentially. To the left, there is also a comparator, a small multiplexer, and an adder that always adds three to update the PC if there is no branch to another address.

To the right, there is a ROM block that is used as a lookup table to translate numbers into letters for the small display. Each display has a register attached to store the currently displayed value. Furthermore, there is a counter and a decoder to cycle through the individual segments of the display. The comparator between the register bank and the ROM checks whether the address to be written to is ffff, in which case the value of SUB is used as the index for the lookup table to write a letter. The actual subtraction of the subleq command happens below the comparator.

### Instruction

Subleq is an instruction with three parameters and behaves as follows:
For `subleq A B C`: `Mem[B] = Mem[B] - Mem[A]; if (Mem[B] <= 0) goto C`.
It is possible to implement every other conventional instruction using only a few subleq instructions.

Because the SUBLEQ computer is based on a single instruction, there is no need to store the instruction type. Therefore, each instruction consists only of three addresses.

### Instruction cycle

An instruction cycle contains the following seven steps.

First, the value at the current program counter is loaded into the SubLoc (Subtrahend Location) register. CTR3 gets incremented and added to the PC. Now, the next value is copied into the MinLoc (Minuend Location) register, likewise with the Adr. Now, all three arguments (A, B, C) are loaded into their corresponding registers. The comparator after the MinLoc register checks the value of the MinLoc (i.e., argument B) against the fixed address `ffff` which would be interpreted as a write to the output screen. The first three steps of the cycle are now completed.

After that, the value at the address, stored inside the SubLoc register, is loaded into the SubLoc register, likewise the value of the MinLoc register. The adder has now already performed the calculation, and the corresponding value can be written back into the RAM during the next step.

As part of the seventh step, a comparator determines if the value is less than or equal to zero. After which the PC either gets set to the address stored inside the Adr register or is incremented by three.

A full SUBLEQ cycle is now completed.
 
## Simulation

To simulate a programs `.hex` (which can be build using an [assembler](../Subleq/README.md#assembler)) load the project file into logisim evolution and right-click onto the RAM Block, to load the program (.hex) into memory. After that check if the option Auto-Tick-Enabled and Auto-Propagate under the simulate menu (top) are checked. To start the simulation go to the tab Simulate (left) and press the corresponding button.
