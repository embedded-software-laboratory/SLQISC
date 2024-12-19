module program_rom(
  input clk,
  input [15:0] address,
  output [15:0] instruction
);

rom rom_hard(
  .address(address),
  .clock(clk),
  .q(instruction)
);

endmodule