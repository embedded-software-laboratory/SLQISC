module program_rom(
  input [15:0] address,
  output [15:0] instruction
);


function [15:0] rom(input[15:0] address);
  begin
    case(address)
`include "programs/guess_vga.v"
	 default:  rom = 16'h0000;
	 endcase
  end
endfunction

assign instruction = rom(address);

endmodule