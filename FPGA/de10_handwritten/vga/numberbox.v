module numberbox
#(
    parameter SYMBOLS = 4,
	 parameter TOP = 0,
	 parameter LEFT = 0
)
(
  input [8:0] curRow,
  input [9:0] curCol,
  input [4*SYMBOLS-1:0] number,
  output pixel
);

wire [7*SYMBOLS-1:0] characters;

genvar i;
generate
for (i = 0; i < SYMBOLS ; i = i + 1) begin : charassigner
    assign characters[7*i +: 7] = digitToIndex(number[4*(SYMBOLS-i-1) +: 4]);
end
endgenerate


vgabox #(.SYMBOLS(SYMBOLS), .TOP(TOP), .LEFT(LEFT)) box(
  .curRow(curRow),
  .curCol(curCol),
  .characters(characters),
  .pixel(pixel)
);

function [6:0] digitToIndex(input[3:0] digit);
  begin
    if (digit <= 9) begin
	   digitToIndex = {3'b0,digit}+28;
	 end else begin
	   digitToIndex = {3'b0,digit}-9;
	 end
  end
endfunction

endmodule