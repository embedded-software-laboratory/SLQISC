module vga_lcd(
   input clk_25,
   input clk_50,
	input addInput,
	input [6:0] charCode,
	input reset,
	
	output		     [3:0]		VGA_R,
   output           [3:0]		VGA_B,
	output		     [3:0]		VGA_G,
	output		          		VGA_HS,
	output		          		VGA_VS );
   
	localparam W = 640;
	localparam H = 480;
	
   wire [3:0] buffer_r;
   wire [3:0] buffer_g;
   wire [3:0] buffer_b;
  
   wire [8:0] curRow;
   wire [9:0] curCol;
	
	wire [8:0] charRow;
	wire [9:0] charCol;
	
	wire [8:0] inCharRow;
	wire [9:0] inCharCol;
	
	wire [49:0] pixelGrid;
	wire [7:0] pixelRow;
	wire superpixel;
	
	reg [6:0] characters [15:0];
	
	reg [3:0] freeX = 0;
	reg [3:0] freeXN = 0;
	
	/*assign charRow = curRow >> 5;
	assign charCol = curCol >> 5;
	assign inCharRow = (curRow-charRow << 5) >> 3;
	assign inCharCol = (curCol-(charCol+1) << 5)>>3;
	assign pixelGrid = {charMap(characters[charRow][charCol]), 5'b0};
	assign pixelRow = {pixelGrid[inCharRow*5 +: 5], 1'b0};
	assign superpixel = curCol < 36*17 && curCol >= 36 ? pixelRow[inCharCol] : 0;
   assign buffer_r = superpixel ? 15 : 0;
   assign buffer_g = superpixel ? 15 : 0;
   assign buffer_b = superpixel ? 15 : 0;*/
	
	/*assign charRow = curRow/60;
	assign charCol = curCol/36 - 1;
	assign inCharRow = (curRow-charRow*60)/6;
	assign inCharCol = (curCol-(charCol+1)*36)/6;
	assign pixelGrid = {charMap(characters[charRow][charCol]), 5'b0};
	assign pixelRow = {pixelGrid[inCharRow*5 +: 5], 1'b0};
	assign superpixel = curCol < 36*17 && curCol >= 36 ? pixelRow[inCharCol] : 0;
   assign buffer_r = superpixel ? 15 : 0;
   assign buffer_g = superpixel ? 15 : 0;
   assign buffer_b = superpixel ? 15 : 0;*/
	
	assign charCol = curCol >> 5;
	assign inCharRow = (curRow-200) >> 2;
	assign inCharCol = (curCol & 9'h1f) >> 2;
	assign pixelGrid = {charMap(characters[charCol-2]), 5'b0};
	assign pixelRow = {pixelGrid[inCharRow*5 +: 5], 3'b0};
	assign superpixel = pixelRow[inCharCol] && 200 <= curRow && curRow < 240 && 64 <= curCol && curCol < 576;
   assign buffer_r = superpixel ? 15 : 0;
   assign buffer_g = superpixel ? 15 : 0;
   assign buffer_b = superpixel ? 15 : 0;
	
//" ABCDEFGHIJKLMNOPQRSTUVWXYZ!0123456789abcdefghijklmnopqrstuvwxyz+-"
function [44:0] charMap (input[6:0] chr);
  begin
  case(chr)
    00: charMap=45'b0;
	 01: charMap=45'b10001_10001_10001_10001_11111_10001_10001_10001_11111;
	 02: charMap=45'b01111_10001_10001_10001_01111_10001_10001_10001_01111;
	 03: charMap=45'b11110_00001_00001_00001_00001_00001_00001_00001_11110;
	 04: charMap=45'b01111_10001_10001_10001_10001_10001_10001_10001_01111;
	 05: charMap=45'b11111_00001_00001_00001_00111_00001_00001_00001_11111;
	 06: charMap=45'b00001_00001_00001_00001_00111_00001_00001_00001_11111;
	 07: charMap=45'b01110_10001_10001_10001_01101_00001_10001_10001_01110;
	 08: charMap=45'b10001_10001_10001_10001_11111_10001_10001_10001_10001;
	 09: charMap=45'b00100_00100_00100_00100_00100_00100_00100_00100_00100;
	 12: charMap=45'b11111_00001_00001_00001_00001_00001_00001_00001_00001;
	 14: charMap=45'b10001_11001_11001_10101_10101_10101_10011_10011_10001;
	 15: charMap=45'b01110_10001_10001_10001_10001_10001_10001_10001_01110;
	 18: charMap=45'b10001_01001_00101_00011_01111_10001_10001_10001_01111;
	 19: charMap=45'b01110_10001_10000_01000_00100_00010_00001_10001_01110;
	 20: charMap=45'b00100_00100_00100_00100_00100_00100_00100_00100_11111;
	 21: charMap=45'b01110_10001_10001_10001_10001_10001_10001_10001_10001;
	 22: charMap=45'b00100_01010_10001_10001_10001_10001_10001_10001_10001;
	 23: charMap=45'b10001_11011_10101_10101_10101_10001_10001_10001_10001;
	 25: charMap=45'b00100_00100_00100_00100_00100_01010_01010_10001_10001;
	 27: charMap=45'b00100_00000_00000_00100_00100_00100_00100_00100_00100;
	 28: charMap=45'b01110_10001_10001_10101_10101_10101_10001_10001_01110;
	 29: charMap=45'b10000_10000_10000_10000_10001_10010_10100_11000_10000;
	 30: charMap=45'b11111_00010_00100_01000_10000_10001_10001_10001_01110;
	 31: charMap=45'b01110_10001_10000_10000_01110_10000_10000_10001_01110;
	 32: charMap=45'b00100_00100_00100_00100_11111_00101_00101_00101_00101;
	 33: charMap=45'b01110_10001_10000_10000_01111_00001_00001_00001_11111;
	 34: charMap=45'b01110_10001_10001_10001_01111_00001_00001_00001_01110;
	 35: charMap=45'b00100_00100_00100_00100_00100_01000_01000_10000_11111;
	 36: charMap=45'b01110_10001_10001_10001_01110_10001_10001_10001_01110;
	 37: charMap=45'b01110_10001_10000_10000_11110_10001_10001_10001_01110;
    default: charMap=45'b11111_11111_11111_11111_11111_11111_11111_11111_11111;
  endcase
  end
endfunction

always @*
begin
  freeX <= freeXN;
end

reg processed = 0;

always @(posedge clk_50)
begin
  if(reset) begin
     processed <= 0;
	  freeXN <= 0;
	  characters[0] <= 0;
	  characters[1] <= 0;
	  characters[2] <= 0;
	  characters[3] <= 0;
	  characters[4] <= 0;
	  characters[5] <= 0;
	  characters[6] <= 0;
	  characters[7] <= 0;
	  characters[8] <= 0;
	  characters[9] <= 0;
	  characters[10] <= 0;
	  characters[11] <= 0;
	  characters[12] <= 0;
	  characters[13] <= 0;
	  characters[14] <= 0;
	  characters[15] <= 0;
  end else begin
	  if(addInput && !processed) begin
		 characters[freeX] <= charCode;
		 freeXN <= freeX + 1;
		 processed <= 1;
	  end
	  if (!addInput)
		 processed <= 0;
  end
end

  
vga_controller controller(
  .buffer_r(buffer_r),
  .buffer_g(buffer_g),
  .buffer_b(buffer_b),
  .clk_25(clk_25),
  .currentRow(curRow),
  .currentCol(curCol),
  .VGA_R(VGA_R),
  .VGA_G(VGA_G),
  .VGA_B(VGA_B),
  .VGA_HS(VGA_HS),
  .VGA_VS(VGA_VS)
);

endmodule