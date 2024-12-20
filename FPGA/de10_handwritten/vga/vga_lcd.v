module vga_lcd(
   input clk_25,
   input clk_50,
	input addInput,
	input [6:0] charCode,
	input reset,
	input [7:0] inval,
	input [15:0] pcval,
	input [47:0] instr,
	input [31:0] refs,
	input debug,
	
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
	
	wire outpix;
	wire pcpix;
	wire inpix;
	wire instrpix;
	wire refpix;
	
	reg [7*16-1:0] characters;
	
	reg [3:0] freeX = 0;
	reg [3:0] freeXN = 0;
	
	wire runpix = outpix || inpix;
	wire debpix = outpix || pcpix || inpix || instrpix || refpix ;
	wire acpix = debug ? debpix : runpix;
	
   assign buffer_r = acpix ? 4'd15 : 4'd0;
   assign buffer_g = acpix ? 4'd15 : 4'd0;
   assign buffer_b = acpix ? 4'd15 : 4'd0;

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
	  characters <= 0;
  end else begin
	  if(addInput && !processed) begin
		 characters[freeX*7 +: 7] <= charCode;
		 freeXN <= freeX + 4'b1;
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

vgabox #(.SYMBOLS(16), .TOP(300), .LEFT(72)) outbox(
  .curRow(curRow),
  .curCol(curCol),
  .characters(characters),
  .pixel(outpix)
);

numberbox #(.SYMBOLS(4), .TOP(100), .LEFT(72)) pcbox(
  .curRow(curRow),
  .curCol(curCol),
  .number(pcval),
  .pixel(pcpix)
);

numberbox #(.SYMBOLS(2), .TOP(200), .LEFT(72)) inbox(
  .curRow(curRow),
  .curCol(curCol),
  .number(inval),
  .pixel(inpix)
);

numberbox #(.SYMBOLS(12), .TOP(178), .LEFT(150)) instrbox(
  .curRow(curRow),
  .curCol(curCol),
  .number(instr),
  .pixel(instrpix)
);
numberbox #(.SYMBOLS(8), .TOP(222), .LEFT(150)) refbox(
  .curRow(curRow),
  .curCol(curCol),
  .number(refs),
  .pixel(refpix)
);

endmodule