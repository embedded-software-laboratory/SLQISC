module vga_img(
   input clk_25,
   input clk_50,
	input addInput,
	input [11:0] rgbCode,
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
	
	localparam PW = 40;
	localparam PH = 30;
	localparam CH = 2;
	localparam PIX = CH*3;
	
	reg [PW*PH*PIX-1:0] pixmap;
	
	reg [10:0] freeX = 0;
	reg [10:0] freeXN = 0;
	
	wire [4:0] mapRow = curRow[8:4];
	wire [5:0] mapCol = curCol[9:4];
	
   assign buffer_r = pixmap[mapRow*PW*PIX+mapCol*PIX   +: CH] << (4-CH);
   assign buffer_g = pixmap[mapRow*PW*PIX+mapCol*PIX+CH +: CH] << (4-CH);
   assign buffer_b = pixmap[mapRow*PW*PIX+mapCol*PIX+CH*2 +: CH] << (4-CH);

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
	  pixmap <= 0;
  end else begin
	  if(addInput && !processed) begin
		 pixmap[freeX*PIX +: CH] <= rgbCode[11 -: CH];
		 pixmap[freeX*PIX+CH +: CH] <= rgbCode[7 -: CH];
		 pixmap[freeX*PIX+2*CH +: CH] <= rgbCode[3 -: CH];
		 freeXN <= freeX + 11'b1 >= PW*PH ? 11'b0 : freeX + 11'b1;
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
