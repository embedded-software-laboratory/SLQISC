module vga_lcd(
   input clk_25,	
	
	
	output		     [3:0]		VGA_R,
   output           [3:0]		VGA_B,
	output		     [3:0]		VGA_G,
	output		          		VGA_HS,
	output		          		VGA_VS );
   
	localparam W = 640;
	localparam H = 480;
	
   reg [W*4:0] rowbuffer_r;
   reg [W*4:0] rowbuffer_g;
   reg [W*4:0] rowbuffer_b;
  
   wire [8:0] curRow;
	wire nextRow;
  
  
vga_controller controller(
  .rowbuffer_r(rowbuffer_r),
  .rowbuffer_g(rowbuffer_g),
  .rowbuffer_b(rowbuffer_b),
  .clk_25(clk_25),
  .currentRow(curRow),
  .requestRow(nextRow),
  .VGA_R(VGA_R),
  .VGA_G(VGA_G),
  .VGA_B(VGA_B),
  .VGA_HS(VGA_HS),
  .VGA_VS(VGA_VS)
);

endmodule