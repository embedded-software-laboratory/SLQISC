module vga_controller(
   input [2559:0] rowbuffer_r,
   input [2559:0] rowbuffer_g,
   input [2559:0] rowbuffer_b,
   input clk_25,	
	
	output           [8:0]     currentRow,
	output                     requestRow,
	
	output		     [3:0]		VGA_R,
	output		     [3:0]		VGA_G,
   output           [3:0]		VGA_B,
	output		          		VGA_HS,
	output		          		VGA_VS );
		
	reg[9:0] HCycle = 0;
	reg[9:0] VCycle = 0;
	
	reg		     [3:0]		oR;
	reg		     [3:0]		oG;
   reg           [3:0]		oB;
	reg		          		oHS;
	reg		          		oVS;
	
	assign VGA_R = oR;
	assign VGA_G = oG;
	assign VGA_B = oB;
	assign VGA_HS = oHS;
	assign VGA_VS = oVS;
	
	localparam H = 640;
	localparam HCnt = 800;
	localparam HSync = 96;
	localparam HOffset = 144;
	localparam V = 480;
	localparam VCnt = 525;
	localparam VSync = 2;
	localparam VOffset = 35;
	
	wire [31:0] diff;
	
	assign diff = VCycle - VOffset;
	assign currentRow = diff[8:0];
	
	always @(posedge clk_25)
	begin
	    oHS <= ~(HCycle < HSync);
	    oVS <= ~(VCycle < VSync);
		 if (HOffset <= HCycle && HCycle < HOffset + H
		  && VOffset <= VCycle && VCycle < VOffset + V ) begin
		   oR <= rowbuffer_r[4*(HCycle-HOffset) +: 4];
		   oG <= rowbuffer_g[4*(HCycle-HOffset) +: 4];
		   oB <= rowbuffer_b[4*(HCycle-HOffset) +: 4];
		 end else begin
		   oR <= 0;
			oG <= 0;
			oB <= 0;
		 end
	end
	
	
endmodule
