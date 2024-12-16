module vga_controller(
   input [3:0] buffer_r,
   input [3:0] buffer_g,
   input [3:0] buffer_b,
   input clk_25,	
	
	output           [8:0]     currentRow,
	output           [9:0]     currentCol,
	
	output		     [3:0]		VGA_R,
	output		     [3:0]		VGA_G,
   output           [3:0]		VGA_B,
	output		          		VGA_HS,
	output		          		VGA_VS );
		
	reg[9:0] HCycle = 0;
	reg[9:0] VCycle = 0;
	reg[9:0] HCycleN = 0;
	reg[9:0] VCycleN = 0;
	
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
	
	wire [31:0] Hdiff;
	assign Hdiff = HCycle - HOffset;
	assign currentCol = Hdiff[9:0];
	
	wire [31:0] Vdiff;
	assign Vdiff = VCycle - VOffset;
	assign currentRow = Vdiff[8:0];
	
	always @*
	begin
	  HCycle <= HCycleN;
	  VCycle <= VCycleN;
	end
	
	always @(posedge clk_25)
	begin
	    oHS <= ~(HCycle < HSync);
	    oVS <= ~(VCycle < VSync);
		 if (HOffset <= HCycle && HCycle < HOffset + H
		  && VOffset <= VCycle && VCycle < VOffset + V ) begin
		   oR <= buffer_r;
		   oG <= buffer_g;
		   oB <= buffer_b;
		 end else begin
		   oR <= 0;
			oG <= 0;
			oB <= 0;
		 end
		 if (HCycle + 1 == HCnt)
		 begin
		   HCycleN <= 0;
			 if (VCycle + 1 == VCnt)
			   VCycleN <= 0;
			 else
			   VCycleN <= VCycle + 10'd1;
		 end else begin
		   HCycleN <= HCycle + 10'd1;
		 end
	end
	
	
endmodule
