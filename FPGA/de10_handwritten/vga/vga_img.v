module vga_img(
   input clk_25,
   input clk_50,
	input addInput,
	input [11:0] rgbCode,
	input reset,
	input interpolate,
	
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
	
	reg [PW*PH*CH-1:0] rmap;
	reg [PW*PH*CH-1:0] gmap;
	reg [PW*PH*CH-1:0] bmap;
	
	reg [10:0] freeX = 0;
	reg [10:0] freeXN = 0;
	
	wire [4:0] mapRow = curRow[8:4];
	wire [5:0] mapCol = curCol[9:4];

	wire [3:0] w_ri;
	wire [3:0] w_gi;
	wire [3:0] w_bi;

   assign buffer_r = interpolate ? w_ri : {rmap[mapRow*PW*CH+mapCol*CH +: CH],rmap[mapRow*PW*CH+mapCol*CH +: CH]};
   assign buffer_g = interpolate ? w_gi : {gmap[mapRow*PW*CH+mapCol*CH +: CH],gmap[mapRow*PW*CH+mapCol*CH +: CH]};
   assign buffer_b = interpolate ? w_bi : {bmap[mapRow*PW*CH+mapCol*CH +: CH],bmap[mapRow*PW*CH+mapCol*CH +: CH]};

	wire [1:0] dR;
	wire [1:0] dG;
	wire [1:0] dB;
	
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
	  rmap <= 0;
	  gmap <= 0;
	  bmap <= 0;
  end else begin
	  if(addInput && !processed) begin
		 rmap[freeX*CH +: CH] <= dR;
		 gmap[freeX*CH +: CH] <= dG;
		 bmap[freeX*CH +: CH] <= dB;
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

	
wire [3:0] inRow = curRow[3:0]+4'b1000;
wire [3:0] inCol = curCol[3:0]+4'b1000;

bi_interpolator lerp_r(
	.a00(rmap[(mapRow   - (inRow[3:0]>>3) )*PW*CH+(mapCol   - (inCol[3:0]>>3) )*CH +: CH]),
	.a01(rmap[(mapRow   - (inRow[3:0]>>3) )*PW*CH+(mapCol+1 - (inCol[3:0]>>3) )*CH +: CH]),
	.a10(rmap[(mapRow+1 - (inRow[3:0]>>3) )*PW*CH+(mapCol   - (inCol[3:0]>>3) )*CH +: CH]),
	.a11(rmap[(mapRow+1 - (inRow[3:0]>>3) )*PW*CH+(mapCol+1 - (inCol[3:0]>>3) )*CH +: CH]),
	.alpha(inCol),
	.beta(inRow),
	.lerp(w_ri)
);

bi_interpolator lerp_g(
	.a00(gmap[(mapRow   - (inRow[3:0]>>3) )*PW*CH+(mapCol   - (inCol[3:0]>>3) )*CH +: CH]),
	.a01(gmap[(mapRow   - (inRow[3:0]>>3) )*PW*CH+(mapCol+1 - (inCol[3:0]>>3) )*CH +: CH]),
	.a10(gmap[(mapRow+1 - (inRow[3:0]>>3) )*PW*CH+(mapCol   - (inCol[3:0]>>3) )*CH +: CH]),
	.a11(gmap[(mapRow+1 - (inRow[3:0]>>3) )*PW*CH+(mapCol+1 - (inCol[3:0]>>3) )*CH +: CH]),
	.alpha(inCol),
	.beta(inRow),
	.lerp(w_gi)
);

bi_interpolator lerp_b(
	.a00(bmap[(mapRow   - (inRow[3:0]>>3) )*PW*CH+(mapCol   - (inCol[3:0]>>3) )*CH +: CH]),
	.a01(bmap[(mapRow   - (inRow[3:0]>>3) )*PW*CH+(mapCol+1 - (inCol[3:0]>>3) )*CH +: CH]),
	.a10(bmap[(mapRow+1 - (inRow[3:0]>>3) )*PW*CH+(mapCol   - (inCol[3:0]>>3) )*CH +: CH]),
	.a11(bmap[(mapRow+1 - (inRow[3:0]>>3) )*PW*CH+(mapCol+1 - (inCol[3:0]>>3) )*CH +: CH]),
	.alpha(inCol),
	.beta(inRow),
	.lerp(w_bi)
);

dither dither_r(
	.value(rgbCode[11:8]),
	.x(~freeX[0]),
	.y(freeX[3]),
	.dithered(dR)
);

dither dither_g(
	.value(rgbCode[7:4]),
	.x(freeX[0]),
	.y(freeX[3]),
	.dithered(dG)
);

dither dither_b(
	.value(rgbCode[3:0]),
	.x(freeX[0]),
	.y(~freeX[3]),
	.dithered(dB)
);

endmodule
