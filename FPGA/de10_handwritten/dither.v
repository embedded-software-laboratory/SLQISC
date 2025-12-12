module dither(input [3:0] value, input x, input y, output [1:0] dithered);

wire [1:0] low = value/5;
wire [1:0] high = (value+4)/5;

wire [1:0] d = value[1:0]-high;

wire [1:0] bayer = {x ^ y, y};

assign dithered = d <= bayer ? low : high;

endmodule