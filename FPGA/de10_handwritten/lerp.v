module l4_interpolator(input [3:0] a, input [3:0] b, input [3:0] alpha, output [7:0] lerp);

assign lerp = a < b ? {4'b0,alpha}*{4'b0,b-a} + {a,4'b0} : {a,4'b0} - {4'b0,alpha}*{4'b0,a-b};

endmodule

module l8_interpolator(input [7:0] a, input [7:0] b, input [3:0] alpha, output [11:0] lerp);

assign lerp = a < b ? {8'b0,alpha}*{4'b0,b-a} + {a,4'b0} : {a,4'b0} - {8'b0,alpha}*{4'b0,a-b};

endmodule


module bi_interpolator(input [1:0] a00, input [1:0] a01, input [1:0] a10, input [1:0] a11, input [3:0] alpha, input [3:0] beta, output [3:0] lerp);

l4_interpolator h1(
  .a({a00,a00}),
  .b({a01,a01}),
  .alpha(alpha),
  .lerp(lerp0)
);

l4_interpolator h2(
  .a({a10,a10}),
  .b({a11,a11}),
  .alpha(alpha),
  .lerp(lerp1)
);

wire [7:0] lerp0;
wire [7:0] lerp1;

l8_interpolator v(
  .a(lerp0),
  .b(lerp1),
  .alpha(beta),
  .lerp(vRes)
);

wire [11:0] vRes;

assign lerp = vRes[11:8];

endmodule