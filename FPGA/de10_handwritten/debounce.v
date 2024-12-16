module debounce(input clk, input sig, output reg d_sig);

reg q1, q2, q3, q4 = 0;

always @(posedge clk) begin
  q4 <= q3;
  q3 <= q2;
  q2 <= q1;
  q1 <= sig;
  
  if (q1 == q2 && q2 == q3 && q3 == q4)
    d_sig <= q1;
  else
    d_sig <= d_sig;
end

endmodule