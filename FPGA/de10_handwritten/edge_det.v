module edge_det (input sig, input clk, output edg);        

   reg sig_last;                          
	always @ (posedge clk) begin
		sig_last <= sig;
	end

	assign edg = sig & ~sig_last;
endmodule