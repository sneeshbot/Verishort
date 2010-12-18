module pn(clock,reset,o,p);
	input clock;
	input reset;
	output o;
	output p;

	reg o_reg;
	assign o=o_reg;
	reg p_reg;
	assign p=p_reg;
	
	always @ (posedge clk) begin
		o_reg<=1;
		p_reg<=0;
		end

	always @ (negedge clk) begin
		if (o==1) begin
			o_reg<=0;
			p_reg<=1;
			end
		end

	always @ (*) begin
		if (reset) begin
			o_reg<=0;
			p_reg<=0;
			end
		end
	endmodule
	
