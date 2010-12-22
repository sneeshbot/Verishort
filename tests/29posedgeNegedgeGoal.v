module pn(_clock,_reset,o,p);
	input _clock;
	input _reset;
	output o;
	output p;

	reg o_reg;
	assign o=o_reg;
	reg p_reg;
	assign p=p_reg;
	
	always @ (posedge _clock) begin
		o_reg<=1;
		p_reg<=0;
		end

	always @ (negedge _clock) begin
		if (o==1) begin
			o_reg<=0;
			p_reg<=1;
			end
		end

	always @ (*) begin
		if (_reset) begin
			o_reg<=0;
			p_reg<=0;
			end
		end
	endmodule
	
