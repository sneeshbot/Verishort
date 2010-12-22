module nestedIfElse(_clock,_reset,a,b);
	input _clock;
	input _reset;
	input a;
	output [1:0]b;
	wire [1:0]d;

	reg [1:0]b_reg;
	assign b = b_reg;
	reg [1:0]d_reg;
	assign d = d_reg;

	always @ (*) begin
		if (a) begin
			if (0) begin
				b_reg=1;
				end
			else begin
				if (1) begin
					b_reg=2;
					d_reg=b_reg;
					end
				end
			end
		else begin
			b_reg=3;
			end

		if (_reset) begin
			b_reg =0;
			d_reg =0;
			end
		end
	endmodule 
