module nestedIfElse(clock,reset,a,b);
	input clock;
	input reset;
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
				assign b_reg<=1;
				end
			else begin
				if (1) begin
					assign b_reg<=2;
					assign d_reg<=b_reg;
					end
				end
			end
		else begin
			b_reg<=3;
			end
		end
	endmodule 
