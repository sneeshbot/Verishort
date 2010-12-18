module ifElse(clock,reset,a,b,c);
	input clock;
	input reset;
	input a;
	output b;
	output c;
	wire [1:0]d;
	
	reg b_reg;
	assign b=b_reg;
	reg c_reg;
	assign c=c_reg;
	reg [1:0]d_reg;
	assign d=d_reg;
	
	always @ (*) begin
		if (a) begin
			b_reg <= 1;
			end
		else if (a>1) begin
			b_reg <= 0;
			c_reg <= 1;
			end
		else if (0) begin
			b_reg <= 1;
			end
		else begin
			b_reg <= 0;
			d_reg <= 3;
			end

		if (reset) begin
			b_reg <=0;
			c_reg <=0;
			end
		end
	endmodule
	
