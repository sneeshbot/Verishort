module ifElse(clock,reset,a,b);
	input clock;
	input reset;
	input a;
	output b;
	wire [1:0]d;
	always @ (*) begin
		if (a) begin
			b = 1;
			end
		else if (a>1) begin
			b = 0;
			c = 1;
			end
		else if (0) begin
			b = 1;
			end
		else begin
			b = 0;
			d = 3;
			end
		end
	endmodule
	
