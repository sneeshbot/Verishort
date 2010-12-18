module mod(clock,reset,a);
	input clock;
	input reset;
	output [4:0]a;
	wire [3:0]b;
	wire [1:0]c;
	b = 10;
	c = 2'b10;
	assign a = (b-(c*b+1'b0)/-5)%3;

	endmodule
