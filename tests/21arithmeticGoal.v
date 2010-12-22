module mod(_clock,_reset,a);
	input _clock;
	input _reset;
	output [4:0]a;
	wire [3:0]b;
	wire [1:0]c;
	assign b = 10;
	assign c = 2'b10;
	assign a = (b-(c*b+1'b0)-5)%3;

	endmodule
