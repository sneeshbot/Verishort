module mod(clock,reset, a,b,c);
	input clock;
	input reset;
	input [9:0] a;
	input b;
	output c;
	assign c=a;
	endmodule
