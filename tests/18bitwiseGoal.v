module moo(clock,reset,a,b,c,d);
	input clock;
	input reset;

	input [1:0]a;
	input [1:0]b;
	input c;
	output [1:0]d;

	assign d = (!3 & b | (2{c}!&2'b10) !| a) ^ a !^ 2{1};
	
	endmodule
