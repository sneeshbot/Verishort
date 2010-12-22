module mod(_clock,_reset, a,b,c);
	input _clock;
	input _reset;
	input [9:0] a;
	input b;
	output c;
	assign c=a;
	endmodule
