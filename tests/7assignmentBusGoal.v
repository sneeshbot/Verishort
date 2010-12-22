module mod(_clock,_reset,a,b);
	input _clock;
	input _reset;

	input [11:0] a;
	output [11:0] b;
	assign b=a;
	endmodule
