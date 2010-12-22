module mo(_clock,_reset,a,b);
	input _clock;
	input _reset;
	input a;
	output [9:0]b;
	assign b = a;
	endmodule
