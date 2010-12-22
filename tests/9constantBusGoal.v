module mod(_clock,_reset,a);
	input _clock;
	input _reset;

	output [12:0]a;
	assign a=13'b0101010101011;
	endmodule
