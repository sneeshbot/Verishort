module mod(_clock,_reset,a,c);
	input _clock;
	input _reset;
	input [1:0] a;
	output c;
	wire d; 
	wire e;
	assign d = 1;
	assign e = 3'b101;
	assign c = a<d & d>e & 8==d | 2'b10 <= 0 ^ a !=d | 1>=0;
	endmodule
