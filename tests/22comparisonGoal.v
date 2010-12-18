module mod(clock,reset,a,c) {
	input clock;
	input reset;
	input [1:0] a;
	output c;
	wire d; 
	wire e;
	d = 1;
	e = 3'b101;
	assign c = a<d & d>e & 8==d | 10b <= 0 ^ a !=d | 1>=0;
	endmodule
