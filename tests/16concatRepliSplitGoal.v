module mod(clock,reset,a);
	input clock;
	input reset;

	output [1:0]a;
	wire b;
	wire c;
	wire d;
	wire [5:0]e;
	wire [11:0]f;

	assign b = 0;
	assign c = 1;
	assign d = 2'b10;
	assign e = {b,3{c},1{2'b01}};

	assign f = 2{e};
	endmodule
