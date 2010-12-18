module mod(clock,reset,b,a);
	input clock;
	input reset;

	input [8:0]b;
	output [19:0]a;

	assign a[10] = b[7];
	assign a[9:1] = b;
	assign a[12:11] = 2'b10;
	assign a[15:13] = 5;
	endmodule
