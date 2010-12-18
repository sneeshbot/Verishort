module mod(clock,reset,a,b);
	input clock;
	input reset;

	output [2:0]a;
	output b;
	mod2 mod2_0(.clock(clock),.reset(reset),.c(1'b1),.a(b));
	assign a=2;
	endmodule
	
module mod2(clock,reset,c,a);
	input clock;
	input reset;

	input c;
	output a;
	assign a = 0;
	endmodule

