module _(_clock,_reset,a,b,c,d,e);
	input _clock;
	input _reset;
	input [1:0]a;
	input [2:0]b;
	input [3:0]c;
	input [4:0]d;
	output e;
	wire f;
	wire g;
	assign f = {&20,|b,^c};
	assign g = {~&d,~|a,~^4'b0110};
	assign e = &{f,g,05};
	endmodule
