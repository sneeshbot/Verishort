module ab(_clock,_reset,a,b,c,d,f);
	input _clock;
	input _reset;
	input a;
	output [7:0]b;
	output [8:0]c;
	output [9:0]d;
	output [10:0]f;
	
	wire [2:0]e;
	assign e=3'b100;
	assign b={{7{a[0]}},a};
	assign c={{7{0}},3};
	assign d={{8{0}},2'b01};
	assign f={{8{e[2]}},e};
	endmodule
