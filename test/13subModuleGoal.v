module mod0(a,b,c);
	input [9:0]a;
	input [2:0]b;
	output c;
	
	mod0 mod1_0(.a(b),.d(c),.e(b));
	endmodule;

module mod1(a,b,d,e);
	input [2:0]a;
	input [1:0]b;
	output d;
	output [2:0]e;
	
	assign d = b[0];
	endmodule;
