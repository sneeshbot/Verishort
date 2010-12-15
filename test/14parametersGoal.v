module mod(a,b);
	output [2:0]a;
	output b;
	mod2 mod2_0(.c(1'b1),.a(b));
	assign a=2;
	endmodule
	
module mod2(c,a);
	input c;
	output a;
	assign a = 0;
	endmodule

