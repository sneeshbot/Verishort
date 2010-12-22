module mod(_clock,_reset,a,b);
	input _clock;
	input _reset;

	output [2:0]a;
	output b;
	mod2 mod2_0(._clock(_clock),._reset(_reset),.c(1'b1),.a(b));
	assign a=2;
	endmodule
	
module mod2(_clock,_reset,c,a);
	input _clock;
	input _reset;

	input c;
	output a;
	assign a = 0;
	endmodule

