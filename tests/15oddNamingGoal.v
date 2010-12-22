module m22_1sk(_clock,_reset,d_m1,e3f,_23, _s3s);
	input _clock;
	input _reset;

	output [0:0]d_m1;
	output [103:0]e3f;
	input [31:0]_23;
	input [2:0]_s3s;
	_13p _13p_0(.b01b(d_m1),._23(_23));
	endmodule

module _13p(_clock,_reset,b01b,_23);
	input _clock;
	input _reset;

	output b01b;
	input [103:0]_23;
	wire _13p_0 = 1;
	assign b01b = _13p_0;
	endmodule
