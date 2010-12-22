module one(_clock,_reset,i,o);
	input _clock;
	input _reset;
	input i;
	output o;

	wire mid;
	three three_0(._clock(_clock),._reset(_reset),.in(i),.out(mid));
	three three_1(._clock(_clock),._reset(_reset),.in(i));
	two two_0(._clock(_clock),._reset(_reset),.in(mid),.out(o));
	endmodule

module two(_clock,_reset,in,out);
	input _clock;
	input _reset;
	input in;
	output out;

	assign _clock = !_clock;
	three three_0(._clock(_clock),._reset(_reset),.in(in),.out(out));
	
	endmodule

module three(_clock, _reset, in, out);
	input _clock;
	input _reset;
	input in;
	output out;

	reg out_reg;
	assign out = out_reg;

	always @(negedge _clock) begin
		out_reg <= in;
		end
	endmodule
	
