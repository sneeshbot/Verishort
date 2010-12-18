module one(clock,reset,i,o);
	input clock;
	input reset;
	input i;
	output o;

	wire mid;
	three three_0(.clock(clock),.reset(reset),.in(i),.out(mid));
	three three_1(.clock(clock),.reset(reset),.in(i));
	two two_0(.clock(clock),.reset(reset),.in(mid),.out(o));
	endmodule

module two(clock,reset,in,out);
	input clock;
	input reset;
	input in;
	output out;

	assign clock = !clock;
	three three_0(.clock(clock),.reset(reset),.in(in),.out(out));
	
	endmodule

module three(clock, reset, in, out);
	input clock;
	input reset;
	input in;
	output out;

	reg out_reg;
	assign out = out_reg;

	always @(negedge clock) begin
		out_reg <= in;
		end
	endmodule
	
