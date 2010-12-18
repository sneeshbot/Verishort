module gate(clock, reset, enable, in, out);
	input clock;
	input reset;
	input enable;
	input in;
	output out;

	reg clock_reg;
	assign clock = clock_reg;
	reg reset_reg;
	assign reset = reset_reg;
	
	always @(*) begin
		if (enable) begin
			clock_reg <= clock & 1;
			end
		if (!enable) begin
			clock_reg <= 0;
			reset_reg <= 1;
			end

		if (reset) begin
			clock_reg <= 0;
			reset_reg <= 0;
			end
		end

	gated gated_0(.clock(clock),.reset(reset),.in(in),.out(out))
	endmodule

module gated(clock, reset, in, out);
	input clock;
	input reset;
	input in;
	output out;

	reg out_reg;
	assign out = out_reg;

	always @(posedge clock) begin
		out_reg <= in;
		end

	endmodule
