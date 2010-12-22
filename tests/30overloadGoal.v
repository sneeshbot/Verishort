module gate(_clock, _reset, enable, in, out);
	input _clock;
	input _reset;
	input enable;
	input in;
	output out;

	reg _clock_reg;
	assign _clock = _clock_reg;
	reg _reset_reg;
	assign _reset = _reset_reg;
	
	always @(*) begin
		if (enable) begin
			_clock_reg = _clock & 1;
			end
		if (!enable) begin
			_clock_reg = 0;
			_reset_reg = 1;
			end

		if (_reset) begin
			_clock_reg = 0;
			_reset_reg = 0;
			end
		end

	gated gated_0(._clock(_clock),._reset(_reset),.in(in),.out(out));
	endmodule

module gated(_clock, _reset, in, out);
	input _clock;
	input _reset;
	input in;
	output out;

	reg out_reg;
	assign out = out_reg;

	always @(posedge _clock) begin
		out_reg = in;
		end

	endmodule
