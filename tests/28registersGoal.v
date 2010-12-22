module count(_clock, _reset, out,tick);
	input _clock;
	input _reset;
	output [3:0]out;
	output tick;

	reg [3:0]c;
	reg t;

	assign out = c;
	assign tick = t;

	always @(posedge _clock) begin
		c <= c + 1;
		if (t) begin
			t <= 0;
			end
		else begin
			t <= 1;
			end
		end
	always @(*) begin
		if (_reset) begin
			c <= 0;
			t <= 0;
			end
		end
	endmodule
