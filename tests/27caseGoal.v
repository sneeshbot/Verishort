module cas(_clock,_reset,a,b,c,d);
	input _clock;
	input _reset;
	input a;
	input [2:0]b;
	output [2:0]c;
	output d;

	reg [2:0]c_reg;
	assign c=c_reg;
	reg d_reg;
	assign d=d_reg;

	always @(*) begin
		casex(a)
			1: begin
				c_reg=3;
				end
			0: begin
				d_reg=1'b0;
				end
			endcase
		casex(b)
			3'b10x: begin
				d_reg=1;
				end
			3'b0x1: begin
				d_reg=0;
				c_reg=3'b010;
				end
			default: begin
				c_reg=2'b10;
				end
			endcase
		if (_reset) begin
			c_reg =0;
			d_reg =0;
			end
		end
	endmodule
