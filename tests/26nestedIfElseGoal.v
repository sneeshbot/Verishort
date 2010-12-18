module nestedIfElse(clock,reset,a,b);
	input clock;
	input reset;
	input a;
	output [1:0]b;

	always @ (*) begin
		if (a) begin
			if (0) begin
				b=1;
				end
			else begin
				if (1) begin
					b=2;
					end
				end
			end
		else begin
			b=3;
			end
		end

	endmodule 
