//NOT DONE

module memArray(clock, reset, row, column, return)
	input clock;
	input reset;
	input [1:0]row;
	input [1:0]column;
	output return;
	wire value;
	reg value_reg;
	assign value = value_reg;
	always @ (*) begin
		if (0==row) begin
			value_reg;
			end
		if (1==row) begin
			value_reg;
			end
		if (2==row) begin
			value_reg;
			end
		if (3==row) begin
			value_reg;
			end
		end

	assign return = value;
	endmodule

module memRow(clock, reset, select, return)
	input clock;
	input reset;
	input [1:0]select;
	output return;
	
	reg [3:0]ff;
	
	assign return = ff[select];
	endmodule
	
