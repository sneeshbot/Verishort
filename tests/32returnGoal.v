module parent(clock, reset, i, return);
	input clock;
	input reset;
	input i;
	output return;

	wire child_return;
	wire [4:0]childBus_return;
	child child_0(.clock(clock),.reset(reset),.i(i),.return(child_return));
	childBus childBus_0(.clock(clock),.reset(reset),.i(i),.return(childBusReturn));
	
	assign return = ^childReturn+child;
	endmodule

module child(clock, reset,i,return);
	input clock;
	input reset;
	input i;
	output return;

	assign return = i+1;
	endmodule

module childBus(clock,reset,i,return);
	input clock;
	input reset;
	input i;
	output return;

	assign return = {5{i}};
	endmodule
	
