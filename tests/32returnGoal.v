module parent(_clock, _reset, i, return);
	input _clock;
	input _reset;
	input i;
	output return;

	wire child_return;
	wire [4:0]childBus_return;
	child child_0(._clock(_clock),._reset(_reset),.i(i),.return(child_return));
	childBus childBus_0(._clock(_clock),._reset(_reset),.i(i),.return(childBusReturn));
	
	assign return = ^childReturn+child;
	endmodule

module child(_clock, _reset,i,return);
	input _clock;
	input _reset;
	input i;
	output return;

	assign return = i+1;
	endmodule

module childBus(_clock,_reset,i,return);
	input _clock;
	input _reset;
	input i;
	output return;

	assign return = {5{i}};
	endmodule
	
