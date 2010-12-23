module stim();

  // reg feeds input, wires get output
  reg clk;
  reg [3:0]row;
  reg [3:0]column;
  wire value;

  // instance the dut
  _memArray dut(
		._clock(clk),
		._reset(reset),
		._row(row),
		._column(column),
		._value(value)
		);

  initial begin
    reset = 1;

    #1 clk = 0;
    #1 clk = 1; // first positive edge, i.e. first cycle.  Zero out the module
    
    reset = 0;
	row = 1;
	col = 5;
    
    #1 clk = 0;
    #1 clk = 1; 
	$display("Value at %d,%d:%d\n",row,column,value);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

	#1 $finish;
  end
endmodule
