// This is a very rudimentary stim file for the gcd.  It will input two numbers, 11 and 42, and find the least common denominator
module stim();

  // reg feeds input, wires get output
  reg clk;
  reg enable;
  reg reset;
  wire [7:0]letter;

  // instance the dut
  _helloWorld dut(
		._clock(clk),
		._reset(reset),
		._enable(enable),
		._letter(letter)
		);

  initial begin
    reset = 1;

    #1 clk = 0;
    #1 clk = 1; // first positive edge, i.e. first cycle.  Zero out the module
    
    reset = 0;
	enable = 1;
    
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

    #1 clk = 0;
    #1 clk = 1;
	$display("Letter %c\n", letter);

	#1 $finish;
  end
endmodule
