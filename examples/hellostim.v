// This is a very rudimentary stim file for the hello world program.
// It tells the mipspipe to load a value and then store the same value
// back into memory.
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
