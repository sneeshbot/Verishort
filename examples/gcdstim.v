// This is a very rudimentary stim file for the gcd.  It will input two numbers, 456 and 42, and find the greatest common denominator
module stim();

  // reg feeds input, wires get output
  reg clk;
  reg [7:0]num1;
  reg [7:0]num0;
  reg [3:0]count;
  wire [7:0]greatest;
  wire success;

  // instance the dut
  _gcd dut(
		._clock(clk),
		._reset(reset),
		._num0(num0),
		._num1(num1),
		._greatest(greatest),
		._success(success)
		);

  initial begin
    reset = 1;

    #1 clk = 0;
    #1 clk = 1; // first positive edge, i.e. first cycle.  Zero out the module
    
    reset = 0;
    start = 1;
    num0 = 42;
    num1 = 456;
    
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);

    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1;
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);
    #1 clk = 0;
    #1 clk = 1; 
	$display("Found:%d Current greatest %d\n",success,greatest);

	#1 $finish;
  end
endmodule
