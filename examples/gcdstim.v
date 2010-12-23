// This is a very rudimentary stim file for the gcd.  It will input two numbers, 36 and 24, and find the greatest common denominator
module stim();

  // reg feeds input, wires get output
  reg clk;
  reg reset;
  reg [7:0]num1;
  reg [7:0]num0;
  reg [3:0]count;
  reg start;
  wire [7:0]greatest;
  wire success;

  // instance the dut
  _gcd dut(
		._clock(clk),
		._reset(reset),
		._num0(num0),
		._num1(num1),
		._start(start),
		._greatest(greatest),
		._success(success)
		);

  initial begin
    reset = 1;

    #1 clk = 0;
    #1 clk = 1; // first positive edge, i.e. first cycle.  Zero out the module
    
    reset = 0;
    start = 1;
    num0 = 36;
    num1 = 24;
    
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

	#1 $finish;
  end
endmodule
