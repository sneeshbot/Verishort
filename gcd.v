module _gcd(_clock, _reset, _num0, _num1, _start, _greatest);
input _clock;
input _reset;
input [7:0] _num0;
input [7:0] _num1;
input _start;
output [7:0] _greatest;
reg [7:0] _tmp;

reg _found;

assign _greatest[7:0] = _tmp[7:0];

always @ (*) begin
	if (_start) begin
		if ((_num0[7:0]<_num1[7:0])) begin
			_tmp[7:0]=_num0[7:0];
			end
		else begin
			_tmp[7:0]=_num1[7:0];
			end
		end
	else begin
		end
	if (_reset) begin
		_tmp= 0;
		_found= 0;
		end
	end

always @ (posedge _clock) begin
	if ((~_found)) begin
		if ((((_num0[7:0]%_tmp[7:0])==0)&((_num1[7:0]%_tmp[7:0])==0))) begin
			_found=1;
			end
		else begin
			_tmp[7:0]=(_tmp[7:0]-1);
			end
		end
	else begin
		end
	end

always @ (negedge _clock) begin
	end
	
endmodule
