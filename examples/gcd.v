module _gcd(_clock, _reset, _num0, _num1, _start, _greatest, _success);
input _clock;
input _reset;
input [7:0] _num0;
input [7:0] _num1;
input _start;
output [7:0] _greatest;
output _success;
reg [7:0] __reg_greatest;

reg _found;

assign _success = _found;
assign _greatest[7:0] = __reg_greatest[7:0];
always @ (*) begin
if (_start)
begin
if ((_num0[7:0]<_num1[7:0]))
begin
__reg_greatest[7:0]=_num0[7:0];
end
else
begin
__reg_greatest[7:0]=_num1[7:0];
end
end
else
begin
end
if (_reset) begin
__reg_greatest= 0;
_found= 0;
end
end
always @ (posedge _clock) begin
if ((~_found))
begin
if ((((_num0[7:0]%_greatest[7:0])==0)&((_num1[7:0]%_greatest[7:0])==0)))
begin
_found=1;
end
else
begin
__reg_greatest[7:0]=(_greatest[7:0]-1);
end
end
else
begin
end
end
always @ (negedge _clock) begin
end
endmodule
