module _helloWorld(_clock, _reset, _enable, _letter);
input _clock;
input _reset;
input _enable;
output [7:0] _letter;
reg [7:0] __reg_letter;

wire [3:0] _c;

reg [3:0] _count;

assign _c = _count;
assign _letter = __reg_letter;
always @ (*) begin
if (_enable[0:0])
begin
casex(_c)
4'b0001: 
begin
__reg_letter=72;
end
4'b0010: 
begin
__reg_letter=101;
end
4'b0011: 
begin
__reg_letter=108;
end
4'b0100: 
begin
__reg_letter=108;
end
4'b0101: 
begin
__reg_letter=111;
end
4'b0110: 
begin
__reg_letter=32;
end
4'b0111: 
begin
__reg_letter=87;
end
4'b1000: 
begin
__reg_letter=111;
end
4'b1001: 
begin
__reg_letter=114;
end
4'b1010: 
begin
__reg_letter=108;
end
4'b1011: 
begin
__reg_letter=100;
end
4'b1100: 
begin
__reg_letter=33;
end
endcase
end
else
begin
end
if (_reset) begin
__reg_letter= 0;
_count= 0;
end
end
always @ (posedge _clock) begin
_count=(_count+1);
end
always @ (negedge _clock) begin
end
endmodule
