module _helloWorld(_clock, _reset, _enable, _letter);
input _clock;
input _reset;
input _enable;
output [7:0] _letter;
reg [7:0] __reg_letter;

wire [3:0] _c;

reg [3:0] _count;

assign _letter[7:0] = __reg_letter[7:0];
always @ (*) begin
if (_enable)
begin
casex(_c[3:0])
4'b0000: 
begin
__reg_letter[7:0]=72;
end
4'b0001: 
begin
__reg_letter[7:0]=101;
end
4'b0010: 
begin
__reg_letter[7:0]=108;
end
4'b0011: 
begin
__reg_letter[7:0]=108;
end
4'b0100: 
begin
__reg_letter[7:0]=111;
end
4'b0101: 
begin
__reg_letter[7:0]=32;
end
4'b0110: 
begin
__reg_letter[7:0]=87;
end
4'b0111: 
begin
__reg_letter[7:0]=111;
end
4'b1000: 
begin
__reg_letter[7:0]=114;
end
4'b1001: 
begin
__reg_letter[7:0]=108;
end
4'b1010: 
begin
__reg_letter[7:0]=100;
end
4'b1011: 
begin
__reg_letter[7:0]=33;
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
_count[3:0]=(_count[3:0]+1);
end
always @ (negedge _clock) begin
end
endmodule
