module _mod(_clock, _reset, _a);
input _clock;
input _reset;
output [12:0] _a;
assign _a[12:0] = 2731;
always @ (*) begin
if (_reset) begin
end
end
always @ (posedge _clock) begin
end
always @ (negedge _clock) begin
end
endmodule
