module gate(input enable, in; output out) {
	if(enable) {
		clock = clock & 1;
		}
	if (!enable) {
		clock = 0;
		reset = 1;
		}
	
	gated(in,out);
	}

module gated(input in; output out) {
	if (posedge) {
		out = in;
		}
	}
