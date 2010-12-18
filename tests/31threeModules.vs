module one(input i; output o) {
	wire mid;
	
	three(in=i,out=mid);
	three(in=mid);
	two(in=mid, out=o);
	}

module two(input in; output out) {
	clock = !clock;
	three(in=in,out=out);
	}
	
module three(input in; output out) {
	if (negedge) {
		out = in;
		}
	}
