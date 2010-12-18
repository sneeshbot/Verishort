//Counts up to 16
module count(output out[4], tick) {
	register c[4];
	register t;

	if (posedge) {
		c = c + 1;
		if (t==1) {
			t = 0;
			}
		else {
			t = 1;
			}
		}
	tick = t;
	out = c;
	
	}
