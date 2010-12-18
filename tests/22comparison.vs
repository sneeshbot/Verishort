module mod(input a[2], output c) {
	wire d = 1;
	wire e = 101b;
	c = a<d & d>e & 8==d | 10b <= 0 ^ a !=d | 1>=0;
	}
