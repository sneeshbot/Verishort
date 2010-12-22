module mod(output a[5]) {
	wire b[4],c[2];
	b = 10;//4 long
	c = 10b;
	a=(b-(c*b+0b)-5)%3;
	}
