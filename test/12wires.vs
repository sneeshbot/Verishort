module m (output b[2], c, x, y[3]) {
	wire a[2];
	wire d;
	wire e, f[3];
	a = 3;
	d = 1b;
	e = 0;
	f = 010b;
	
	b = a;
	c = d;
	x = e;
	y = f;
	}
