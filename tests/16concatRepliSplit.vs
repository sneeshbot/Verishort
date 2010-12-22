module mod(output a[2]) {
	wire b, c, d[2], e[6], f[12];
	b = 0;
	c = 1;
	d = 10b;
	e = concat(b,3{c},1{01b});
	f = concat(2{e});
	a = e[5:4];
	}
