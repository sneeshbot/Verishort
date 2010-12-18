module _(input a[2],b[3],c[4],d[5]; output e) {
	wire f[3], g[3]; 
	f = concat(&20,|b,^c);
	g = concat(!&d,!|a,!^0110b);
	e = & concat(f,g,05);
	
	}
