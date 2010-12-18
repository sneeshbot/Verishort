module mod1 (input a; output b) {
	parameter d = 0;
	parameter dd = 1;
	wire f[1] = !a;
	register f[5];
}

module mod2 (input c; output d) {}

