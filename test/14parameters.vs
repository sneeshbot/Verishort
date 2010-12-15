module mod(output a[3], b) {
	parameter paramete = 1b;
	parameter parametere = 2;
	mod2(c=paramete,a=b);
	a=parametere;
	}

module mod2(input c; output a) {
	parameter paramete = 0;
	a = paramete;
	}
