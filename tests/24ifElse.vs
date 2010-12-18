module ifElse(input a; output b, c) {
	wire d[2];
	if (a) {
		b = 1;
		}
	else if (a>1) {
		b = 0;
		c = 1;
		}
	else if (0) {
		b = 1;
		}
	else {
		b = 0;
		d = 3;
		}
	}
