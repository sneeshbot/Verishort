module nestedIfElse(input a; output b[2]) {
	if (a) {
		if (0) {
			b=1;
			}
		else {
			if (1) {
				b=2;
				}
			}
		}
	else {
		b=3;
		}
	}
