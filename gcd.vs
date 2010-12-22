module gcd(input num0[8], num1[8], start; output greatest[8]) {
	register found;
	register tmp[8];
	
	greatest = tmp;
	
	if (start) {
		if (num0<num1) {
			tmp = num0;
			}
		else {
			tmp = num1;
			}
		}
	
	if (posedge) {
		if (~found) {
			if (num0%tmp==0 & num1%tmp==0) {
				found = 1;
				}
			else {
				tmp = tmp -1;
				}
			}
		}

	}
