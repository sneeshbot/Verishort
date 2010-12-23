module gcd(input num0[8], num1[8], start; output greatest[8]) {
	register found;
	
	if (start) {
		if (num0<num1) {
			greatest = num0;
			}
		else {
			greatest = num1;
			}
		}
	
	if (posedge) {
		if (~found) {
			if (num0%greatest==0 & num1%greatest==0) {
				found = 1;
				}
			else {
				greatest = greatest -1;
				}
			}
		}

	}
