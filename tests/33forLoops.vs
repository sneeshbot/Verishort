module memArray(input row[2], column[2]) {
	wire value;
	for (i=0; i<4; i=i+1) {
		if (i==row) {
			value = memRow(select=column);
			}
		}
	return value;
	}
	
	
module memRow(input select[2]) {
	register ff[4];
	return ff[select];
	}
