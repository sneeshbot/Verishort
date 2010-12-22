module memArray(input row[2]; output column[2]) {
	wire value[2] = 11b;
	for (i=0; i<2; i=i+1) {
			memRow(select[1:0] = column[1:0]);
		}
	//return value;
	}
	
	
module memRow(input select[2]) {
	register ff[4];
	//return ff[select];
	}
