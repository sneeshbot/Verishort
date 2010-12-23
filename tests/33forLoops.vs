module memArray(input row[3], column[3]; output value) {
	wire rowdecoded[8];
	wire val[8];
	wire mid[8];
	
	decoder(select=row; decoded=rowdecoded);

	mid = val & rowdecoded;
	value = | mid; 

	for (i=0; i<8; i=i+1) {
		memRow(select=column; value = val[i]);
		}
	
	
	}
	
	
module memRow(input select[3]; output value) {
	wire coldecoded[8];
	wire mid[8];

	register ff[8];
	
	decoder(select=select; decoded=coldecoded);
	
	mid = ff&coldecoded;
	value = | mid;
	}
	
module decoder(input select[3]; output decoded[8]) {
	case (select) {
		000b:decoded=00000001b;
		001b:decoded=00000010b;
		010b:decoded=00000100b;
		011b:decoded=00001000b;
		100b:decoded=00010000b;
		101b:decoded=00100000b;
		110b:decoded=01000000b;
		111b:decoded=10000000b;		
		}
	}
