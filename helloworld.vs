module helloWorld(input enable; output letter[8]) {
	register count[4];
	wire c[4];
	
	if (enable) {
		case(c) {
			0000b:letter=01001000b;
			0001b:letter=01100101b;
			0010b:letter=01101100b;
			0011b:letter=01101100b;
			0100b:letter=01101111b;
			0101b:letter=00100000b;
			0110b:letter=01010111b;
			0111b:letter=01101111b;
			1000b:letter=01110010b;
			1001b:letter=01101100b;
			1010b:letter=01100100b;
			1011b:letter=00100001b;
			//default: letter=00000000b;
			}
		}
	
	if (posedge) {
		count = count+1;
		}
	}
