module cas(input a, b[3]; output c[3], d) {
	case(a) {
		1:{c=3;}
		0:d=0b;
		}
	case(b) {
		10x:{d=1}
		0x1:{d=0;c=010b;}
		default:c=10b;
		}
	}
