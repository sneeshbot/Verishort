module pn(output o, p) {
	if (posedge) {
		o = 1;
		p = 0;
		}
	if (negedge) {
		if (o==1) {
			o=0;
			p=1;
			}
		}

	}
