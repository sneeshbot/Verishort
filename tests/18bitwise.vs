module moo(input a[2], b[2], c; output d[2]) {
	d = (~a & b | (concat(2{c})&10b) | a) ^ a;
	}
