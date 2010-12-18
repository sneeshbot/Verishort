module[2] parent(input i) {
	return ^childBus(i=i)+child(i=i);
	}

module child(input i) {
	return i+1;
	}

module [5]childBus(input i) {
	return 5{i}
	}
