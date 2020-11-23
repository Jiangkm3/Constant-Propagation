int main() {
	int a;
	int i;
	for(i = 0; a < 100; i += 4) {
		if (a % 2 == 0) {
			a += 3;
		} else {
			a += 1;
		}
	}
	return 1;
}
