int main() {
	int a[17];
	int i;
	for (i = 0; i < 17; i++) {
		if ((i % 2 == 0) && (i % 3 == 0)) {
			a[i] = i;
		} else if (i % 5 == 0) {
			a[i] = 2 * i;
		}
	}
	return 1;
}
