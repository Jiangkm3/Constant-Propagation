int main() {
	int a[1024];
	int i = 2;
	while (i < 1024) {
		if ((i % 2 == 0) && (i % 3 == 0)) {
			a[i] = i;
		} else if (i % 5 == 0) {
			a[i] = 2 * i;
		}
		i *= 2;
	}
	return 1;
}
