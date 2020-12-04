void f(int m) {
	m = 0;
}

int main() {
	int a = 0;
	int b = 0;
	int i = 0;
	for (i = 0; i < 50; i++) {
		a++;
		b = 10;
		do {
			a++;
			b--;
		} while (b > 0);
	}
	return 1;
}
