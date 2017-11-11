
int main()
{
	int a = 10;

	printf("hello, world %d, %d, %d\n", foo(2, 1), *&a, &a);

	return 11;
}

int foo(int a, int b)
{
	return a * a + b;
}

int printf(ubyte *, ...);
