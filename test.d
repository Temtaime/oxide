
int main()
{
	printf("hello, world %d\n", foo(2, 1));

	return 11;
}

int foo(int a, int b)
{
	return a * a + b;
}

int printf(ubyte *, ...);
