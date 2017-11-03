
int main()
{
	int a = 0;

	while(a < 10)
	{
		a = a + 1;

		if(1 > 2)
		{
			break;
		}
		else
			return 1;
	}

	return foo(a + 1, 1);
}

int foo(int a, int b)
{
	return a * a + b;
}
