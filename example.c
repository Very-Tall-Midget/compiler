int power(int base, int exp)
{
    if (exp == 0)
        return 1;
    int res = base;
    for (int i = 1; i < exp; i++)
        res *= base;
    return res;
}

int factorial(int n)
{
    if (n <= 1)
        return 1;
    return n * factorial(n - 1);
}

int main()
{
    return factorial(9) + power(2, 10);
}
