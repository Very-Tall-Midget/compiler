int power(int base, int exp)
{
    if (exp == 0)
        return 1;
    int res = base;
    for (int i = 1; i < exp; i++)
        res *= base;
    return res;
}

int e = 4, n = 2;
int main()
{
    return power(n, e);
}
