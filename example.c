int __syscall putchar(int c);

int print_fizz() {
    putchar(70);
    putchar(105);
    putchar(122);
    putchar(122);
    putchar(10);
}

int print_buzz() {
    putchar(66);
    putchar(117);
    putchar(122);
    putchar(122);
    putchar(10);
}

int print_fizz_buzz() {
    putchar(70);
    putchar(105);
    putchar(122);
    putchar(122);
    putchar(66);
    putchar(117);
    putchar(122);
    putchar(122);
    putchar(10);
}

int print_num(int i) {
    int hundreds = i / 100;
    int tens = i / 10 - hundreds * 10;
    int ones = i - hundreds * 100 - tens * 10;
    if (hundreds) putchar(hundreds + 48);
    if (tens) putchar(tens + 48);
    putchar(ones + 48);
    putchar(10);
}

int main() {
    for (int i = 1; i <= 100; ++i) {
        if (i % 15 == 0) print_fizz_buzz();
        else if (i % 3 == 0) print_fizz();
        else if (i % 5 == 0) print_buzz();
        else print_num(i);
    }
}
