// int __syscall putchar(int c);
// int __syscall getchar();

// int num = (2 > 3 || 4 < 5) ? 100 : 14;

// int power(int n, int e) {
//     if (e == 0) return 1;
//     int res = n;
//     for (int i = 1; i < e; i++) {
//         res *= n;
//     }
//     return res;
// }

// int print_num(int n) {
//     if (n == 0) putchar(48);
//     else {
//         int start = 0;
//         for (; n >= power(10, start + 1); start++);
//         for (; start >= 0; start--) {
//             int digit = n / power(10, start);
//             n -= digit * power(10, start);
//             putchar(digit + 48);
//         }
//     }
// }

// int print_fizz() {
//     putchar(70);
//     putchar(105);
//     putchar(122);
//     putchar(122);
// }

// int print_buzz() {
//     putchar(66);
//     putchar(117);
//     putchar(122);
//     putchar(122);
// }

// int print_fizz_buzz() {
//     putchar(70);
//     putchar(105);
//     putchar(122);
//     putchar(122);
//     putchar(66);
//     putchar(117);
//     putchar(122);
//     putchar(122);
// }

// int print_intro() {
//     putchar(70);
//     putchar(105);
//     putchar(114);
//     putchar(115);
//     putchar(116);
//     putchar(32);
//     print_num(num);
//     putchar(32);
//     putchar(110);
//     putchar(117);
//     putchar(109);
//     putchar(98);
//     putchar(101);
//     putchar(114);
//     putchar(115);
//     putchar(32);
//     putchar(111);
//     putchar(102);
//     putchar(32);
//     putchar(70);
//     putchar(105);
//     putchar(122);
//     putchar(122);
//     putchar(66);
//     putchar(117);
//     putchar(122);
//     putchar(122);
//     putchar(58);
//     putchar(10);
// }

// int print_error() {
//     putchar(80);
//     putchar(108);
//     putchar(101);
//     putchar(97);
//     putchar(115);
//     putchar(101);
//     putchar(32);
//     putchar(101);
//     putchar(110);
//     putchar(116);
//     putchar(101);
//     putchar(114);
//     putchar(32);
//     putchar(97);
//     putchar(110);
//     putchar(32);
//     putchar(105);
//     putchar(110);
//     putchar(116);
//     putchar(101);
//     putchar(103);
//     putchar(101);
//     putchar(114);
//     putchar(58);
//     putchar(32);
// }

// int print_request() {
//     putchar(69);
//     putchar(110);
//     putchar(116);
//     putchar(101);
//     putchar(114);
//     putchar(32);
//     putchar(116);
//     putchar(104);
//     putchar(101);
//     putchar(32);
//     putchar(110);
//     putchar(117);
//     putchar(109);
//     putchar(98);
//     putchar(101);
//     putchar(114);
//     putchar(32);
//     putchar(111);
//     putchar(102);
//     putchar(32);
//     putchar(110);
//     putchar(117);
//     putchar(109);
//     putchar(98);
//     putchar(101);
//     putchar(114);
//     putchar(115);
//     putchar(32);
//     putchar(111);
//     putchar(102);
//     putchar(32);
//     putchar(70);
//     putchar(105);
//     putchar(122);
//     putchar(122);
//     putchar(66);
//     putchar(117);
//     putchar(122);
//     putchar(122);
//     putchar(32);
//     putchar(116);
//     putchar(111);
//     putchar(32);
//     putchar(100);
//     putchar(105);
//     putchar(115);
//     putchar(112);
//     putchar(108);
//     putchar(97);
//     putchar(121);
//     putchar(58);
//     putchar(32);
// }

// int getint() {
//     int res, error;
//     do {
//         error = 0;
//         int c;
//         while ((c = getchar()) != 10) {
//             if (c >= 48 && c <= 57)
//                 res = res * 10 + c - 48;
//             else {
//                 while (getchar() != 10);
//                 res = 0;
//                 error = 1;
//                 print_error();
//                 break;
//             }
//         }
//     } while (error);
//     return res;
// }

// int main() {
//     print_request();
//     num = getint();
//     print_intro();

//     int i = 1;
//     for (; i <= num; ++i) {
//         if (i % 15 == 0) print_fizz_buzz();
//         else if (i % 3 == 0) print_fizz();
//         else if (i % 5 == 0) print_buzz();
//         else print_num(i);
//         putchar(10);
//     }
// }

int main() {
    return 2 + 3;
}
