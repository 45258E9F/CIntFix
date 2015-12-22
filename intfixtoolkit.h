#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>
#include <gmp.h>

unsigned int __CHECK_GMP_UINT(mpz_t op);
int __CHECK_GMP_INT(mpz_t op);
unsigned long __CHECK_GMP_ULONG(mpz_t op);
long __CHECK_GMP_SLONG(mpz_t op);
unsigned short __CHECK_GMP_USHORT(mpz_t op);
short __CHECK_GMP_SSHORT(mpz_t op);
unsigned char __CHECK_GMP_UCHAR(mpz_t op);
char __CHECK_GMP_SCHAR(mpz_t op);

unsigned int __CHECK_SIMPLE_UINT(unsigned long int op, int sign);
int __CHECK_SIMPLE_INT(unsigned long int op, int sign);
signed long __CHECK_SIMPLE_SLONG(unsigned long int op, int sign);
unsigned long __CHECK_SIMPLE_ULONG(unsigned long int op, int sign);
short __CHECK_SIMPLE_SSHORT(unsigned long int op, int sign);
unsigned short __CHECK_SIMPLE_USHORT(unsigned long int op, int sign);
char __CHECK_SIMPLE_SCHAR(unsigned long int op, int sign);
unsigned char __CHECK_SIMPLE_UCHAR(unsigned long int op, int sign);

void __CHECK_POINTER_PLUS_UL(unsigned long int base, unsigned long int offset);
void __CHECK_POINTER_PLUS_SL(unsigned long int base, long int offset);
void __CHECK_POINTER_MINUS_UL(unsigned long int base, unsigned long int offset);
void __CHECK_POINTER_MINUS_SL(unsigned long int base, long int offset);

int __CALC_BRANCH_HASH(int n, ...);
void __CINTFIX_ERROR(const char * errmsg);
