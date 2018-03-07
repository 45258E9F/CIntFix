#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>
#include <flint/fmpz.h>

#define DIRTY_VALUE 20
#define PRESSURE_TEST

unsigned int __CHECK_GMP_UINT(fmpz_t op);
int __CHECK_GMP_INT(fmpz_t op);
unsigned long __CHECK_GMP_ULONG(fmpz_t op);
long __CHECK_GMP_SLONG(fmpz_t op);
unsigned short __CHECK_GMP_USHORT(fmpz_t op);
short __CHECK_GMP_SSHORT(fmpz_t op);
unsigned char __CHECK_GMP_UCHAR(fmpz_t op);
char __CHECK_GMP_SCHAR(fmpz_t op);

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

unsigned int __CHECK_GMP_UINT(fmpz_t op)
{
  if(fmpz_cmp_ui(op, UINT_MIN) < 0 || fmpz_cmp_ui(op, UINT_MAX) > 0)
  {
#ifdef PRESSURE_TEST
	return DIRTY_VALUE;
#else
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_GMP_UINT)\n");
#endif
  }
  return ((unsigned int)fmpz_get_ui(op));
}

int __CHECK_GMP_INT(fmpz_t op)
{
  if(fmpz_cmp_si(op, INT_MIN) < 0 || fmpz_cmp_si(op, INT_MAX) > 0)
  {
#ifdef PRESSURE_TEST
	return DIRTY_VALUE;
#else
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_GMP_INT)\n");
#endif
  }
  return ((int)fmpz_get_si(op));
}

unsigned long __CHECK_GMP_ULONG(fmpz_t op)
{
  if(fmpz_cmp_ui(op, ULONG_MIN) < 0 || fmpz_cmp_ui(op, ULONG_MAX) > 0)
  {
#ifdef PRESSURE_TEST
	return DIRTY_VALUE;
#else
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_GMP_ULONG)\n");
#endif
  }
  return ((unsigned long)fmpz_get_ui(op));
}

long __CHECK_GMP_SLONG(fmpz_t op)
{
  if(fmpz_cmp_si(op, LONG_MIN) < 0 || fmpz_cmp_si(op, LONG_MAX) > 0)
  {
#ifdef PRESSURE_TEST
	return DIRTY_VALUE;
#else
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_GMP_SLONG)\n");
#endif
  }
  return ((long)fmpz_get_si(op));
}

unsigned short __CHECK_GMP_USHORT(fmpz_t op)
{
  if(fmpz_cmp_ui(op, USHRT_MIN) < 0 || fmpz_cmp_ui(op, USHRT_MAX) > 0)
  {
#ifdef PRESSURE_TEST
	return DIRTY_VALUE;
#else
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_GMP_USHORT)\n");
#endif
  }
  return ((unsigned short)fmpz_get_ui(op));
}

short __CHECK_GMP_SSHORT(fmpz_t op)
{
  if(fmpz_cmp_si(op, SHRT_MIN) < 0 || fmpz_cmp_si(op, SHRT_MAX) > 0)
  {
#ifdef PRESSURE_TEST
	return DIRTY_VALUE;
#else
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_GMP_SSHORT)\n");
#endif  
  }
  return ((short)fmpz_get_si(op));
}

unsigned char __CHECK_GMP_UCHAR(fmpz_t op)
{
  if(fmpz_cmp_ui(op, UCHAR_MIN) < 0 || fmpz_cmp_ui(op, UCHAR_MAX) > 0)
  {
    return ((unsigned char)fmpz_get_ui(op));
  }
  else
  {
#ifdef PRESSURE_TEST
	return DIRTY_VALUE;
#else
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_GMP_UCHAR)\n");
#endif
  }
}

char __CHECK_GMP_SCHAR(fmpz_t op)
{
  if(fmpz_cmp_si(op, CHAR_MIN) < 0 || fmpz_cmp_si(op, CHAR_MAX) > 0)
  {
    return ((char)fmpz_get_si(op));
  }
  else
  {
#ifdef PRESSURE_TEST
	return DIRTY_VALUE;
#else
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_GMP_SCHAR)\n");
#endif
  }
}

/*
 * op: input integer. Although it has unsigned long type, but it doesn't matter.
 *     We only use its bit-vector.
 * sign: how to interpret input bit-vector? 0 for unsigned and 1 for signed.
 */
unsigned int __CHECK_SIMPLE_UINT(unsigned long int op, int sign)
{
  if(sign)
  {
    signed long int sop = (signed long int)op;
    if(sop >= 0 && sop <= (signed long int)UINT_MAX)
    {
      return (unsigned int)op;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_UINT)\n");
#endif
    }
  }
  else
  {
    if(op >= 0 && op <= UINT_MAX)
    {
      return (unsigned int)op;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_UINT)\n");
#endif
    }
  }
}

int __CHECK_SIMPLE_INT(unsigned long int op, int sign)
{
  if(sign)
  {
    signed long int sop = (signed long int)op;
    if(sop >= (signed long int)INT_MIN && sop <= (signed long int)INT_MAX)
    {
      return (signed int)op;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_INT)\n");
#endif
    }
  }
  else
  {
    if(op >= 0 && op <= (unsigned long int)INT_MAX)
    {
      return (signed int)op;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_INT)\n");
#endif    
    }
  }
}

signed long __CHECK_SIMPLE_SLONG(unsigned long int op, int sign)
{
  if(sign)
  {
    signed long int sop = (signed long int)op;
    return sop;
  }
  else
  {
    if(op >= 0 && op <= (unsigned long int)LONG_MAX)
    {
      return (signed long int)op;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_SLONG)\n");
#endif
    }
  }
}

unsigned long __CHECK_SIMPLE_ULONG(unsigned long int op, int sign)
{
  if(sign)
  {
    signed long int sop = (signed long int)op;
    if(sop >= 0 && sop <= (signed long int)LONG_MAX)
    {
      return (unsigned long int)sop;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_ULONG)\n");
#endif
    }
  }
  else
  {
    return op;
  }
}

short __CHECK_SIMPLE_SSHORT(unsigned long int op, int sign)
{
  if(sign)
  {
    signed long int sop = (signed long int)op;
    if(sop >= (signed long int)SHRT_MIN && sop <= (signed long int)SHRT_MAX)
    {
      return (short)sop;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_SSHORT)\n");
#endif
    }
  }
  else
  {
    if(op >= 0 && op <= (unsigned long int)SHRT_MAX)
    {
      return (short)op;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_SSHORT)\n");
#endif
    }
  }
}

unsigned short __CHECK_SIMPLE_USHORT(unsigned long int op, int sign)
{
  if(sign)
  {
    signed long int sop = (signed long int)op;
    if(sop >= 0 && sop <= (unsigned short)USHRT_MAX)
    {
      return (unsigned short)sop;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_USHORT)\n");
#endif
    }
  }
  else
  {
    if(op >= 0 && op <= (unsigned short)USHRT_MAX)
    {
      return (unsigned short)op;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_USHORT)\n");
#endif
    }
  }
}

char __CHECK_SIMPLE_SCHAR(unsigned long int op, int sign)
{
  if(sign)
  {
    signed long int sop = (signed long int)op;
    if(sop >= (signed long int)CHAR_MIN && sop <= (signed long int)CHAR_MAX)
    {
      return (char)sop;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_SCHAR)\n");
#endif
    }
  }
  else
  {
    if(op >= 0 && op <= (unsigned long int)CHAR_MAX)
    {
      return (char)op;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_SCHAR)\n");
#endif
    }
  }
}

unsigned char __CHECK_SIMPLE_UCHAR(unsigned long int op, int sign)
{
  if(sign)
  {
    signed long int sop = (signed long int)op;
    if(sop >= 0 && sop <= (signed long int)UCHAR_MAX)
    {
      return (unsigned char)sop;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_UCHAR)\n");
#endif
    }
  }
  else
  {
    if(op >= 0 && op <= (unsigned long int)UCHAR_MAX)
    {
      return (unsigned char)op;
    }
    else
    {
#ifdef PRESSURE_TEST
	  return DIRTY_VALUE;
#else
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_SIMPLE_UCHAR)\n");
#endif
    }
  }
}

void __CHECK_POINTER_PLUS_UL(unsigned long int base, unsigned long int offset)
{
  unsigned long int result = base + offset;
  // check whether overflow occurs
  if(base > result || offset > result)
  {
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_POINTER_PLUS_UL)\n");
  }
}

void __CHECK_POINTER_PLUS_SL(unsigned long int base, long int offset)
{
  // two cases: (1) offset >= 0, the same as the above;
  //            (2) offset < 0, change '+' to '-'
  unsigned long int result = 0;
  if(offset >= 0)
  {
    unsigned long int Poffset = (unsigned long int)offset;
    result = base + Poffset;
    if(base > result || Poffset > result)
    {
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_POINTER_PLUS_SL)\n");
    }
  }
  else
  {
    unsigned long int Noffset = (unsigned long int)(-offset);
    result = base - Noffset;
    if(base < Noffset)
    {
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_POINTER_PLUS_SL)\n");
    }
  }
}

void __CHECK_POINTER_MINUS_UL(unsigned long int base, unsigned long int offset)
{
  if(base < offset)
  {
    __CINTFIX_ERROR("ERROR! (failed check: __CHECK_POINTER_MINUS_UL)\n");
  }
}

void __CHECK_POINTER_MINUS_SL(unsigned long int base, long int offset)
{
  if(offset >= 0)
  {
    unsigned long int Poffset = (unsigned long int)offset;
    if(base < Poffset)
    {
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_POINTER_MINUS_SL)\n");
    }
  }
  else
  {
    unsigned long int Noffset = (unsigned long int)(-offset);
    unsigned long int result = base + Noffset;
    if(base > result || Noffset > result)
    {
      __CINTFIX_ERROR("ERROR! (failed check: __CHECK_POINTER_MINUS_SL)\n");
    }
  }
}

int __CALC_BRANCH_HASH(int n, ...)
{
  va_list vl;
  va_start(vl, n);
  int i = 0;
  int flag = 0;
  int sum = 0;
  for(i = 1; i <= n; i++)
  {
    flag = va_arg(vl, int);
    sum = sum + i * flag;
  }
  return sum;
}

void __CINTFIX_ERROR(const char * errmsg)
{
  fprintf(stderr, errmsg);
  exit(1);
}
