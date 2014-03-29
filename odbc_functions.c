/*

This library is based on the 'extension-functions.c' file at http://www.sqlite.org/contrib/
Ideally it would be used with the ODBC driver from http://www.ch-werner.de/sqliteodbc/ to provide ODBC compatibility

It adds support for a few extra functions that ODBC requires and comments out support for a few functions that aren't needed

*/

#define COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE 1
#define HAVE_ACOSH 1
#define HAVE_ASINH 1
#define HAVE_ATANH 1
#define HAVE_SINH 1
#define HAVE_COSH 1
#define HAVE_TANH 1
#define HAVE_LOG10 1
#define HAVE_ISBLANK 1
#define SQLITE_SOUNDEX 1
#define HAVE_TRIM 1		/* LMH 2007-03-25 if sqlite has trim functions */

#ifdef COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE
#include "sqlite3ext.h"
SQLITE_EXTENSION_INIT1
#else
#include "sqlite3.h"
#endif

#include <ctype.h>
/* relicoder */
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>		/* LMH 2007-03-25 */

#include <stdlib.h>
#include <assert.h>

#define LARGEST_INT64  (0xffffffff|(((i64)0x7fffffff)<<32))

#ifndef _MAP_H_
#define _MAP_H_

#include <stdint.h>

/*
** Simple binary tree implementation to use in median, mode and quartile calculations
** Tree is not necessarily balanced. That would require something like red&black trees of AVL
*/

typedef int(*cmp_func)(const void *, const void *);
typedef void(*map_iterator)(void*, int64_t, void*);

typedef struct node{
  struct node *l;
  struct node *r;
  void* data;
  int64_t count;
} node;

typedef struct map{
  node *base;
  cmp_func cmp;
  short free;
} map;

/*
** creates a map given a comparison function
*/
map map_make(cmp_func cmp);

/*
** inserts the element e into map m
*/
void map_insert(map *m, void *e);

/*
** executes function iter over all elements in the map, in key increasing order
*/
void map_iterate(map *m, map_iterator iter, void* p);

/*
** frees all memory used by a map
*/
void map_destroy(map *m);

/*
** compares 2 integers
** to use with map_make
*/
int int_cmp(const void *a, const void *b);

/*
** compares 2 doubles
** to use with map_make
*/
int double_cmp(const void *a, const void *b);

#endif /* _MAP_H_ */

typedef uint8_t         u8;
typedef uint16_t        u16;
typedef int64_t         i64;

/*
** These are copied verbatim from fun.c so as to not have the names exported
*/

/* LMH from sqlite3 3.3.13 */
/*
** This table maps from the first byte of a UTF-8 character to the number
** of trailing bytes expected. A value '4' indicates that the table key
** is not a legal first byte for a UTF-8 character.
*/
static const u8 xtra_utf8_bytes[256]  = {
/* 0xxxxxxx */
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,

/* 10wwwwww */
4, 4, 4, 4, 4, 4, 4, 4,     4, 4, 4, 4, 4, 4, 4, 4,
4, 4, 4, 4, 4, 4, 4, 4,     4, 4, 4, 4, 4, 4, 4, 4,
4, 4, 4, 4, 4, 4, 4, 4,     4, 4, 4, 4, 4, 4, 4, 4,
4, 4, 4, 4, 4, 4, 4, 4,     4, 4, 4, 4, 4, 4, 4, 4,

/* 110yyyyy */
1, 1, 1, 1, 1, 1, 1, 1,     1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1,     1, 1, 1, 1, 1, 1, 1, 1,

/* 1110zzzz */
2, 2, 2, 2, 2, 2, 2, 2,     2, 2, 2, 2, 2, 2, 2, 2,

/* 11110yyy */
3, 3, 3, 3, 3, 3, 3, 3,     4, 4, 4, 4, 4, 4, 4, 4,
};


/*
** This table maps from the number of trailing bytes in a UTF-8 character
** to an integer constant that is effectively calculated for each character
** read by a naive implementation of a UTF-8 character reader. The code
** in the READ_UTF8 macro explains things best.
*/
static const int xtra_utf8_bits[] =  {
  0,
  12416,          /* (0xC0 << 6) + (0x80) */
  925824,         /* (0xE0 << 12) + (0x80 << 6) + (0x80) */
  63447168        /* (0xF0 << 18) + (0x80 << 12) + (0x80 << 6) + 0x80 */
};

/*
** If a UTF-8 character contains N bytes extra bytes (N bytes follow
** the initial byte so that the total character length is N+1) then
** masking the character with utf8_mask[N] must produce a non-zero
** result.  Otherwise, we have an (illegal) overlong encoding.
*/
static const int utf_mask[] = {
  0x00000000,
  0xffffff80,
  0xfffff800,
  0xffff0000,
};

/* LMH salvaged from sqlite3 3.3.13 source code src/utf.c */
#define READ_UTF8(zIn, c) { \
  int xtra;                                            \
  c = *(zIn)++;                                        \
  xtra = xtra_utf8_bytes[c];                           \
  switch( xtra ){                                      \
    case 4: c = (int)0xFFFD; break;                    \
    case 3: c = (c<<6) + *(zIn)++;                     \
    case 2: c = (c<<6) + *(zIn)++;                     \
    case 1: c = (c<<6) + *(zIn)++;                     \
    c -= xtra_utf8_bits[xtra];                         \
    if( (utf_mask[xtra]&c)==0                          \
        || (c&0xFFFFF800)==0xD800                      \
        || (c&0xFFFFFFFE)==0xFFFE ){  c = 0xFFFD; }    \
  }                                                    \
}

static int sqlite3ReadUtf8(const unsigned char *z){
  int c;
  READ_UTF8(z, c);
  return c;
}

#define SKIP_UTF8(zIn) {                               \
  zIn += (xtra_utf8_bytes[*(u8 *)zIn] + 1);            \
}


/*
** X is a pointer to the first byte of a UTF-8 character.  Increment
** X so that it points to the next character.  This only works right
** if X points to a well-formed UTF-8 string.
*/
#define sqliteNextChar(X)  while( (0xc0&*++(X))==0x80 ){}
#define sqliteCharVal(X)   sqlite3ReadUtf8(X)

/*
** This is a macro that facilitates writting wrappers for math.h functions
** it creates code for a function to use in SQlite that gets one numeric input
** and returns a floating point value.
**
** Could have been implemented using pointers to functions but this way it's inline
** and thus more efficient. Lower * ranking though...
** 
** Parameters:
** name:      function name to de defined (eg: sinFunc)
** function:  function defined in math.h to wrap (eg: sin)
** domain:    boolean condition that CAN'T happen in terms of the input parameter rVal
**            (eg: rval<0 for sqrt)
*/
/* LMH 2007-03-25 Changed to use errno and remove domain; no pre-checking for errors. */
#define GEN_MATH_WRAP_DOUBLE_1(name, function) \
static void name(sqlite3_context *context, int argc, sqlite3_value **argv){\
  double rVal = 0.0, val;\
  assert( argc==1 );\
  switch( sqlite3_value_type(argv[0]) ){\
    case SQLITE_NULL: {\
      sqlite3_result_null(context);\
      break;\
    }\
    default: {\
      rVal = sqlite3_value_double(argv[0]);\
      errno = 0;\
      val = function(rVal);\
      if (errno == 0) {\
        sqlite3_result_double(context, val);\
      } else {\
        sqlite3_result_error(context, strerror(errno), errno);\
      }\
      break;\
    }\
  }\
}\


/*
** Example of GEN_MATH_WRAP_DOUBLE_1 usage
** this creates function sqrtFunc to wrap the math.h standard function sqrt(x)=x^0.5
*/
GEN_MATH_WRAP_DOUBLE_1(sqrtFunc, sqrt)

/* trignometric functions */
GEN_MATH_WRAP_DOUBLE_1(acosFunc, acos)
GEN_MATH_WRAP_DOUBLE_1(asinFunc, asin)
GEN_MATH_WRAP_DOUBLE_1(atanFunc, atan)

/*
** math.h doesn't require cot (cotangent) so it's defined here
*/
static double cot(double x){
  return 1.0/tan(x);
}

GEN_MATH_WRAP_DOUBLE_1(sinFunc, sin)
GEN_MATH_WRAP_DOUBLE_1(cosFunc, cos)
GEN_MATH_WRAP_DOUBLE_1(tanFunc, tan)
GEN_MATH_WRAP_DOUBLE_1(cotFunc, cot)

/*
** Some systems lack log in base 10. This will emulate it
*/

#ifndef HAVE_LOG10
static double log10(double x){
  static double l10 = -1.0;
  if( l10<0.0 ){
    l10 = log(10.0);
  }
  return log(x)/l10;
}
#endif

GEN_MATH_WRAP_DOUBLE_1(logFunc, log)
GEN_MATH_WRAP_DOUBLE_1(log10Func, log10)
GEN_MATH_WRAP_DOUBLE_1(expFunc, exp)

/*
** Fallback for systems where math.h doesn't define M_PI
*/
#undef M_PI
#ifndef M_PI
/*
** static double PI = acos(-1.0);
** #define M_PI (PI)
*/
#define M_PI 3.14159265358979323846
#endif

/* Convert Degrees into Radians */
static double deg2rad(double x){
  return x*M_PI/180.0;
}

/* Convert Radians into Degrees */
static double rad2deg(double x){
  return 180.0*x/M_PI;
}

GEN_MATH_WRAP_DOUBLE_1(rad2degFunc, rad2deg)
GEN_MATH_WRAP_DOUBLE_1(deg2radFunc, deg2rad)

/* constant function that returns the value of PI=3.1415... */
static void piFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  sqlite3_result_double(context, M_PI);
}

/*
** Wraps the pow math.h function
** When both the base and the exponent are integers the result should be integer
** (see sqrt just before this). Here the result is always double
*/
/* LMH 2007-03-25 Changed to use errno; no pre-checking for errors.  Also removes
  but that was present in the pre-checking that called sqlite3_result_error on 
  a non-positive first argument, which is not always an error. */
static void powerFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  double r1 = 0.0;
  double r2 = 0.0;
  double val;

  assert( argc==2 );
  
  if( sqlite3_value_type(argv[0]) == SQLITE_NULL || sqlite3_value_type(argv[1]) == SQLITE_NULL ){
    sqlite3_result_null(context); 
  }else{
    r1 = sqlite3_value_double(argv[0]);
    r2 = sqlite3_value_double(argv[1]);
    errno = 0;
    val = pow(r1,r2);
    if (errno == 0) {
      sqlite3_result_double(context, val);
    } else {  
      sqlite3_result_error(context, strerror(errno), errno);
    }  
  }
}

/*
** Wraps the mod math.h function
** When both the numerator and the divisor are integers the result should be integer
*/
static void modFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  double r1 = 0.0;
  double r2 = 0.0;
  i64 iVal1 = 0;
  i64 iVal2 = 0;
  double val;

  assert( argc==2 );
  
  if( sqlite3_value_type(argv[0]) == SQLITE_NULL || sqlite3_value_type(argv[1]) == SQLITE_NULL ){
    sqlite3_result_null(context);
  }else if( sqlite3_value_type(argv[0]) == SQLITE_INTEGER && sqlite3_value_type(argv[1]) == SQLITE_INTEGER ){
	iVal1 = sqlite3_value_int64(argv[0]);
	iVal2 = sqlite3_value_int64(argv[1]);
	
    sqlite3_result_int64(context, iVal1 % iVal2);
  }else{
    r1 = sqlite3_value_double(argv[0]);
    r2 = sqlite3_value_double(argv[1]);
    errno = 0;
    val = fmod(r1,r2);
    if (errno == 0) {
      sqlite3_result_double(context, val);
    } else {  
      sqlite3_result_error(context, strerror(errno), errno);
    }  
  }
}

/*
** atan2 wrapper
*/
static void atn2Func(sqlite3_context *context, int argc, sqlite3_value **argv){
  double r1 = 0.0;
  double r2 = 0.0;

  assert( argc==2 );
  
  if( sqlite3_value_type(argv[0]) == SQLITE_NULL || sqlite3_value_type(argv[1]) == SQLITE_NULL ){
    sqlite3_result_null(context); 
  }else{
    r1 = sqlite3_value_double(argv[0]);
    r2 = sqlite3_value_double(argv[1]);
    sqlite3_result_double(context, atan2(r1,r2));
  }
}

/*
** Implementation of the sign() function
** return one of 3 possibilities +1,0 or -1 when the argument is respectively
** positive, 0 or negative.
** When the argument is NULL the result is also NULL (completly conventional)
*/
static void signFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  double rVal=0.0;
  i64 iVal=0;
  assert( argc==1 );
  switch( sqlite3_value_type(argv[0]) ){
    case SQLITE_INTEGER: {
      iVal = sqlite3_value_int64(argv[0]);
      iVal = ( iVal > 0) ? 1: ( iVal < 0 ) ? -1: 0;
      sqlite3_result_int64(context, iVal);
      break;
    }
    case SQLITE_NULL: {
      sqlite3_result_null(context);
      break;
    }
    default: {
 /* 2nd change below. Line for abs was: if( rVal<0 ) rVal = rVal * -1.0;  */

      rVal = sqlite3_value_double(argv[0]);
      rVal = ( rVal > 0) ? 1: ( rVal < 0 ) ? -1: 0;
      sqlite3_result_double(context, rVal);
      break;
    }
  }
}


/*
** smallest integer value not less than argument
*/
static void ceilFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  double rVal=0.0;
  i64 iVal=0;
  assert( argc==1 );
  switch( sqlite3_value_type(argv[0]) ){
    case SQLITE_INTEGER: {
      iVal = sqlite3_value_int64(argv[0]);
      sqlite3_result_int64(context, iVal);
      break;
    }
    case SQLITE_NULL: {
      sqlite3_result_null(context);
      break;
    }
    default: {
      rVal = sqlite3_value_double(argv[0]);
      sqlite3_result_int64(context, (i64) ceil(rVal));
      break;
    }
  }
}

/*
** largest integer value not greater than argument
*/
static void floorFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  double rVal=0.0;
  i64 iVal=0;
  assert( argc==1 );
  switch( sqlite3_value_type(argv[0]) ){
    case SQLITE_INTEGER: {
      iVal = sqlite3_value_int64(argv[0]);
      sqlite3_result_int64(context, iVal);
      break;
    }
    case SQLITE_NULL: {
      sqlite3_result_null(context);
      break;
    }
    default: {
      rVal = sqlite3_value_double(argv[0]);
      sqlite3_result_int64(context, (i64) floor(rVal));
      break;
    }
  }
}
/*
** Implementation of the truncate() function, similar to round()
*/
#ifndef SQLITE_OMIT_FLOATING_POINT
static void truncateFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  int n = 0, p;
  double r;
  assert( argc==2 );
  
  if( SQLITE_NULL==sqlite3_value_type(argv[1]) ) return;
  n = sqlite3_value_int(argv[1]);
  if( n>30 ) n = 30;
  if( n<-30 ) n = -30;
  
  if( sqlite3_value_type(argv[0])==SQLITE_NULL ) return;
  r = sqlite3_value_double(argv[0]);
  /* If Y==0 and X will fit in a 64-bit int,
  ** handle the rounding directly,
  ** otherwise use printf.
  */
  if( n==0 && r>=0 && r<LARGEST_INT64-1 ){
    r = (double)((sqlite_int64)(r+0.5));
  }else if( n==0 && r<0 && (-r)<LARGEST_INT64-1 ){
    r = -(double)((sqlite_int64)((-r)+0.5));
  }else{
    if ( n>0 ){
	  p = pow(10, n);
	  r = (double) ((sqlite_int64)(r * p)) / p;
	} else {
      p = pow(10, -n);
      r = (double) ((sqlite_int64)(r / p)) * p;
	}
  }
  sqlite3_result_double(context, r);
}
#endif

/*
** Given a string (s) in the first argument and an integer (n) in the second returns the 
** string that constains s contatenated n times
*/
static void spaceFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  unsigned char *zo;       /* result string */
  i64 iCount;              /* times to repeat */
  i64 i=0;
  
  iCount = sqlite3_value_int64(argv[0]);
  
  if( iCount<0 ){
    sqlite3_result_error(context, "domain error", -1);
  }else{
    zo=sqlite3_malloc(iCount+1);
    if (!zo){
      sqlite3_result_error_nomem(context);
      if (zo) sqlite3_free(zo);
      return;
    }
	
    for(i=0; i<iCount; ++i){
      zo[i] = ' ';
    }
	zo[iCount] = '\0';

    sqlite3_result_text(context, (char*)zo, -1, SQLITE_TRANSIENT);
    sqlite3_free(zo);
  }
}

/*
** Given a string (s) in the first argument and an integer (n) in the second returns the 
** string that constains s contatenated n times
*/
static void replicateFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  unsigned char *z;        /* input string */
  unsigned char *zo;       /* result string */
  i64 iCount;              /* times to repeat */
  i64 nLen;                /* length of the input string (no multibyte considerations) */ 
  i64 nTLen;               /* length of the result string (no multibyte considerations) */
  i64 i=0;

  if( argc!=2 || SQLITE_NULL==sqlite3_value_type(argv[0]) )
    return;

  iCount = sqlite3_value_int64(argv[1]);

  if( iCount<0 ){
    sqlite3_result_error(context, "domain error", -1);
  }else{

    nLen  = sqlite3_value_bytes(argv[0]);
    nTLen = nLen*iCount;
    z=sqlite3_malloc(nTLen+1);
    zo=sqlite3_malloc(nLen+1);
    if (!z || !zo){
      sqlite3_result_error_nomem(context);
      if (z) sqlite3_free(z);
      if (zo) sqlite3_free(zo);
      return;
    }
    strcpy((char*)zo, (char*)sqlite3_value_text(argv[0]));

    for(i=0; i<iCount; ++i){
      strcpy((char*)(z+i*nLen), (char*)zo);
    }

    sqlite3_result_text(context, (char*)zo, -1, SQLITE_TRANSIENT);
    sqlite3_free(z);
    sqlite3_free(zo);
  }
}

/** INSERT( string_exp1, start, length, string_exp2) (ODBC 1.0)
*   Returns a character string where length characters have been deleted from string_exp1, beginning at start, and where string_exp2 has been inserted into string_exp, beginning at start.
*	e.g. INSERT('Goldman', 2, 4, 'xx') => Gxxan
*/
void insertFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  char *z1, *z2, *out;
  int n, n2, start, len;
  
  n = sqlite3_value_bytes(argv[0]);
  z1 = (char*)sqlite3_value_text(argv[0]);
  start = sqlite3_value_int(argv[1]) - 1;
  len = sqlite3_value_int(argv[2]);
  n2 = sqlite3_value_bytes(argv[3]);
  z2 = (char*)sqlite3_value_text(argv[3]);
  
  if (start <= 0 || len < 0) {
    sqlite3_result_null(context);
	return;
  }
  
  out = (char*)sqlite3_malloc(n - len + n2 + 1);
  
  strncpy(out, z1, start);
  strncpy(out + start, z2, n2);
  strncpy(out + start + n2, z1 + start + len, n - start - len);
  
  sqlite3_result_text(context, out, n - len + n2, sqlite3_free);
}


/* 
** Some systems (win32 among others) don't have an isblank function, this will emulate it.
** This function is not UFT-8 safe since it only analyses a byte character.
*/
#ifndef HAVE_ISBLANK
int isblank(char c){
  return( ' '==c || '\t'==c );
}
#endif

/*
** Implementation of the upper() and lower() SQL functions.
*/
static void upperFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  char *z1;
  const char *z2;
  int i, n;
  
  z2 = (char*)sqlite3_value_text(argv[0]);
  n = sqlite3_value_bytes(argv[0]);
  /* Verify that the call to _bytes() does not invalidate the _text() pointer */
  assert( z2==(char*)sqlite3_value_text(argv[0]) );
  if( z2 ){
    z1 = sqlite3_malloc(((i64)n)+1);
    if( z1 ){
      for(i=0; i<n; i++){
        z1[i] = (char)toupper(z2[i]);
      }
      sqlite3_result_text(context, z1, n, sqlite3_free);
    }
  }
}

static void lowerFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  char *z1;
  const char *z2;
  int i, n;
  
  z2 = (char*)sqlite3_value_text(argv[0]);
  n = sqlite3_value_bytes(argv[0]);
  /* Verify that the call to _bytes() does not invalidate the _text() pointer */
  assert( z2==(char*)sqlite3_value_text(argv[0]) );
  if( z2 ){
    z1 = sqlite3_malloc(((i64)n)+1);
    if( z1 ){
      for(i=0; i<n; i++){
        z1[i] = tolower(z2[i]);
      }
      sqlite3_result_text(context, z1, n, sqlite3_free);
    }
  }
}


/*
** Given a string z1, retutns the (0 based) index of it's first occurence
** in z2 after the first s characters.
** Returns -1 when there isn't a match.
** updates p to point to the character where the match occured.
** This is an auxiliary function.
*/
static int _substr(const char* z1, const char* z2, int s, const char** p){
  int c = 0;
  int rVal=-1;
  const char* zt1;
  const char* zt2;
  int c1,c2;

  if( '\0'==*z1 ){
    return -1;
  }
  
  while( (sqliteCharVal((unsigned char *)z2) != 0) && (c++)<s){
    sqliteNextChar(z2);
  }
  
  c = 0;
  while( (sqliteCharVal((unsigned char *)z2)) != 0 ){
    zt1 = z1;
    zt2 = z2;

    do{
      c1 = sqliteCharVal((unsigned char *)zt1);
      c2 = sqliteCharVal((unsigned char *)zt2);
      sqliteNextChar(zt1);
      sqliteNextChar(zt2);
    }while( c1 == c2 && c1 != 0 && c2 != 0 );

    if( c1 == 0 ){
      rVal = c;
      break; 
    }
    
    sqliteNextChar(z2);
    ++c;
  }
  if(p){
    *p=z2;
  }
  return rVal >=0 ? rVal+s : rVal;
}

/*
** Assuming zIn points to the first byte of a UTF-8 character,
** advance zIn to point to the first byte of the next UTF-8 character.
*/
#define SQLITE_SKIP_UTF8(zIn) {                        \
  if( (*(zIn++))>=0xc0 ){                              \
    while( (*zIn & 0xc0)==0x80 ){ zIn++; }             \
  }                                                    \
}

/*
** Implementation of the substr() function.
**
** substr(x,p1,p2)  returns p2 characters of x[] beginning with p1.
** p1 is 1-indexed.  So substr(x,1,1) returns the first character
** of x.  If x is text, then we actually count UTF-8 characters.
** If x is a blob, then we count bytes.
**
** If p1 is negative, then we begin abs(p1) from the end of x[].
**
** If p2 is negative, return the p2 characters preceding p1.
*/
static void substrFunc(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  const unsigned char *z;
  const unsigned char *z2;
  int len;
  int p0type;
  i64 p1, p2;
  int negP2 = 0;

  assert( argc==3 || argc==2 );
  if( sqlite3_value_type(argv[1])==SQLITE_NULL
   || (argc==3 && sqlite3_value_type(argv[2])==SQLITE_NULL)
  ){
    return;
  }
  p0type = sqlite3_value_type(argv[0]);
  p1 = sqlite3_value_int(argv[1]);
  if( p0type==SQLITE_BLOB ){
    len = sqlite3_value_bytes(argv[0]);
    z = sqlite3_value_blob(argv[0]);
    if( z==0 ) return;
    assert( len==sqlite3_value_bytes(argv[0]) );
  }else{
    z = sqlite3_value_text(argv[0]);
    if( z==0 ) return;
    len = 0;
    if( p1<0 ){
      for(z2=z; *z2; len++){
        SQLITE_SKIP_UTF8(z2);
      }
    }
  }
  if( argc==3 ){
    p2 = sqlite3_value_int(argv[2]);
    if( p2<0 ){
      p2 = -p2;
      negP2 = 1;
    }
  }else{
	p2 = sqlite3_limit(sqlite3_context_db_handle(context), SQLITE_LIMIT_LENGTH, -1);
  }
  if( p1<0 ){
    p1 += len;
    if( p1<0 ){
      p2 += p1;
      if( p2<0 ) p2 = 0;
      p1 = 0;
    }
  }else if( p1>0 ){
    p1--;
  }else if( p2>0 ){
    p2--;
  }
  if( negP2 ){
    p1 -= p2;
    if( p1<0 ){
      p2 += p1;
      p1 = 0;
    }
  }
  assert( p1>=0 && p2>=0 );
  if( p0type!=SQLITE_BLOB ){
    while( *z && p1 ){
      SQLITE_SKIP_UTF8(z);
      p1--;
    }
    for(z2=z; *z2 && p2; p2--){
      SQLITE_SKIP_UTF8(z2);
    }
    sqlite3_result_text(context, (char*)z, (int)(z2-z), SQLITE_TRANSIENT);
  }else{
    if( p1+p2>len ){
      p2 = len-p1;
      if( p2<0 ) p2 = 0;
    }
    sqlite3_result_blob(context, (char*)&z[p1], (int)p2, SQLITE_TRANSIENT);
  }
}

/*
** given 2 input strings (s1,s2) and an integer (n) searches from the nth character
** for the string s1. Returns the position where the match occured.
** Characters are counted from 1.
** 0 is returned when no match occurs.
*/

static void charindexFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const u8 *z1;          /* s1 string */
  u8 *z2;                /* s2 string */
  int s=0;
  int rVal=0;

  assert( argc==3 ||argc==2);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) || SQLITE_NULL==sqlite3_value_type(argv[1])){
    sqlite3_result_null(context);
    return;
  }

  z1 = sqlite3_value_text(argv[0]);
  if( z1==0 ) return;
  z2 = (u8*) sqlite3_value_text(argv[1]);
  if(argc==3){
    s = sqlite3_value_int(argv[2])-1;
    if(s<0){
      s=0;
    }
  }else{
    s = 0;
  }

  rVal = _substr((char *)z1,(char *)z2,s,NULL);
  sqlite3_result_int(context, rVal+1);
}


/**
 * Concatenates values.
 */
static void concatFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
    size_t lens[10];
    size_t lenall = 0;
    size_t lentilnow = 0;

    int i = 0;

    for(i = 0; i < argc; i++)
    {
        lens[i] = strlen((char *)sqlite3_value_text(argv[i]));
        lenall += lens[i];
    }

    char* all = (char*)sqlite3_malloc(lenall + 1);
    all[lenall - 1] = '\0';

    for(i = 0; i < argc; i++)
    {
        memcpy(all + lentilnow, sqlite3_value_text(argv[i]), lens[i]);
        lentilnow += lens[i];
    }

    sqlite3_result_text(context, all, lenall, SQLITE_TRANSIENT);
    sqlite3_free(all);
}

/*
** The ascii() function.  Return the integer ascii code-point value
** for the first character of the input string. 
*/
static void asciiFunc(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  const unsigned char *z = sqlite3_value_text(argv[0]);
  (void)argc;
  if( z && z[0] ) sqlite3_result_int(context, (int) &z);
}

/*
** given a string (s) and an integer (n) returns the n leftmost (UTF-8) characters
** if the string has a length<=n or is NULL this function is NOP
*/
static void leftFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  int c=0;
  int cc=0;
  int l=0;
  const unsigned char *z;       /* input string */
  const unsigned char *zt;
  unsigned char *rz;            /* output string */

  assert( argc==2);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) || SQLITE_NULL==sqlite3_value_type(argv[1])){
    sqlite3_result_null(context);
    return;
  }

  z  = sqlite3_value_text(argv[0]);
  l  = sqlite3_value_int(argv[1]);
  zt = z;

  while( sqliteCharVal(zt) && c++<l)
    sqliteNextChar(zt);

  cc=zt-z;

  rz = sqlite3_malloc(zt-z+1);
  if (!rz){
    sqlite3_result_error_nomem(context);
    return;
  }
  strncpy((char*) rz, (char*) z, zt-z);
  *(rz+cc) = '\0';
  sqlite3_result_text(context, (char*)rz, -1, SQLITE_TRANSIENT); 
  sqlite3_free(rz);
}

/*
** given a string (s) and an integer (n) returns the n rightmost (UTF-8) characters
** if the string has a length<=n or is NULL this function is NOP
*/
static void rightFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  int l=0;
  int c=0;
  int cc=0;
  const char *z;
  const char *zt;
  const char *ze;
  char *rz;

  assert( argc==2);

  if( SQLITE_NULL == sqlite3_value_type(argv[0]) || SQLITE_NULL == sqlite3_value_type(argv[1])){
    sqlite3_result_null(context);
    return;
  }

  z  = (char *)sqlite3_value_text(argv[0]);
  l  = sqlite3_value_int(argv[1]);
  zt = z;

  while( sqliteCharVal((unsigned char *)zt)!=0){
    sqliteNextChar(zt);
    ++c;
  }

  ze = zt;
  zt = z;

  cc=c-l;
  if(cc<0)
    cc=0;
  
  while( cc-- > 0 ){
    sqliteNextChar(zt);
  }

  rz = sqlite3_malloc(ze-zt+1);
  if (!rz){
    sqlite3_result_error_nomem(context);
    return;
  }
  strcpy((char*) rz, (char*) (zt));
  sqlite3_result_text(context, (char*)rz, -1, SQLITE_TRANSIENT); 
  sqlite3_free(rz);
}

#ifndef HAVE_TRIM
/*
** removes the whitespaces at the begining of a string.
*/
const char* ltrim(const char* s){
  while( *s==' ' )
    ++s;
  return s;
}

/*
** removes the whitespaces at the end of a string.
** !mutates the input string!
*/
void rtrim(char* s){
  char* ss = s+strlen(s)-1;
  while( ss>=s && *ss==' ' )
    --ss;
  *(ss+1)='\0';
}

/*
**  Removes the whitespace at the begining of a string
*/
static void ltrimFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *z;

  assert( argc==1);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }
  z = sqlite3_value_text(argv[0]);
  sqlite3_result_text(context, ltrim(z), -1, SQLITE_TRANSIENT); 
}

/*
**  Removes the whitespace at the end of a string
*/
static void rtrimFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *z;
  char *rz;
  /* try not to change data in argv */

  assert( argc==1);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }
  z = sqlite3_value_text(argv[0]);
  rz = sqlite3StrDup(z);
  rtrim(rz);
  sqlite3_result_text(context, rz, -1, SQLITE_TRANSIENT);
  sqlite3_free(rz);
}

/*
**  Removes the whitespace at the begining and end of a string
*/
static void trimFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *z;
  char *rz;
  /* try not to change data in argv */

  assert( argc==1);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }
  z = sqlite3_value_text(argv[0]);
  rz = sqlite3StrDup(z);
  rtrim(rz);
  sqlite3_result_text(context, ltrim(rz), -1, SQLITE_TRANSIENT);
  sqlite3_free(rz);
}
#endif

/*
** given a pointer to a string s1, the length of that string (l1), a new string (s2)
** and it's length (l2) appends s2 to s1.
** All lengths in bytes.
** This is just an auxiliary function
*/
// static void _append(char **s1, int l1, const char *s2, int l2){
//   *s1 = realloc(*s1, (l1+l2+1)*sizeof(char));
//   strncpy((*s1)+l1, s2, l2);
//   *(*(s1)+l1+l2) = '\0';
// }

#ifndef HAVE_TRIM

/*
** given strings s, s1 and s2 replaces occurrences of s1 in s by s2
*/
static void replaceFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *z1;     /* string s (first parameter) */
  const char *z2;     /* string s1 (second parameter) string to look for */
  const char *z3;     /* string s2 (third parameter) string to replace occurrences of s1 with */
  int lz1;
  int lz2;
  int lz3;
  int lzo=0;
  char *zo=0;
  int ret=0;
  const char *zt1;
  const char *zt2;

  assert( 3==argc );

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }

  z1 = sqlite3_value_text(argv[0]);
  z2 = sqlite3_value_text(argv[1]);
  z3 = sqlite3_value_text(argv[2]);
  /* handle possible null values */
  if( 0==z2 ){
    z2="";
  }
  if( 0==z3 ){
    z3="";
  }

  lz1 = strlen(z1);
  lz2 = strlen(z2);
  lz3 = strlen(z3);

#if 0
  /* special case when z2 is empty (or null) nothing will be changed */
  if( 0==lz2 ){
    sqlite3_result_text(context, z1, -1, SQLITE_TRANSIENT);
    return;
  }
#endif

  zt1=z1;
  zt2=z1;

  while(1){
    ret=_substr(z2,zt1 , 0, &zt2);

    if( ret<0 )
      break;

    _append(&zo, lzo, zt1, zt2-zt1);
    lzo+=zt2-zt1;
    _append(&zo, lzo, z3, lz3);
    lzo+=lz3;

    zt1=zt2+lz2;
  }
  _append(&zo, lzo, zt1, lz1-(zt1-z1));
  sqlite3_result_text(context, zo, -1, SQLITE_TRANSIENT);
  sqlite3_free(zo);
}
#endif


#ifdef SQLITE_SOUNDEX

/* relicoder factored code */
/*
** Calculates the soundex value of a string
*/

static void soundex(const u8 *zIn, char *zResult){
  int i, j;
  static const unsigned char iCode[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 2, 3, 0, 1, 2, 0, 0, 2, 2, 4, 5, 5, 0,
    1, 2, 6, 2, 3, 0, 1, 0, 2, 0, 2, 0, 0, 0, 0, 0,
    0, 0, 1, 2, 3, 0, 1, 2, 0, 0, 2, 2, 4, 5, 5, 0,
    1, 2, 6, 2, 3, 0, 1, 0, 2, 0, 2, 0, 0, 0, 0, 0,
  };

  for(i=0; zIn[i] && !isalpha(zIn[i]); i++){}
  if( zIn[i] ){
    zResult[0] = toupper(zIn[i]);
    for(j=1; j<4 && zIn[i]; i++){
      int code = iCode[zIn[i]&0x7f];
      if( code>0 ){
        zResult[j++] = code + '0';
      }
    }
    while( j<4 ){
      zResult[j++] = '0';
    }
    zResult[j] = 0;
  }else{
    strcpy(zResult, "?000");
  }
}

/*
** computes the number of different characters between the soundex value fo 2 strings
*/
static void differenceFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  char zResult1[8];
  char zResult2[8];
  char *zR1 = zResult1;
  char *zR2 = zResult2;
  int rVal = 0;
  int i = 0;
  const u8 *zIn1;
  const u8 *zIn2;

  assert( argc==2 );
  
  if( sqlite3_value_type(argv[0])==SQLITE_NULL || sqlite3_value_type(argv[1])==SQLITE_NULL ){
    sqlite3_result_null(context);
    return;
  }
  
  zIn1 = (u8*)sqlite3_value_text(argv[0]);
  zIn2 = (u8*)sqlite3_value_text(argv[1]);

  soundex(zIn1, zR1);
  soundex(zIn2, zR2);

  for(i=0; i<4; ++i){
    if( sqliteCharVal((unsigned char *)zR1)==sqliteCharVal((unsigned char *)zR2) )
      ++rVal;
    sqliteNextChar(zR1);
    sqliteNextChar(zR2);
  }
  sqlite3_result_int(context, rVal);
}
#endif

#ifndef SQLITE_OMIT_DATETIME_FUNCS
#include <time.h>

/* Date functions pinched from the core sqlite3 code */


/*
** A structure for holding a single date and time.
*/
typedef struct DateTime DateTime;
struct DateTime {
  sqlite3_int64 iJD; /* The julian day number times 86400000 */
  int Y, M, D;       /* Year, month, and day */
  int h, m;          /* Hour and minutes */
  int tz;            /* Timezone offset in minutes */
  double s;          /* Seconds */
  char validYMD;     /* True (1) if Y,M,D are valid */
  char validHMS;     /* True (1) if h,m,s are valid */
  char validJD;      /* True (1) if iJD is valid */
  char validTZ;      /* True (1) if tz is valid */
};


/*
** Convert zDate into one or more integers.  Additional arguments
** come in groups of 5 as follows:
**
**       N       number of digits in the integer
**       min     minimum allowed value of the integer
**       max     maximum allowed value of the integer
**       nextC   first character after the integer
**       pVal    where to write the integers value.
**
** Conversions continue until one with nextC==0 is encountered.
** The function returns the number of successful conversions.
*/
static int getDigits(const char *zDate, ...){
  va_list ap;
  int val;
  int N;
  int min;
  int max;
  int nextC;
  int *pVal;
  int cnt = 0;
  va_start(ap, zDate);
  do{
    N = va_arg(ap, int);
    min = va_arg(ap, int);
    max = va_arg(ap, int);
    nextC = va_arg(ap, int);
    pVal = va_arg(ap, int*);
    val = 0;
    while( N-- ){
      if( !isdigit(*zDate) ){
        goto end_getDigits;
      }
      val = val*10 + *zDate - '0';
      zDate++;
    }
    if( val<min || val>max || (nextC!=0 && nextC!=*zDate) ){
      goto end_getDigits;
    }
    *pVal = val;
    zDate++;
    cnt++;
  }while( nextC );
end_getDigits:
  va_end(ap);
  return cnt;
}

/*
** Parse a timezone extension on the end of a date-time.
** The extension is of the form:
**
**        (+/-)HH:MM
**
** Or the "zulu" notation:
**
**        Z
**
** If the parse is successful, write the number of minutes
** of change in p->tz and return 0.  If a parser error occurs,
** return non-zero.
**
** A missing specifier is not considered an error.
*/
static int parseTimezone(const char *zDate, DateTime *p){
  int sgn = 0;
  int nHr, nMn;
  int c;
  while( isspace(*zDate) ){ zDate++; }
  p->tz = 0;
  c = *zDate;
  if( c=='-' ){
    sgn = -1;
  }else if( c=='+' ){
    sgn = +1;
  }else if( c=='Z' || c=='z' ){
    zDate++;
    goto zulu_time;
  }else{
    return c!=0;
  }
  zDate++;
  if( getDigits(zDate, 2, 0, 14, ':', &nHr, 2, 0, 59, 0, &nMn)!=2 ){
    return 1;
  }
  zDate += 5;
  p->tz = sgn*(nMn + nHr*60);
zulu_time:
  while( isspace(*zDate) ){ zDate++; }
  return *zDate!=0;
}

/*
** Parse times of the form HH:MM or HH:MM:SS or HH:MM:SS.FFFF.
** The HH, MM, and SS must each be exactly 2 digits.  The
** fractional seconds FFFF can be one or more digits.
**
** Return 1 if there is a parsing error and 0 on success.
*/
static int parseHhMmSs(const char *zDate, DateTime *p){
  int h, m, s;
  double ms = 0.0;
  if( getDigits(zDate, 2, 0, 24, ':', &h, 2, 0, 59, 0, &m)!=2 ){
    return 1;
  }
  zDate += 5;
  if( *zDate==':' ){
    zDate++;
    if( getDigits(zDate, 2, 0, 59, 0, &s)!=1 ){
      return 1;
    }
    zDate += 2;
    if( *zDate=='.' && isdigit(zDate[1]) ){
      double rScale = 1.0;
      zDate++;
      while( isdigit(*zDate) ){
        ms = ms*10.0 + *zDate - '0';
        rScale *= 10.0;
        zDate++;
      }
      ms /= rScale;
    }
  }else{
    s = 0;
  }
  p->validJD = 0;
  p->validHMS = 1;
  p->h = h;
  p->m = m;
  p->s = s + ms;
  if( parseTimezone(zDate, p) ) return 1;
  p->validTZ = (p->tz!=0)?1:0;
  return 0;
}

/*
** Convert from YYYY-MM-DD HH:MM:SS to julian day.  We always assume
** that the YYYY-MM-DD is according to the Gregorian calendar.
**
** Reference:  Meeus page 61
*/
static void computeJD(DateTime *p){
  int Y, M, D, A, B, X1, X2;

  if( p->validJD ) return;
  if( p->validYMD ){
    Y = p->Y;
    M = p->M;
    D = p->D;
  }else{
    Y = 2000;  /* If no YMD specified, assume 2000-Jan-01 */
    M = 1;
    D = 1;
  }
  if( M<=2 ){
    Y--;
    M += 12;
  }
  A = Y/100;
  B = 2 - A + (A/4);
  X1 = 36525*(Y+4716)/100;
  X2 = 306001*(M+1)/10000;
  p->iJD = (sqlite3_int64)((X1 + X2 + D + B - 1524.5 ) * 86400000);
  p->validJD = 1;
  if( p->validHMS ){
    p->iJD += p->h*3600000 + p->m*60000 + (sqlite3_int64)(p->s*1000);
    if( p->validTZ ){
      p->iJD -= p->tz*60000;
      p->validYMD = 0;
      p->validHMS = 0;
      p->validTZ = 0;
    }
  }
}

/*
** Parse dates of the form
**
**     YYYY-MM-DD HH:MM:SS.FFF
**     YYYY-MM-DD HH:MM:SS
**     YYYY-MM-DD HH:MM
**     YYYY-MM-DD
**
** Write the result into the DateTime structure and return 0
** on success and 1 if the input string is not a well-formed
** date.
*/
static int parseYyyyMmDd(const char *zDate, DateTime *p){
  int Y, M, D, neg;

  if( zDate[0]=='-' ){
    zDate++;
    neg = 1;
  }else{
    neg = 0;
  }
  if( getDigits(zDate,4,0,9999,'-',&Y,2,1,12,'-',&M,2,1,31,0,&D)!=3 ){
    return 1;
  }
  zDate += 10;
  while( isspace(*zDate) || 'T'==*(u8*)zDate ){ zDate++; }
  if( parseHhMmSs(zDate, p)==0 ){
    /* We got the time */
  }else if( *zDate==0 ){
    p->validHMS = 0;
  }else{
    return 1;
  }
  p->validJD = 0;
  p->validYMD = 1;
  p->Y = neg ? -Y : Y;
  p->M = M;
  p->D = D;
  if( p->validTZ ){
    computeJD(p);
  }
  return 0;
}

static int sqlite3OsCurrentTimeInt64(sqlite3_vfs *pVfs, sqlite3_int64 *pTimeOut){
  int rc;
  /* IMPLEMENTATION-OF: R-49045-42493 SQLite will use the xCurrentTimeInt64()
  ** method to get the current date and time if that method is available
  ** (if iVersion is 2 or greater and the function pointer is not NULL) and
  ** will fall back to xCurrentTime() if xCurrentTimeInt64() is
  ** unavailable.
  */
  if( pVfs->iVersion>=2 && pVfs->xCurrentTimeInt64 ){
    rc = pVfs->xCurrentTimeInt64(pVfs, pTimeOut);
  }else{
    double r;
    rc = pVfs->xCurrentTime(pVfs, &r);
    *pTimeOut = (sqlite3_int64)(r*86400000.0);
  }
  return rc;
}

/*
** Compute the Year, Month, and Day from the julian day number.
*/
static void computeYMD(DateTime *p){
  int Z, A, B, C, D, E, X1;
  if( p->validYMD ) return;
  if( !p->validJD ){
    p->Y = 2000;
    p->M = 1;
    p->D = 1;
  }else{
    Z = (int)((p->iJD + 43200000)/86400000);
    A = (int)((Z - 1867216.25)/36524.25);
    A = Z + 1 + A - (A/4);
    B = A + 1524;
    C = (int)((B - 122.1)/365.25);
    D = (36525*C)/100;
    E = (int)((B-D)/30.6001);
    X1 = (int)(30.6001*E);
    p->D = B - D - X1;
    p->M = E<14 ? E-1 : E-13;
    p->Y = p->M>2 ? C - 4716 : C - 4715;
  }
  p->validYMD = 1;
}

/*
** Compute the Hour, Minute, and Seconds from the julian day number.
*/
static void computeHMS(DateTime *p){
  int s;
  if( p->validHMS ) return;
  computeJD(p);
  s = (int)((p->iJD + 43200000) % 86400000);
  p->s = s/1000.0;
  s = (int)p->s;
  p->s -= s;
  p->h = s/3600;
  s -= p->h*3600;
  p->m = s/60;
  p->s += s - p->m*60;
  p->validHMS = 1;
}

/*
** Compute both YMD and HMS
*/
static void computeYMD_HMS(DateTime *p){
  computeYMD(p);
  computeHMS(p);
}


/*
** Clear the YMD and HMS and the TZ
*/
static void clearYMD_HMS_TZ(DateTime *p){
  p->validYMD = 0;
  p->validHMS = 0;
  p->validTZ = 0;
}

/*
** On recent Windows platforms, the localtime_s() function is available
** as part of the "Secure CRT". It is essentially equivalent to 
** localtime_r() available under most POSIX platforms, except that the 
** order of the parameters is reversed.
**
** See http://msdn.microsoft.com/en-us/library/a442x3ye(VS.80).aspx.
**
** If the user has not indicated to use localtime_r() or localtime_s()
** already, check for an MSVC build environment that provides 
** localtime_s().
*/
#if !defined(HAVE_LOCALTIME_R) && !defined(HAVE_LOCALTIME_S) && \
     defined(_MSC_VER) && defined(_CRT_INSECURE_DEPRECATE)
#define HAVE_LOCALTIME_S 1
#endif

#ifndef SQLITE_OMIT_LOCALTIME
/*
** The following routine implements the rough equivalent of localtime_r()
** using whatever operating-system specific localtime facility that
** is available.  This routine returns 0 on success and
** non-zero on any kind of error.
*/
static int osLocaltime(time_t *t, struct tm *pTm){
  int rc;
#if (!defined(HAVE_LOCALTIME_R) || !HAVE_LOCALTIME_R) \
      && (!defined(HAVE_LOCALTIME_S) || !HAVE_LOCALTIME_S)
  struct tm *pX;
//#if SQLITE_THREADSAFE>0
  sqlite3_mutex *mutex = sqlite3_mutex_alloc(SQLITE_MUTEX_STATIC_MASTER);
//#endif
  sqlite3_mutex_enter(mutex);
  pX = localtime(t);
  if( pX ) *pTm = *pX;
  sqlite3_mutex_leave(mutex);
  rc = pX==0;
#else
#if defined(HAVE_LOCALTIME_R) && HAVE_LOCALTIME_R
  rc = localtime_r(t, pTm)==0;
#else
  rc = localtime_s(pTm, t);
#endif /* HAVE_LOCALTIME_R */
#endif /* HAVE_LOCALTIME_R || HAVE_LOCALTIME_S */
  return rc;
}
#endif /* SQLITE_OMIT_LOCALTIME */


#ifndef SQLITE_OMIT_LOCALTIME
/*
** Compute the difference (in milliseconds) between localtime and UTC
** (a.k.a. GMT) for the time value p where p is in UTC. If no error occurs,
** return this value and set *pRc to SQLITE_OK. 
**
** Or, if an error does occur, set *pRc to SQLITE_ERROR. The returned value
** is undefined in this case.
*/
static sqlite3_int64 localtimeOffset(
  DateTime *p,                    /* Date at which to calculate offset */
  sqlite3_context *pCtx,          /* Write error here if one occurs */
  int *pRc                        /* OUT: Error code. SQLITE_OK or ERROR */
){
  DateTime x, y;
  time_t t;
  struct tm sLocal;

  /* Initialize the contents of sLocal to avoid a compiler warning. */
  memset(&sLocal, 0, sizeof(sLocal));

  x = *p;
  computeYMD_HMS(&x);
  if( x.Y<1971 || x.Y>=2038 ){
    x.Y = 2000;
    x.M = 1;
    x.D = 1;
    x.h = 0;
    x.m = 0;
    x.s = 0.0;
  } else {
    int s = (int)(x.s + 0.5);
    x.s = s;
  }
  x.tz = 0;
  x.validJD = 0;
  computeJD(&x);
  t = (time_t)(x.iJD/1000 - 21086676*(i64)10000);
  if( osLocaltime(&t, &sLocal) ){
    sqlite3_result_error(pCtx, "local time unavailable", -1);
    *pRc = SQLITE_ERROR;
    return 0;
  }
  y.Y = sLocal.tm_year + 1900;
  y.M = sLocal.tm_mon + 1;
  y.D = sLocal.tm_mday;
  y.h = sLocal.tm_hour;
  y.m = sLocal.tm_min;
  y.s = sLocal.tm_sec;
  y.validYMD = 1;
  y.validHMS = 1;
  y.validJD = 0;
  y.validTZ = 0;
  computeJD(&y);
  *pRc = SQLITE_OK;
  return y.iJD - x.iJD;
}
#endif /* SQLITE_OMIT_LOCALTIME */


/*
** Set the time to the current time reported by the VFS.
**
** Return the number of errors.
*/
static int setDateTimeToLocalCurrent(sqlite3_context *context, DateTime *p){
  int rc = 1;
  if( sqlite3OsCurrentTimeInt64(sqlite3_vfs_find(NULL), &p->iJD)==SQLITE_OK ){
    p->validJD = 1;
	p->iJD += localtimeOffset(p, context, &rc);
	clearYMD_HMS_TZ(p);
	if (rc == SQLITE_OK)
		return 0;
  }
  return 1;
}

/*
** Attempt to parse the given string into a Julian Day Number.  Return
** the number of errors.
**
** The following are acceptable forms for the input string:
**
**      YYYY-MM-DD HH:MM:SS.FFF  +/-HH:MM
**      DDDD.DD 
**      now
**
** In the first form, the +/-HH:MM is always optional.  The fractional
** seconds extension (the ".FFF") is optional.  The seconds portion
** (":SS.FFF") is option.  The year and date can be omitted as long
** as there is a time string.  The time string can be omitted as long
** as there is a year and date.
*/
static int parseDateOrTime(
  sqlite3_context *context, 
  const char *zDate, 
  DateTime *p
){
  if( parseYyyyMmDd(zDate,p)==0 ){
    return 0;
  }else if( parseHhMmSs(zDate, p)==0 ){
    return 0;
  }else if( sqlite3_stricmp(zDate,"now")==0){
    return setDateTimeToLocalCurrent(context, p);
  }
  return 1;
}

/*
** current_time()
**
** This function returns the same value as time('now').
*/
static void ctimeFunc(
  sqlite3_context *context,
  int NotUsed,
  sqlite3_value **NotUsed2
){
  DateTime x;
  memset(&x, 0, sizeof(x));
  
  if( setDateTimeToLocalCurrent(context, &x)==0){
    char zBuf[100];
    computeHMS(&x);
    sqlite3_snprintf(sizeof(zBuf), zBuf, "%02d:%02d:%02d", x.h, x.m, (int)x.s);
    sqlite3_result_text(context, zBuf, -1, SQLITE_TRANSIENT);
  }
}

/*
** current_date()
**
** This function returns the same value as date('now').
*/
static void cdateFunc(
  sqlite3_context *context,
  int NotUsed,
  sqlite3_value **NotUsed2
){
  DateTime x;
  memset(&x, 0, sizeof(x));
  
  if( setDateTimeToLocalCurrent(context, &x)==0){
    char zBuf[100];
    computeYMD(&x);
    sqlite3_snprintf(sizeof(zBuf), zBuf, "%04d-%02d-%02d", x.Y, x.M, x.D);
    sqlite3_result_text(context, zBuf, -1, SQLITE_TRANSIENT);
  }
}

/*
** current_timestamp()
**
** This function returns the same value as datetime('now').
*/
static void ctimestampFunc(
  sqlite3_context *context,
  int NotUsed,
  sqlite3_value **NotUsed2
){
  DateTime x;
  memset(&x, 0, sizeof(x));
  
  if( setDateTimeToLocalCurrent(context, &x)==0){
    char zBuf[100];
    computeYMD_HMS(&x);
    sqlite3_snprintf(sizeof(zBuf), zBuf, "%04d-%02d-%02d %02d:%02d:%02d",
                     x.Y, x.M, x.D, x.h, x.m, (int)(x.s));
    sqlite3_result_text(context, zBuf, -1, SQLITE_TRANSIENT);
  }
}

static void
dayofmonthFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p;
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeYMD_HMS(&p);
  
  sqlite3_result_int64(context, p.D);
}

static void
dayofweekFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p;
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeJD(&p);
  
  sqlite3_result_int64(context, (((p.iJD+129600000)/86400000) % 7) + 1);
}

static void
dayofyearFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p, y;
  int nDay;
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeJD(&p);
  
  y = p;
  y.validJD = 0;
  y.M = 1;
  y.D = 1;
  computeJD(&y);
  nDay = (int)((p.iJD-y.iJD+43200000)/86400000);
  
  sqlite3_result_int64(context, nDay);
}

static void
hourFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p;
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeYMD_HMS(&p);
  
  sqlite3_result_int64(context, p.h);
}

static void
minuteFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p;
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeYMD_HMS(&p);
  
  sqlite3_result_int64(context, p.m);
}

static void
monthFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p;
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeYMD_HMS(&p);
  
  sqlite3_result_int64(context, p.M);
}

static void
quarterFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p;
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeYMD_HMS(&p);
  
  sqlite3_result_int64(context, p.M / 4 + 1);
}

static void
secondFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p;
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeYMD_HMS(&p);
  
  sqlite3_result_int64(context, p.s);
}

static void
weekFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p, y;
  int nDay;
  int wd;   /* 0=Monday, 1=Tuesday, ... 6=Sunday */
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeJD(&p);
  
  y = p;
  y.validJD = 0;
  y.M = 1;
  y.D = 1;
  computeJD(&y);
  nDay = (int)((p.iJD-y.iJD+43200000)/86400000);
  wd = (int)(((p.iJD+43200000)/86400000)%7);
  
  sqlite3_result_int64(context, (nDay+7-wd)/7 + 1);
}

static void
yearFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  DateTime p;
  const unsigned char *z;
  
  memset(&p, 0, sizeof(p));
  
  z = sqlite3_value_text(argv[0]);
  if( !z || parseDateOrTime(context, (char*)z, &p) ){
    sqlite3_result_error(context, "date/time parsing error", -1);
	return;
  }
  
  computeYMD_HMS(&p);
  
  sqlite3_result_int64(context, p.Y);
}

#endif

static void
userFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  sqlite3_result_text(context, "", 0, SQLITE_TRANSIENT);
}

static void
databaseFunc(sqlite3_context *context, int argc, sqlite3_value **argv)
{
  sqlite3_result_text(context, "main", -1, SQLITE_TRANSIENT);
}

/*
** Implementation of random().  Return a random integer.  
*/
static void randomFunc(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  sqlite_int64 r;
  sqlite3_randomness(sizeof(r), &r);
  if( r<0 ){
    /* We need to prevent a random number of 0x8000000000000000 
    ** (or -9223372036854775808) since when you do abs() of that
    ** number of you get the same value back again.  To do this
    ** in a way that is testable, mask the sign bit off of negative
    ** values, resulting in a positive value.  Then take the 
    ** 2s complement of that positive value.  The end result can
    ** therefore be no less than -9223372036854775807.
    */
    r = -(r & LARGEST_INT64);
  }
  sqlite3_result_int64(context, r);
}

/*
** This function registered all of the above C functions as SQL
** functions.  This should be the only routine in this file with
** external linkage.
*/
int RegisterExtensionFunctions(sqlite3 *db){
  static const struct FuncDef {
     char *zName;
     signed char nArg;
     u8 argType;           /* 0: none.  1: db  2: (-1) */
     u8 eTextRep;          /* 1: UTF-16.  0: UTF-8 */
     u8 needCollSeq;
     void (*xFunc)(sqlite3_context*,int,sqlite3_value **);
  } aFuncs[] = {
    /* math.h */
    { "acos",               1, 0, SQLITE_UTF8,    0, acosFunc  },
    { "asin",               1, 0, SQLITE_UTF8,    0, asinFunc  },
    { "atan",               1, 0, SQLITE_UTF8,    0, atanFunc  },
    { "atan2",              2, 0, SQLITE_UTF8,    0, atn2Func  },

    { "difference",         2, 0, SQLITE_UTF8,    0, differenceFunc},
    { "degrees",            1, 0, SQLITE_UTF8,    0, rad2degFunc  },
    { "radians",            1, 0, SQLITE_UTF8,    0, deg2radFunc  },

    { "cos",                1, 0, SQLITE_UTF8,    0, cosFunc  },
    { "sin",                1, 0, SQLITE_UTF8,    0, sinFunc },
    { "tan",                1, 0, SQLITE_UTF8,    0, tanFunc },
    { "cot",                1, 0, SQLITE_UTF8,    0, cotFunc },

    { "exp",                1, 0, SQLITE_UTF8,    0, expFunc  },
    { "log",                1, 0, SQLITE_UTF8,    0, logFunc  },
    { "log10",              1, 0, SQLITE_UTF8,    0, log10Func  },
    { "power",              2, 0, SQLITE_UTF8,    0, powerFunc  },
    { "mod",                2, 0, SQLITE_UTF8,    0, modFunc  },
    { "sign",               1, 0, SQLITE_UTF8,    0, signFunc },
    { "sqrt",               1, 0, SQLITE_UTF8,    0, sqrtFunc },

    { "ceiling",            1, 0, SQLITE_UTF8,    0, ceilFunc },
    { "floor",              1, 0, SQLITE_UTF8,    0, floorFunc },

    { "pi",                 0, 0, SQLITE_UTF8,    1, piFunc },
	
	{ "rand",               0, 0, SQLITE_UTF8,    0, randomFunc },
	{ "rand",               1, 0, SQLITE_UTF8,    0, randomFunc },
	
#ifndef SQLITE_OMIT_FLOATING_POINT
	{ "truncate",           2, 0, SQLITE_UTF8,    0, truncateFunc },
#endif

    /* string */
    { "ascii",              1, 0, SQLITE_UTF8,    0, asciiFunc },
    { "repeat",             2, 0, SQLITE_UTF8,    0, replicateFunc },
    { "space",              1, 0, SQLITE_UTF8,    0, spaceFunc },
    { "locate",             2, 0, SQLITE_UTF8,    0, charindexFunc },
    { "locate",             3, 0, SQLITE_UTF8,    0, charindexFunc },
    { "substring",          2, 0, SQLITE_UTF8,    0, substrFunc },
    { "substring",          3, 0, SQLITE_UTF8,    0, substrFunc },
	
    { "left",               2, 0, SQLITE_UTF8,    0, leftFunc },
    { "right",              2, 0, SQLITE_UTF8,    0, rightFunc },
    { "concat",            -1, 0, SQLITE_UTF8,    0, concatFunc },
	{ "insert",             4, 0, SQLITE_UTF8,    0, insertFunc },
	
#ifndef HAVE_TRIM
    { "ltrim",              1, 0, SQLITE_UTF8,    0, ltrimFunc },
    { "rtrim",              1, 0, SQLITE_UTF8,    0, rtrimFunc },
    { "trim",               1, 0, SQLITE_UTF8,    0, trimFunc },
    { "replace",            3, 0, SQLITE_UTF8,    0, replaceFunc },
#endif
    { "ucase",              1, 0, SQLITE_UTF8,    0, upperFunc },
    { "lcase",              1, 0, SQLITE_UTF8,    0, lowerFunc },
    
    
#ifndef SQLITE_OMIT_DATETIME_FUNCS
	/* date */
    { "curdate",            0, 0, SQLITE_UTF8,    0, cdateFunc },
    { "curtime",            0, 0, SQLITE_UTF8,    0, ctimeFunc },
    { "now",                0, 0, SQLITE_UTF8,    0, ctimestampFunc },
	
    { "dayofmonth",         1, 0, SQLITE_UTF8,    0, dayofmonthFunc },
    { "dayofweek",          1, 0, SQLITE_UTF8,    0, dayofweekFunc },
    { "dayofyear",          1, 0, SQLITE_UTF8,    0, dayofyearFunc },
    { "hour",               1, 0, SQLITE_UTF8,    0, hourFunc },
    { "minute",             1, 0, SQLITE_UTF8,    0, minuteFunc },
    { "month",              1, 0, SQLITE_UTF8,    0, monthFunc },
    { "quarter",            1, 0, SQLITE_UTF8,    0, quarterFunc },
    { "second",             1, 0, SQLITE_UTF8,    0, secondFunc },
    { "week",               1, 0, SQLITE_UTF8,    0, weekFunc },
    { "year",               1, 0, SQLITE_UTF8,    0, yearFunc },
	
#endif
    
	{ "user",               0, 0, SQLITE_UTF8,    0, userFunc },
	{ "database",           0, 0, SQLITE_UTF8,    0, databaseFunc },

  };
  /* Aggregate functions */
  static const struct FuncDefAgg {
    char *zName;
    signed char nArg;
    u8 argType;
    u8 needCollSeq;
    void (*xStep)(sqlite3_context*,int,sqlite3_value**);
    void (*xFinalize)(sqlite3_context*);
  } aAggs[] = {
    //{ "stdev",            1, 0, 0, varianceStep, stdevFinalize  },
    //{ "variance",         1, 0, 0, varianceStep, varianceFinalize  },
    //{ "mode",             1, 0, 0, modeStep,     modeFinalize  },
    //{ "median",           1, 0, 0, modeStep,     medianFinalize  },
    //{ "lower_quartile",   1, 0, 0, modeStep,     lower_quartileFinalize  },
    //{ "upper_quartile",   1, 0, 0, modeStep,     upper_quartileFinalize  },
  };
  int i;

  for(i=0; i<sizeof(aFuncs)/sizeof(aFuncs[0]); i++){
    void *pArg = 0;
    switch( aFuncs[i].argType ){
      case 1: pArg = db; break;
      case 2: pArg = (void *)(-1); break;
    }
    //sqlite3CreateFunc
    /* LMH no error checking */
    sqlite3_create_function(db, aFuncs[i].zName, aFuncs[i].nArg,
        aFuncs[i].eTextRep, pArg, aFuncs[i].xFunc, 0, 0);
#if 0
    if( aFuncs[i].needCollSeq ){
      struct FuncDef *pFunc = sqlite3FindFunction(db, aFuncs[i].zName, 
          strlen(aFuncs[i].zName), aFuncs[i].nArg, aFuncs[i].eTextRep, 0);
      if( pFunc && aFuncs[i].needCollSeq ){
        pFunc->needCollSeq = 1;
      }
    }
#endif
  }

  for(i=0; i<sizeof(aAggs)/sizeof(aAggs[0]); i++){
    void *pArg = 0;
    switch( aAggs[i].argType ){
      case 1: pArg = db; break;
      case 2: pArg = (void *)(-1); break;
    }
    //sqlite3CreateFunc
    /* LMH no error checking */
    sqlite3_create_function(db, aAggs[i].zName, aAggs[i].nArg, SQLITE_UTF8, 
        pArg, 0, aAggs[i].xStep, aAggs[i].xFinalize);
#if 0
    if( aAggs[i].needCollSeq ){
      struct FuncDefAgg *pFunc = sqlite3FindFunction( db, aAggs[i].zName,
          strlen(aAggs[i].zName), aAggs[i].nArg, SQLITE_UTF8, 0);
      if( pFunc && aAggs[i].needCollSeq ){
        pFunc->needCollSeq = 1;
      }
    }
#endif
  }
  return 0;
}

#ifdef COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE
int sqlite3_extension_init(
    sqlite3 *db, char **pzErrMsg, const sqlite3_api_routines *pApi){
  SQLITE_EXTENSION_INIT2(pApi);
  RegisterExtensionFunctions(db);
  return 0;
}
#endif /* COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE */

map map_make(cmp_func cmp){
  map r;
  r.cmp=cmp;
  r.base = 0;

  return r;
}

void* xcalloc(size_t nmemb, size_t size, char* s){
  void* ret = calloc(nmemb, size);
  return ret;
}

void xfree(void* p){
  free(p);
}

void node_insert(node** n, cmp_func cmp, void *e){
  int c;
  node* nn;
  if(*n==0){
    nn = (node*)xcalloc(1,sizeof(node), "for node");
    nn->data = e;
    nn->count = 1;
    *n=nn;
  }else{
    c=cmp((*n)->data,e);
    if(0==c){
      ++((*n)->count);
      xfree(e);
    }else if(c>0){
      /* put it right here */
      node_insert(&((*n)->l), cmp, e);
    }else{
      node_insert(&((*n)->r), cmp, e);
    }
  }
}

void map_insert(map *m, void *e){
  node_insert(&(m->base), m->cmp, e);
}

void node_iterate(node *n, map_iterator iter, void* p){
  if(n){
    if(n->l)
      node_iterate(n->l, iter, p);
    iter(n->data, n->count, p);
    if(n->r)
      node_iterate(n->r, iter, p);
  }
}

void map_iterate(map *m, map_iterator iter, void* p){
  node_iterate(m->base, iter, p);
}

void node_destroy(node *n){
  if(0!=n){
    xfree(n->data);
    if(n->l)
      node_destroy(n->l);
    if(n->r)
      node_destroy(n->r);

    xfree(n);
  }
}

void map_destroy(map *m){
  node_destroy(m->base);
}

int int_cmp(const void *a, const void *b){
  int64_t aa = *(int64_t *)(a);
  int64_t bb = *(int64_t *)(b);
  /* printf("cmp %d <=> %d\n",aa,bb); */
  if(aa==bb)
    return 0;
  else if(aa<bb)
    return -1;
  else
    return 1;
}

int double_cmp(const void *a, const void *b){
  double aa = *(double *)(a);
  double bb = *(double *)(b);
  /* printf("cmp %d <=> %d\n",aa,bb); */
  if(aa==bb)
    return 0;
  else if(aa<bb)
    return -1;
  else
    return 1;
}

