/* ------------------------------------------------------------------ */
/* Decimal Number arithmetic module header                            */
/* ------------------------------------------------------------------ */
/* Copyright (c) IBM Corporation, 2000, 2006.  All rights reserved.   */
/*                                                                    */
/* This software is made available under the terms of the             */
/* ICU License -- ICU 1.8.1 and later.                                */
/*                                                                    */
/* The description and User's Guide ("The decNumber C Library") for   */
/* this software is called decNumber.pdf.  This document is           */
/* available, together with arithmetic and format specifications,     */
/* testcases, and Web links, at: http://www2.hursley.ibm.com/decimal  */
/*                                                                    */
/* Please send comments, suggestions, and corrections to the author:  */
/*   mfc@uk.ibm.com                                                   */
/*   Mike Cowlishaw, IBM Fellow                                       */
/*   IBM UK, PO Box 31, Birmingham Road, Warwick CV34 5JL, UK         */
/* ------------------------------------------------------------------ */

#if !defined(DECNUMBER)
  #define DECNUMBER
  #define DECNAME     "decNumber"                       /* Short name */
  #define DECVERSION  "decNumber 3.36"           /* Version [16 max.] */
  #define DECFULLNAME "Decimal Number Module"         /* Verbose name */
  #define DECAUTHOR   "Mike Cowlishaw"                /* Who to blame */

  #if !defined(DECCONTEXT)
    #include "decContext.h"
  #endif

  // Bit settings for decNumber.bits
  #define DECNEG    0x80        // Sign; 1=negative, 0=positive or zero
  #define DECINF    0x40        // 1=Infinity
  #define DECNAN    0x20        // 1=NaN
  #define DECSNAN   0x10        // 1=sNaN
  // The remaining bits are reserved; they must be 0
  #define DECSPECIAL (DECINF|DECNAN|DECSNAN) // any special value

  // Define the decNumber data structure.  The size and shape of the
  // units array in the structure is determined by the following
  // constant.  This must not be changed without recompiling the
  // decNumber library modules.
  #define DECDPUN 3                // DECimal Digits Per UNit [must be in
                                   // range 1-9; 3 or powers of 2 are best].

  // DECNUMDIGITS is the default number of digits that can be held in
  // the structure.  If undefined, 1 is assumed and it is assumed that
  // the structure will be immediately followed by extra space, as
  // required.  DECNUMDIGITS is always >0.
  #if !defined(DECNUMDIGITS)
    #define DECNUMDIGITS 1
  #endif

  // The size (integer data type) of each unit is determined by the
  // number of digits it will hold.
  #if   DECDPUN<=2
    #define decNumberUnit uint8_t
  #elif DECDPUN<=4
    #define decNumberUnit uint16_t
  #else
    #define decNumberUnit uint32_t
  #endif
  // The number of decNumberUnits needed is ceil(DECNUMDIGITS/DECDPUN)
  #define DECNUMUNITS ((DECNUMDIGITS+DECDPUN-1)/DECDPUN)

  // The data structure...
  typedef struct {
    int32_t digits;                // Count of digits in the coefficient; >0
    int32_t exponent;              // Unadjusted exponent, unbiased, in
                                   // range: -1999999997 through 999999999
    uint8_t bits;                  // Indicator bits (see above)
    decNumberUnit lsu[DECNUMUNITS];// Coefficient, from least significant unit
    } decNumber;

  // Notes:
  // 1. If digits is > DECDPUN then there will be more than one
  //    decNumberUnits immediately following the first element of lsu.
  //    These contain the remaining (more significant) digits of the
  //    number, and may be in the lsu array, or may be guaranteed by
  //    some other mechanism (such as being contained in another
  //    structure, or being overlaid on dynamically allocated storage).
  //
  //    Each integer of the coefficient (except potentially the last)
  //    contains DECDPUN digits (e.g., a value in the range 0 through
  //    99999999 if DECDPUN is 8, or 0 through 9999 if DECDPUN is 4).
  //
  // 2. A decNumber converted to a string may need up to digits+14
  //    characters.  The worst cases (non-exponential and exponential
  //    formats) are: -0.00000{9...}#
  //             and: -9.{9...}E+999999999#   (where # is '\0')


  /* ------------------------------------------------------------------ */
  /* decNumber public functions and macros                              */
  /* ------------------------------------------------------------------ */
  // Conversions
  decNumber * decNumberFromString(decNumber *, const char *, decContext *);
  char * decNumberToString(const decNumber *, char *);
  char * decNumberToEngString(const decNumber *, char *);

  // Operators and elementary functions
  decNumber * decNumberAbs(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberAdd(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberCompare(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberCompareTotal(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberDivide(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberDivideInteger(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberExp(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberLn(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberLog10(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMax(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMin(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMinus(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMultiply(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberNormalize(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberPlus(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberPower(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberQuantize(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberRemainder(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberRemainderNear(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberRescale(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberSameQuantum(decNumber *, const decNumber *, const decNumber *);
  decNumber * decNumberSquareRoot(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberSubtract(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberToIntegralValue(decNumber *, const decNumber *, decContext *);

  // Utilities
  decNumber  * decNumberCopy(decNumber *, const decNumber *);
  decNumber  * decNumberTrim(decNumber *);
  const char * decNumberVersion(void);
  decNumber  * decNumberZero(decNumber *);

  // Macros
  #define decNumberIsZero(dn)     (*(dn)->lsu==0 \
                                   && (dn)->digits==1 \
                                   && (((dn)->bits&DECSPECIAL)==0))
  #define decNumberIsNegative(dn) (((dn)->bits&DECNEG)!=0)
  #define decNumberIsNaN(dn)      (((dn)->bits&(DECNAN|DECSNAN))!=0)
  #define decNumberIsQNaN(dn)     (((dn)->bits&(DECNAN))!=0)
  #define decNumberIsSNaN(dn)     (((dn)->bits&(DECSNAN))!=0)
  #define decNumberIsInfinite(dn) (((dn)->bits&DECINF)!=0)

#endif
