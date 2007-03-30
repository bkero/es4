/* ------------------------------------------------------------------ */
/* Decimal 128-bit format module                                      */
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
/* This module comprises the routines for decimal128 format numbers.  */
/* Conversions are supplied to and from decNumber and String.         */
/*                                                                    */
/* No arithmetic routines are included; decNumber provides these.     */
/*                                                                    */
/* Error handling is the same as decNumber (qv.).                     */
/* ------------------------------------------------------------------ */
#include <string.h>           // [for memset/memcpy]
#include <stdio.h>            // [for printf]

#define  DECNUMDIGITS 34      // make decNumbers with space for 34
#include "decNumber.h"        // base number library
#include "decNumberLocal.h"   // decNumber local types, etc.
#include "decimal128.h"       // our primary include

/* Utility routines and tables [in decimal64.c] */
extern const uInt COMBEXP[32], COMBMSD[32];
extern void decDigitsFromDPD(decNumber *, const uInt *, Int);
extern void decDigitsToDPD(const decNumber *, uInt *, Int);

#if DECTRACE || DECCHECK
void decimal128Show(const decimal128 *);          // for debug
extern void decNumberShow(const decNumber *);     // ..
#endif

/* compile-time endian tester [assumes sizeof(int)>1] */
static  const  Int mfcone=1;                 // constant 1
static  const  Flag *mfctop=(Flag *)&mfcone; // -> top byte
#define LITEND mfctop[0]           // named flag; 1=little-endian

/* Useful macro */
// Clear a structure (e.g., a decNumber)
#define DEC_clear(d) memset(d, 0, sizeof(*d))

/* ------------------------------------------------------------------ */
/* decimal128FromNumber -- convert decNumber to decimal128            */
/*                                                                    */
/*   ds is the target decimal128                                      */
/*   dn is the source number (assumed valid)                          */
/*   set is the context, used only for reporting errors               */
/*                                                                    */
/* The set argument is used only for status reporting and for the     */
/* rounding mode (used if the coefficient is more than DECIMAL128_Pmax*/
/* digits or an overflow is detected).  If the exponent is out of the */
/* valid range then Overflow or Underflow will be raised.             */
/* After Underflow a subnormal result is possible.                    */
/*                                                                    */
/* DEC_Clamped is set if the number has to be 'folded down' to fit,   */
/* by reducing its exponent and multiplying the coefficient by a      */
/* power of ten, or if the exponent on a zero had to be clamped.      */
/* ------------------------------------------------------------------ */
decimal128 * decimal128FromNumber(decimal128 *d128, const decNumber *dn,
                                  decContext *set) {
  uInt status=0;                   // status accumulator
  decNumber  dw;                   // work
  decContext dc;                   // ..
  uInt *pu;                        // ..
  uInt comb, exp;                  // ..
  uInt targar[4]={0,0,0,0};        // target 128-bit
  #define targup targar[3]         // name the word with the sign

  // If the number has too many digits, or the exponent could be
  // out of range then reduce the number under the appropriate
  // constraints.  This could push the number to Infinity or zero,
  // so this check and rounding must be done before generating the
  // decimal128]
  if (!(dn->bits&DECSPECIAL)) {              // not a special value
    Int ae=dn->exponent+dn->digits-1;        // adjusted exponent
    if (dn->digits>DECIMAL128_Pmax           // too many digits
     || ae>DECIMAL128_Emax                   // likely overflow
     || ae<DECIMAL128_Emin) {                // likely underflow
      decContextDefault(&dc, DEC_INIT_DECIMAL128); // [no traps]
      dc.round=set->round;                   // use supplied rounding
      decNumberPlus(&dw, dn, &dc);           // (round and check)
      // [this changes -0 to 0, so enforce the sign...]
      dw.bits|=dn->bits&DECNEG;
      status=dc.status;                      // save status
      dn=&dw;                                // use the work number
      }
    } // maybe out of range

  if (dn->bits&DECSPECIAL) {                      // a special value
    if (dn->bits&DECINF) targup=DECIMAL_Inf<<24;
     else {                                       // sNaN or qNaN
      if ((*dn->lsu!=0 || dn->digits>1)           // non-zero coefficient
       && (dn->digits<DECIMAL128_Pmax)) {         // coefficient fits
        decDigitsToDPD(dn, targar, 0);
        }
      if (dn->bits&DECNAN) targup|=DECIMAL_NaN<<24;
       else targup|=DECIMAL_sNaN<<24;
      } // a NaN
    } // special

   else { // is finite
    if (decNumberIsZero(dn)) {               // is a zero
      // set and clamp exponent
      if (dn->exponent<-DECIMAL128_Bias) {
        exp=0;                               // low clamp
        status|=DEC_Clamped;
        }
       else {
        exp=dn->exponent+DECIMAL128_Bias;    // bias exponent
        if (exp>DECIMAL128_Ehigh) {          // top clamp
          exp=DECIMAL128_Ehigh;
          status|=DEC_Clamped;
          }
        }
      comb=(exp>>9) & 0x18;             // msd=0, exp top 2 bits ..
      }
     else {                             // non-zero finite number
      uInt msd;                         // work
      Int pad=0;                        // coefficient pad digits

      // the dn is known to fit, but it may need to be padded
      exp=(uInt)(dn->exponent+DECIMAL128_Bias);    // bias exponent
      if (exp>DECIMAL128_Ehigh) {                  // fold-down case
        pad=exp-DECIMAL128_Ehigh;
        exp=DECIMAL128_Ehigh;                      // [to maximum]
        status|=DEC_Clamped;
        }

      decDigitsToDPD(dn, targar, pad);

      // save and clear the top digit
      msd=targup>>14;
      targup&=0x00003fff;

      // create the combination field
      if (msd>=8) comb=0x18 | ((exp>>11) & 0x06) | (msd & 0x01);
             else comb=((exp>>9) & 0x18) | msd;
      }
    targup|=comb<<26;              // add combination field ..
    targup|=(exp&0xfff)<<14;       // .. and exponent continuation
    } // finite

  if (dn->bits&DECNEG) targup|=0x80000000; // add sign bit

  // now write to storage; this may be endian, or not
  #if DECENDIAN
  // DECENDIAN -- direct store, in the right order
  pu=(uInt *)d128->bytes;          // overlay
  if (LITEND) {
    *pu=targar[0];                 // directly store the low int
    pu++;
    *pu=targar[1];                 // then the mid-low
    pu++;
    *pu=targar[2];                 // then the mid-high
    pu++;
    *pu=targar[3];                 // then the high int
    }
   else {
    *pu=targar[3];                 // directly store the high int
    pu++;
    *pu=targar[2];                 // then the mid-high
    pu++;
    *pu=targar[1];                 // then the mid-low
    pu++;
    *pu=targar[0];                 // then the low int
    }
  #else
  // not DECENDIAN -- use network byte order
  if (LITEND) {                    // little-endian needs reversal
    uByte *pb;                     // work
    Int off;                       // ..
    for (pb=&d128->bytes[15]; pb>=d128->bytes; pb--) {
      off=3-((pb-d128->bytes)>>2); // 0, then 1, 2, 3
      *pb=(uByte)(targar[off]&0xff);
      targar[off]>>=8;
      } // i
    }
   else { // big-endian; it's the right way round already
    pu=(uInt *)d128->bytes;        // overlay
    *pu=targar[3];                 // directly store the high int
    pu++;
    *pu=targar[2];                 // then the mid-high
    pu++;
    *pu=targar[1];                 // then the mid-low
    pu++;
    *pu=targar[0];                 // then the low int
    }
  #endif

  if (status!=0) decContextSetStatus(set, status); // pass on status
  // decimal128Show(d128);
  return d128;
  } // decimal128FromNumber

/* ------------------------------------------------------------------ */
/* decimal128ToNumber -- convert decimal128 to decNumber              */
/*   d128 is the source decimal128                                    */
/*   dn is the target number, with appropriate space                  */
/* No error is possible.                                              */
/* ------------------------------------------------------------------ */
decNumber * decimal128ToNumber(const decimal128 *d128, decNumber *dn) {
  uInt msd;                        // coefficient MSD
  uInt exp;                        // exponent top two bits
  uInt comb;                       // combination field
  uInt *pu;                        // work
  Int  need;                       // ..
  uInt sourar[4];                  // source 128-bit
  #define sourhi sourar[3]         // name the word with the sign
  #define sourmh sourar[2]         // and the mid-high word
  #define sourml sourar[1]         // and the mod-low word
  #define sourlo sourar[0]         // and the lowest word

  // load source from storage; this may be endian, or not
  #if DECENDIAN
  // DECENDIAN -- direct load, in the right order
  pu=(uInt *)d128->bytes;          // overlay
  if (LITEND) {
    sourar[0]=*pu;                 // directly load the low int
    pu++;
    sourar[1]=*pu;                 // then the mid-low
    pu++;
    sourar[2]=*pu;                 // then the mid-high
    pu++;
    sourar[3]=*pu;                 // then the high int
    }
   else {
    sourar[3]=*pu;                 // directly load the high int
    pu++;
    sourar[2]=*pu;                 // then the mid-high
    pu++;
    sourar[1]=*pu;                 // then the mid-low
    pu++;
    sourar[0]=*pu;                 // then the low int
    }
  #else
  // not DECENDIAN -- use network byte order
  if (LITEND) {                    // little-endian needs reversal
    const uByte *pb;               // work
    Int off;                       // ..
    for (pb=d128->bytes; pb<=&d128->bytes[15]; pb++) {
      off=3-((pb-d128->bytes)>>2); // 3, then 2, 1, 0
      sourar[off]<<=8;
      sourar[off]|=*pb;
      } // i
    }
   else { // big-endian; it's the right way round already
    pu=(uInt *)d128->bytes;        // overlay
    sourar[3]=*pu;                 // directly load the high int
    pu++;
    sourar[2]=*pu;                 // then the mid-high
    pu++;
    sourar[1]=*pu;                 // then the mid-low
    pu++;
    sourar[0]=*pu;                 // then the low int
    }
  #endif

  comb=(sourhi>>26)&0x1f;          // combination field

  decNumberZero(dn);               // clean number
  if (sourhi&0x80000000) dn->bits=DECNEG; // set sign if negative

  msd=COMBMSD[comb];               // decode the combination field
  exp=COMBEXP[comb];               // ..

  if (exp==3) {                    // is a special
    if (msd==0) {
      dn->bits|=DECINF;
      return dn;                   // no coefficient needed
      }
    else if (sourhi&0x02000000) dn->bits|=DECSNAN;
    else dn->bits|=DECNAN;
    msd=0;                         // no top digit
    }
   else {                          // is a finite number
    dn->exponent=(exp<<12)+((sourhi>>14)&0xfff)-DECIMAL128_Bias; // unbiased
    }

  // get the coefficient
  sourhi&=0x00003fff;              // clean coefficient continuation
  if (msd) {                       // non-zero msd
    sourhi|=msd<<14;               // prefix to coefficient
    need=12;                       // process 12 declets
    }
   else { // msd=0
    if (sourhi) need=11;           // declets to process
     else if (sourmh) need=10;
     else if (sourml) need=7;
     else if (sourlo) need=4;
     else return dn;               // easy: coefficient is 0
    } //msd=0

  decDigitsFromDPD(dn, sourar, need);   // process declets
  // decNumberShow(dn);
  return dn;
  } // decimal128ToNumber

/* ------------------------------------------------------------------ */
/* to-scientific-string -- conversion to numeric string               */
/* to-engineering-string -- conversion to numeric string              */
/*                                                                    */
/*   decimal128ToString(d128, string);                                */
/*   decimal128ToEngString(d128, string);                             */
/*                                                                    */
/*  d128 is the decimal128 format number to convert                   */
/*  string is the string where the result will be laid out            */
/*                                                                    */
/*  string must be at least 24 characters                             */
/*                                                                    */
/*  No error is possible, and no status can be set.                   */
/* ------------------------------------------------------------------ */
char * decimal128ToString(const decimal128 *d128, char *string){
  decNumber dn;                         // work
  decimal128ToNumber(d128, &dn);
  decNumberToString(&dn, string);
  return string;
  } // decimal128ToString

char * decimal128ToEngString(const decimal128 *d128, char *string){
  decNumber dn;                         // work
  decimal128ToNumber(d128, &dn);
  decNumberToEngString(&dn, string);
  return string;
  } // decimal128ToEngString

/* ------------------------------------------------------------------ */
/* to-number -- conversion from numeric string                        */
/*                                                                    */
/*   decimal128FromString(result, string, set);                       */
/*                                                                    */
/*  result  is the decimal128 format number which gets the result of  */
/*          the conversion                                            */
/*  *string is the character string which should contain a valid      */
/*          number (which may be a special value)                     */
/*  set     is the context                                            */
/*                                                                    */
/* The context is supplied to this routine is used for error handling */
/* (setting of status and traps) and for the rounding mode, only.     */
/* If an error occurs, the result will be a valid decimal128 NaN.     */
/* ------------------------------------------------------------------ */
decimal128 * decimal128FromString(decimal128 *result, const char *string,
                                  decContext *set) {
  decContext dc;                             // work
  decNumber dn;                              // ..

  decContextDefault(&dc, DEC_INIT_DECIMAL128); // no traps, please
  dc.round=set->round;                         // use supplied rounding

  decNumberFromString(&dn, string, &dc);     // will round if needed
  decimal128FromNumber(result, &dn, &dc);
  if (dc.status!=0) {                        // something happened
    decContextSetStatus(set, dc.status);     // .. pass it on
    }
  return result;
  } // decimal128FromString

#if DECTRACE || DECCHECK
/* ------------------------------------------------------------------ */
/* decimal128Show -- display a decimal128 in hexadecimal [debug aid]  */
/*   d128 -- the number to show                                       */
/* ------------------------------------------------------------------ */
// Also shows sign/cob/expconfields extracted
void decimal128Show(const decimal128 *d128) {
  char buf[DECIMAL128_Bytes*2+1];
  Int i, j=0;

  #if DECENDIAN
  if (LITEND) {
    for (i=0; i<DECIMAL128_Bytes; i++, j+=2) {
      sprintf(&buf[j], "%02x", d128->bytes[15-i]);
      }
    printf(" D128> %s [S:%d Cb:%02x Ec:%02x] LittleEndian\n", buf,
           d128->bytes[15]>>7, (d128->bytes[15]>>2)&0x1f,
           ((d128->bytes[15]&0x3)<<10)|(d128->bytes[14]<<2)|
           (d128->bytes[13]>>6));
    }
   else {
  #endif
    for (i=0; i<DECIMAL128_Bytes; i++, j+=2) {
      sprintf(&buf[j], "%02x", d128->bytes[i]);
      }
    printf(" D128> %s [S:%d Cb:%02x Ec:%02x] BigEndian\n", buf,
           decimal128Sign(d128), decimal128Comb(d128),
           decimal128ExpCon(d128));
  #if DECENDIAN
    }
  #endif
  } // decimal128Show
#endif
