/* ------------------------------------------------------------------ */
/* Decimal 32-bit format module                                       */
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
/* This module comprises the routines for decimal32 format numbers.   */
/* Conversions are supplied to and from decNumber and String.         */
/*                                                                    */
/* No arithmetic routines are included; decNumber provides these.     */
/*                                                                    */
/* Error handling is the same as decNumber (qv.).                     */
/* ------------------------------------------------------------------ */
#include <string.h>           // [for memset/memcpy]
#include <stdio.h>            // [for printf]

#define  DECNUMDIGITS  7      // make decNumbers with space for 7
#include "decNumber.h"        // base number library
#include "decNumberLocal.h"   // decNumber local types, etc.
#include "decimal32.h"        // our primary include

/* Utility tables and routines [in decimal64.c] */
extern const uInt COMBEXP[32], COMBMSD[32];
extern void decDigitsToDPD(const decNumber *, uInt *, Int);
extern void decDigitsFromDPD(decNumber *, const uInt *, Int);

#if DECTRACE || DECCHECK
void decimal32Show(const decimal32 *);            // for debug
extern void decNumberShow(const decNumber *);     // ..
#endif

/* Useful macro */
// Clear a structure (e.g., a decNumber)
#define DEC_clear(d) memset(d, 0, sizeof(*d))

#if !DECENDIAN || DECTRACE || DECCHECK
/* compile-time endian tester [assumes sizeof(int)>1] */
static  const  Int mfcone=1;                 // constant 1
static  const  Flag *mfctop=(Flag *)&mfcone; // -> top byte
#define LITEND mfctop[0]           // named flag; 1=little-endian
#endif

/* ------------------------------------------------------------------ */
/* decimal32FromNumber -- convert decNumber to decimal32              */
/*                                                                    */
/*   ds is the target decimal32                                       */
/*   dn is the source number (assumed valid)                          */
/*   set is the context, used only for reporting errors               */
/*                                                                    */
/* The set argument is used only for status reporting and for the     */
/* rounding mode (used if the coefficient is more than DECIMAL32_Pmax */
/* digits or an overflow is detected).  If the exponent is out of the */
/* valid range then Overflow or Underflow will be raised.             */
/* After Underflow a subnormal result is possible.                    */
/*                                                                    */
/* DEC_Clamped is set if the number has to be 'folded down' to fit,   */
/* by reducing its exponent and multiplying the coefficient by a      */
/* power of ten, or if the exponent on a zero had to be clamped.      */
/* ------------------------------------------------------------------ */
decimal32 * decimal32FromNumber(decimal32 *d32, const decNumber *dn,
                              decContext *set) {
  uInt status=0;                   // status accumulator
  decNumber  dw;                   // work
  decContext dc;                   // ..
  uInt *pu;                        // ..
  uInt comb, exp;                  // ..
  uInt targ=0;                     // target 32-bit

  // If the number has too many digits, or the exponent could be
  // out of range then reduce the number under the appropriate
  // constraints.  This could push the number to Infinity or zero,
  // so this check and rounding must be done before generating the
  // decimal32]
  if (!(dn->bits&DECSPECIAL)) {              // not a special value
    Int ae=dn->exponent+dn->digits-1;        // adjusted exponent
    if (dn->digits>DECIMAL32_Pmax            // too many digits
     || ae>DECIMAL32_Emax                    // likely overflow
     || ae<DECIMAL32_Emin) {                 // likely underflow
      decContextDefault(&dc, DEC_INIT_DECIMAL32); // [no traps]
      dc.round=set->round;                   // use supplied rounding
      decNumberPlus(&dw, dn, &dc);           // (round and check)
      // [this changes -0 to 0, so enforce the sign...]
      dw.bits|=dn->bits&DECNEG;
      status=dc.status;                      // save status
      dn=&dw;                                // use the work number
      }
    } // maybe out of range

  if (dn->bits&DECSPECIAL) {                      // a special value
    if (dn->bits&DECINF) targ=DECIMAL_Inf<<24;
     else {                                       // sNaN or qNaN
      if ((*dn->lsu!=0 || dn->digits>1)           // non-zero coefficient
       && (dn->digits<DECIMAL32_Pmax)) {          // coefficient fits
        decDigitsToDPD(dn, &targ, 0);
        }
      if (dn->bits&DECNAN) targ|=DECIMAL_NaN<<24;
       else targ|=DECIMAL_sNaN<<24;
      } // a NaN
    } // special

   else { // is finite
    if (decNumberIsZero(dn)) {               // is a zero
      // set and clamp exponent
      if (dn->exponent<-DECIMAL32_Bias) {
        exp=0;                               // low clamp
        status|=DEC_Clamped;
        }
       else {
        exp=dn->exponent+DECIMAL32_Bias;     // bias exponent
        if (exp>DECIMAL32_Ehigh) {           // top clamp
          exp=DECIMAL32_Ehigh;
          status|=DEC_Clamped;
          }
        }
      comb=(exp>>3) & 0x18;             // msd=0, exp top 2 bits ..
      }
     else {                             // non-zero finite number
      uInt msd;                         // work
      Int pad=0;                        // coefficient pad digits

      // the dn is known to fit, but it may need to be padded
      exp=(uInt)(dn->exponent+DECIMAL32_Bias);    // bias exponent
      if (exp>DECIMAL32_Ehigh) {                  // fold-down case
        pad=exp-DECIMAL32_Ehigh;
        exp=DECIMAL32_Ehigh;                      // [to maximum]
        status|=DEC_Clamped;
        }

      decDigitsToDPD(dn, &targ, pad);

      // save and clear the top digit
      msd=targ>>20;
      targ&=0x000fffff;

      // create the combination field
      if (msd>=8) comb=0x18 | ((exp>>5) & 0x06) | (msd & 0x01);
             else comb=((exp>>3) & 0x18) | msd;
      }
    targ|=comb<<26;                // add combination field ..
    targ|=(exp&0x3f)<<20;          // .. and exponent continuation
    } // finite

  if (dn->bits&DECNEG) targ|=0x80000000;  // add sign bit

  // now write to storage; this may be endian, or not
  #if DECENDIAN
  // DECENDIAN -- direct store
  pu=(uInt *)d32->bytes;           // overlay
  *pu=targ;                        // directly store the int
  #else
  // not DECENDIAN -- use network byte order
  if (LITEND) {                    // little-endian needs reversal
    uByte *pb;                     // work
    for (pb=&d32->bytes[3]; pb>=d32->bytes; pb--) {
      *pb=(uByte)(targ&0xff);
      targ>>=8;
      } // i
    }
   else { // big-endian; it's the right way round already
    pu=(uInt *)d32->bytes;         // overlay
    *pu=targ;                      // directly store the int
    }
  #endif

  if (status!=0) decContextSetStatus(set, status); // pass on status
  // decimal32Show(d32);
  return d32;
  } // decimal32FromNumber

/* ------------------------------------------------------------------ */
/* decimal32ToNumber -- convert decimal32 to decNumber                */
/*   d32 is the source decimal32                                      */
/*   dn is the target number, with appropriate space                  */
/* No error is possible.                                              */
/* ------------------------------------------------------------------ */
decNumber * decimal32ToNumber(const decimal32 *d32, decNumber *dn) {
  uInt msd;                        // coefficient MSD
  uInt exp;                        // exponent top two bits
  uInt comb;                       // combination field
  uInt *pu;                        // work
  uInt sour;                       // source 32-bit

  // load source from storage; this may be endian, or not
  #if DECENDIAN
  // DECENDIAN -- direct load
  pu=(uInt *)d32->bytes;           // overlay
  sour=*pu;                        // directly load the int
  #else
  // not DECENDIAN -- use network byte order
  if (LITEND) {                    // little-endian needs reversal
    const uByte *pb;               // work
    sour=0;                        // [keep compiler quiet]
    for (pb=d32->bytes; pb<=&d32->bytes[3]; pb++) {
      sour<<=8;
      sour|=*pb;
      } // i
    }
   else { // big-endian; it's the right way round already
    pu=(uInt *)d32->bytes;         // overlay
    sour=*pu;                      // directly load the int
    }
  #endif

  comb=(sour>>26)&0x1f;            // combination field

  decNumberZero(dn);               // clean number
  if (sour&0x80000000) dn->bits=DECNEG; // set sign if negative

  msd=COMBMSD[comb];               // decode the combination field
  exp=COMBEXP[comb];               // ..

  if (exp==3) {                    // is a special
    if (msd==0) {
      dn->bits|=DECINF;
      return dn;                   // no coefficient needed
      }
    else if (sour&0x02000000) dn->bits|=DECSNAN;
    else dn->bits|=DECNAN;
    msd=0;                         // no top digit
    }
   else {                          // is a finite number
    dn->exponent=(exp<<6)+((sour>>20)&0x3f)-DECIMAL32_Bias; // unbiased
    }

  // get the coefficient
  sour&=0x000fffff;                // clean coefficient continuation
  if (msd) {                       // non-zero msd
    sour|=msd<<20;                 // prefix to coefficient
    decDigitsFromDPD(dn, &sour, 3); // process 3 declets
    return dn;
    }
  // msd=0
  if (!sour) return dn;            // easy: coefficient is 0
  if (sour&0x000ffc00)             // need 2 declets?
    decDigitsFromDPD(dn, &sour, 2); // process 2 declets
   else
    decDigitsFromDPD(dn, &sour, 1); // process 1 declet
  return dn;
  } // decimal32ToNumber

/* ------------------------------------------------------------------ */
/* to-scientific-string -- conversion to numeric string               */
/* to-engineering-string -- conversion to numeric string              */
/*                                                                    */
/*   decimal32ToString(d32, string);                                  */
/*   decimal32ToEngString(d32, string);                               */
/*                                                                    */
/*  d32 is the decimal32 format number to convert                     */
/*  string is the string where the result will be laid out            */
/*                                                                    */
/*  string must be at least 24 characters                             */
/*                                                                    */
/*  No error is possible, and no status can be set.                   */
/* ------------------------------------------------------------------ */
char * decimal32ToString(const decimal32 *d32, char *string){
  decNumber dn;                         // work
  decimal32ToNumber(d32, &dn);
  decNumberToString(&dn, string);
  return string;
  } // decimal32ToString

char * decimal32ToEngString(const decimal32 *d32, char *string){
  decNumber dn;                         // work
  decimal32ToNumber(d32, &dn);
  decNumberToEngString(&dn, string);
  return string;
  } // decimal32ToEngString

/* ------------------------------------------------------------------ */
/* to-number -- conversion from numeric string                        */
/*                                                                    */
/*   decimal32FromString(result, string, set);                        */
/*                                                                    */
/*  result  is the decimal32 format number which gets the result of   */
/*          the conversion                                            */
/*  *string is the character string which should contain a valid      */
/*          number (which may be a special value)                     */
/*  set     is the context                                            */
/*                                                                    */
/* The context is supplied to this routine is used for error handling */
/* (setting of status and traps) and for the rounding mode, only.     */
/* If an error occurs, the result will be a valid decimal32 NaN.      */
/* ------------------------------------------------------------------ */
decimal32 * decimal32FromString(decimal32 *result, const char *string,
                                decContext *set) {
  decContext dc;                             // work
  decNumber dn;                              // ..

  decContextDefault(&dc, DEC_INIT_DECIMAL32); // no traps, please
  dc.round=set->round;                        // use supplied rounding

  decNumberFromString(&dn, string, &dc);     // will round if needed
  decimal32FromNumber(result, &dn, &dc);
  if (dc.status!=0) {                        // something happened
    decContextSetStatus(set, dc.status);     // .. pass it on
    }
  return result;
  } // decimal32FromString

#if DECTRACE || DECCHECK
/* ------------------------------------------------------------------ */
/* decimal32Show -- display a decimal32 in hexadecimal [debug aid]    */
/*   d32 -- the number to show                                        */
/* ------------------------------------------------------------------ */
// Also shows sign/cob/expconfields extracted - valid bigendian only
void decimal32Show(const decimal32 *d32) {
  char buf[DECIMAL32_Bytes*2+1];
  Int i, j=0;

  #if DECENDIAN
  if (LITEND) {
    for (i=0; i<DECIMAL32_Bytes; i++, j+=2) {
      sprintf(&buf[j], "%02x", d32->bytes[3-i]);
      }
    printf(" D32> %s [S:%d Cb:%02x Ec:%02x] LittleEndian\n", buf,
           d32->bytes[3]>>7, (d32->bytes[3]>>2)&0x1f,
           ((d32->bytes[3]&0x3)<<4)| (d32->bytes[2]>>4));
    }
   else {
  #endif
    for (i=0; i<DECIMAL32_Bytes; i++, j+=2) {
      sprintf(&buf[j], "%02x", d32->bytes[i]);
      }
    printf(" D32> %s [S:%d Cb:%02x Ec:%02x] BigEndian\n", buf,
           decimal32Sign(d32), decimal32Comb(d32), decimal32ExpCon(d32));
  #if DECENDIAN
    }
  #endif
  } // decimal32Show
#endif
