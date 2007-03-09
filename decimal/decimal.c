#include <stdlib.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <decimal128.h>

void
exitIfError(decContext *c)
{
  if (c->status & DEC_Errors)
    {
      c->status &= DEC_Errors;
      printf("ERROR: %s\n", decContextStatusToString(c));
      exit(1);
    }
}

int 
main(int argc, char **argv)
{
  /* 
   * This is a stub program for interacting with the "decNumber"
   * package, specifically its decimal128 form.
   *
   * It performs a single decimal128 operation, prints the result, and
   * exits.
   *
   * In addition to argv[0], it requires the following arguments:
   *
   * argv[1]: precision, a decimal integer between 1 and 34 inclusive
   * argv[2]: rounding mode, one of the rounding-mode names (see below)
   * argv[3]: operation: one of the operator names (see below)
   * argv[4]: the left operand
   * argv[5]: the right operand (can be omitted for unary operations)
   *
   * Graydon Hoare <graydon@mozilla.com>
   * March 8 2007
   */

  int32_t precision;
  char buf[DECIMAL128_String];

  decContext context;
  decimal128 tmp;
  decNumber left, right, result;

  if (argc != 5 &&
      argc != 6) 
    {
      printf("usage: %s <precision> <rounding-mode> <operation> <operand> [<operand>] \n", 
	     argv[0]);
      exit(1);
    }

  if (sscanf(argv[1], "%" SCNi32, &precision) != 1)
    {
      printf("error reading precision\n");
      exit(1);
    }

  if (precision > DECIMAL128_Pmax)
    {
      printf("precision must be a number between 1 and %d inclusive\n", 
	     DECIMAL128_Pmax);
      exit(1);
    }

  decContextDefault(&context, DEC_INIT_DECIMAL128);
  context.digits = precision;
  context.traps = 0;

  if (strcmp(argv[2], "ROUND_CEILING") == 0)
    context.round = DEC_ROUND_CEILING;
  else if (strcmp(argv[2], "ROUND_UP") == 0)
    context.round = DEC_ROUND_UP;
  else if (strcmp(argv[2], "ROUND_HALF_UP") == 0)
    context.round = DEC_ROUND_HALF_UP;
  else if (strcmp(argv[2], "ROUND_HALF_EVEN") == 0)
    context.round = DEC_ROUND_HALF_EVEN;
  else if (strcmp(argv[2], "ROUND_HALF_DOWN") == 0)
    context.round = DEC_ROUND_HALF_DOWN;
  else if (strcmp(argv[2], "ROUND_DOWN") == 0)
    context.round = DEC_ROUND_DOWN;
  else if (strcmp(argv[2], "ROUND_FLOOR") == 0)
    context.round = DEC_ROUND_FLOOR;
  else 
    {
      printf("unrecognized rounding mode\n");
      exit(1);
    }

  decimal128FromString(&tmp, argv[4], &context);
  decimal128ToNumber(&tmp, &left);
  exitIfError(&context);

  if (argc == 6)
    {
      decimal128FromString(&tmp, argv[5], &context);
      decimal128ToNumber(&tmp, &right);
    }
  else
    decNumberZero(&right);
  
  exitIfError(&context);

  if (strcmp(argv[3], "abs") == 0)
    decNumberAbs(&result, &left, &context);
  else if (strcmp(argv[3], "add") == 0)
    decNumberAdd(&result, &left, &right, &context);
  else if (strcmp(argv[3], "compare") == 0)
    decNumberCompare(&result, &left, &right, &context);
  else if (strcmp(argv[3], "compareTotal") == 0)
    decNumberCompareTotal(&result, &left, &right, &context);
  else if (strcmp(argv[3], "divide") == 0)
    decNumberDivide(&result, &left, &right, &context);
  else if (strcmp(argv[3], "divideInteger") == 0)
    decNumberDivideInteger(&result, &left, &right, &context);
  else if (strcmp(argv[3], "exp") == 0)
    decNumberExp(&result, &left, &context);
  else if (strcmp(argv[3], "ln") == 0)
    decNumberLn(&result, &left, &context);
  else if (strcmp(argv[3], "log10") == 0)
    decNumberLog10(&result, &left, &context);
  else if (strcmp(argv[3], "max") == 0)
    decNumberMax(&result, &left, &right, &context);
  else if (strcmp(argv[3], "min") == 0)
    decNumberMin(&result, &left, &right, &context);
  else if (strcmp(argv[3], "minus") == 0)
    decNumberMinus(&result, &left, &context);
  else if (strcmp(argv[3], "multiply") == 0)
    decNumberMultiply(&result, &left, &right, &context);
  else if (strcmp(argv[3], "normalize") == 0)
    decNumberNormalize(&result, &left, &context);
  else if (strcmp(argv[3], "plus") == 0)
    decNumberPlus(&result, &left, &context);
  else if (strcmp(argv[3], "power") == 0)
    decNumberPower(&result, &left, &right, &context);
  else if (strcmp(argv[3], "quantize") == 0)
    decNumberQuantize(&result, &left, &right, &context);
  else if (strcmp(argv[3], "remainder") == 0)
    decNumberRemainder(&result, &left, &right, &context);
  else if (strcmp(argv[3], "remainderNear") == 0)
    decNumberRemainderNear(&result, &left, &right, &context);
  else if (strcmp(argv[3], "rescale") == 0)
    decNumberRescale(&result, &left, &right, &context);
  else if (strcmp(argv[3], "sameQuantum") == 0)
    decNumberSameQuantum(&result, &left, &right);
  else if (strcmp(argv[3], "squareRoot") == 0)
    decNumberSquareRoot(&result, &left, &context);
  else if (strcmp(argv[3], "subtract") == 0)
    decNumberSubtract(&result, &left, &right, &context);
  else if (strcmp(argv[3], "toIntegralValue") == 0)
    decNumberToIntegralValue(&result, &left, &context);
  else
    {
      printf("unrecognized operator name: %s\n", argv[3]);
      exit(1);
    }

  exitIfError(&context);

  decimal128FromNumber(&tmp, &result, &context);
  exitIfError(&context);
  decimal128ToString(&tmp, buf);
  printf("%s\n", buf);

  return 0;
}
