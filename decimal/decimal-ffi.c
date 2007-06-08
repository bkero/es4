#include <stdlib.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <decimal128.h>
#include "platform.h"

#define RESULT_SUCCESS 0
#define RESULT_PRECISION_OUT_OF_RANGE 1
#define RESULT_ROUNDING_MODE_OUT_OF_RANGE 2
#define RESULT_OPCODE_OUT_OF_RANGE 3
#define RESULT_INVALID_SYNTAX 4
#define RESULT_OUT_OF_MEMORY 5
#define RESULT_WEDGED 6

enum rounding modes[DEC_ROUND_MAX] = { DEC_ROUND_CEILING,
                                       DEC_ROUND_UP,
                                       DEC_ROUND_HALF_UP,
                                       DEC_ROUND_HALF_EVEN,
                                       DEC_ROUND_HALF_DOWN,
                                       DEC_ROUND_DOWN,
                                       DEC_ROUND_FLOOR };

int isError(decContext *c)
{
    uint32_t e = 
        DEC_Conversion_syntax |
        DEC_Insufficient_storage |
        DEC_Invalid_context;

    return (c->status & e);
}

int decErrorCode(decContext *c)
{
    if (c->status & DEC_Conversion_syntax)
        {
            return RESULT_INVALID_SYNTAX;
        }
    else if (c->status & DEC_Insufficient_storage)
        {
            return RESULT_OUT_OF_MEMORY;
        }
    else if (c->status & DEC_Invalid_context)
        {
            return RESULT_WEDGED;
        }
    return RESULT_SUCCESS;
}

Int32 decop (Int32 precision, Int32 rm, Int32 opcode, Pointer pleft, Pointer pright, Pointer pout)
{
    int i;
    char *sleft;
    char *sright;
    char *sout;
    decContext context;
    decimal128 tmp;
    decNumber left, right, result;

    sleft = (char *)pleft;
    sright = (char *)pright;
    sout = (char *)pout;

    if (precision > DECIMAL128_Pmax)
        {
            return RESULT_PRECISION_OUT_OF_RANGE;
        }

    decContextDefault(&context, DEC_INIT_DECIMAL128);
    context.digits = precision;
    context.traps = 0;

    if (rm >= DEC_ROUND_MAX)
        {
            return RESULT_ROUNDING_MODE_OUT_OF_RANGE;
        }

    context.round = modes[rm];

    decimal128FromString(&tmp, sleft, &context);
    decimal128ToNumber(&tmp, &left);
    if (isError(&context))
        return decErrorCode(&context);

    if (sright)
        {
            decimal128FromString(&tmp, sright, &context);
            decimal128ToNumber(&tmp, &right);
        }
    else
        decNumberZero(&right);
    if (isError(&context))
        return decErrorCode(&context);

    switch (opcode) {
    case 0:
        decNumberAbs(&result, &left, &context);
        break;
    case 1:
        decNumberAdd(&result, &left, &right, &context);
        break;
    case 2:
        decNumberCompare(&result, &left, &right, &context);
        break;
    case 3:
        decNumberCompareTotal(&result, &left, &right, &context);
        break;
    case 4:
        decNumberDivide(&result, &left, &right, &context);
        break;
    case 5:
        decNumberDivideInteger(&result, &left, &right, &context);
        break;
    case 6:
        decNumberExp(&result, &left, &context);
        break;
    case 7:
        decNumberLn(&result, &left, &context);
        break;
    case 8:
        decNumberLog10(&result, &left, &context);
        break;
    case 9:
        decNumberMax(&result, &left, &right, &context);
        break;
    case 10:
        decNumberMin(&result, &left, &right, &context);
        break;
    case 11:
        decNumberMinus(&result, &left, &context);
        break;
    case 12:
        decNumberMultiply(&result, &left, &right, &context);
        break;
    case 13:
        decNumberNormalize(&result, &left, &context);
        break;
    case 14:
        decNumberPlus(&result, &left, &context);
        break;
    case 15:
        decNumberPower(&result, &left, &right, &context);
        break;
    case 16:
        decNumberQuantize(&result, &left, &right, &context);
        break;
    case 17:
        decNumberRemainder(&result, &left, &right, &context);
        break;
    case 18:
        decNumberRemainderNear(&result, &left, &right, &context);
        break;
    case 19:
        decNumberRescale(&result, &left, &right, &context);
        break;
    case 20:
        decNumberSameQuantum(&result, &left, &right);
        break;
    case 21:
        decNumberSquareRoot(&result, &left, &context);
        break;
    case 22:
        decNumberSubtract(&result, &left, &right, &context);
        break;
    case 23:
        decNumberToIntegralValue(&result, &left, &context);
        break;
    default:
        return RESULT_OPCODE_OUT_OF_RANGE;
    }
    if (isError(&context))
        return decErrorCode(&context);

    decimal128FromNumber(&tmp, &result, &context);
    if (isError(&context))
        return decErrorCode(&context);
    decimal128ToString(&tmp, sout);

    return RESULT_SUCCESS;
}
