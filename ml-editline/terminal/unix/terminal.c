/*
 * Copyright (c) 2007 David Herman
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curses.h>

#define printable(ch) ((' ' <= (ch)) && ((ch) <= '~'))

void BuildEscapeSequence(char *in, int arg, char *out)
{
    strncpy(out, tparm(in, arg), 1024);
}

void PrintEscapeSequence(char *str)
{
    /*
    int i;
    fprintf(stderr, "tputs: \"");
    for (i = 0; i < strlen(str); i++)
    {
        fprintf(stderr, printable(str[i]) ? "%c" : "\\%d", str[i]);
    }
    fprintf(stderr, "\"\n");
    fflush(stderr);
    */
    tputs(str, 1, putchar);
    fflush(stdout);
}

void LookupEscapeSequence(char *cap, char *out)
{
    char *result = tigetstr(cap);

    if (result != NULL)
        strncpy(out, tigetstr(cap), 1024);
}
