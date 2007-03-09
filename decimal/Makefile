# This directory contains enough material to implement decimal arithmetic
# as a "plugin" type for an existing programming language, such as ES4.

DECNUMBER=\
	decNumber/decNumber.c \
	decNumber/decContext.c \
	decNumber/decimal128.c \
	decNumber/decimal64.c \
	decNumber/decimal32.c 

decimal: decimal.c
	cc -o decimal -Wall -I decNumber decimal.c $(DECNUMBER)
