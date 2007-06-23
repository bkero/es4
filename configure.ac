AC_INIT([es4],
        [0.0m0],
        [bugs@ecmascript-lang.org],
        [ECMAScript Edition 4 Reference Implementation])

AC_COPYRIGHT([
Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
Software ASA, and others.
])

AC_DEFUN([UNLESS_EXECUTABLE],
         [AC_MSG_CHECKING([whether $1 is executable])
if test -x "$2"; then
    AC_MSG_RESULT([yes])
else
    AC_MSG_RESULT([no])
    $3
fi])

AC_DEFUN([CHECK_EXECUTABLE],
         [UNLESS_EXECUTABLE([$1], [$2], [AC_MSG_ERROR([cannot execute $2])])])

AC_DEFUN([EXEC_SMLNJ],
         [echo -e '$1;' | (sml >/dev/null) 2>&1])

AC_DEFUN([EVAL_SMLNJ_STRING],
         [$1=`echo -e 'TextIO.output (TextIO.stdErr, $2);' | (${SMLNJ} >/dev/null) 2>&1`])

AC_DEFUN([GET_SMLNJ_HEAP_SUFFIX],
         [EVAL_SMLNJ_STRING($1, [SMLofNJ.SysInfo.getHeapSuffix()])])

AC_DEFUN([GET_SMLNJ_CM_ANCHOR_HOME],
         [EVAL_SMLNJ_STRING($1, [valOf(case (CM.Anchor.anchor "$2") of { get, ... } => get ())])])

DIALECT=smlnj
SMLNJ="<<search>>"
MLTON="<<search>>"
DEBUG=no

AC_ARG_WITH([smlnj],
            AC_HELP_STRING([--with-smlnj=FULLPATH],
                           [set full path to SML/NJ executable]),
            [SMLNJ="$withval"])
AC_ARG_WITH([mlton],
            AC_HELP_STRING([--with-mlton=FULLPATH],
                           [set full path to MLton compiler]),
            [MLTON="$withval"])
AC_ARG_WITH([dialect],
            AC_HELP_STRING([--with-dialect=(smlnj|mlton)],
                           [choose dialect of Standard ML (default smlnj)]),
            [DIALECT="$withval"])
AC_ARG_ENABLE([debug],
              AC_HELP_STRING([--enable-debug],
                             [compile SML with debugging information (default no)]),
              [DEBUG="$enableval"])

AC_MSG_NOTICE([using $DIALECT dialect for building releases])

# 1. Find SML/NJ installation information.
if test "$SMLNJ" = "yes" -o "$SMLNJ" = "<<search>>"; then
    AC_PATH_PROGS([SMLNJ], [sml])
fi

if test -n "$SMLNJ" -a -x "$SMLNJ"; then
    SMLNJ_HOME=`dirname $SMLNJ`
    MLBUILD="$SMLNJ_HOME"/ml-build
    HEAP2EXEC="$SMLNJ_HOME"/heap2exec
    AC_MSG_CHECKING([platform-specific heapfile suffix])
    GET_SMLNJ_HEAP_SUFFIX([HEAP_SUFFIX])
    AC_MSG_RESULT([$HEAP_SUFFIX])
else
    SMLNJ=false
    SMLNJ_HOME=XXX
    MLBUILD=false
    HEAP2EXEC=false
    HEAP_SUFFIX=XXX
fi

# 2. SML/NJ dialect

if test "$DIALECT" = "smlnj"; then
    if test "$SMLNJ" = "false"; then
        AC_MSG_ERROR([cannot execute sml])
    fi
    CHECK_EXECUTABLE([ml-build], [$MLBUILD])
    AC_MSG_CHECKING([usability of heap2exec on this platform])
    if test -x "$HEAP2EXEC"; then
        EXEC_SMLNJ([SMLofNJ.exportFn("autoconf-test.heap",(fn _ => OS.Process.success))])
        "$HEAP2EXEC" autoconf-test.heap.${HEAP_SUFFIX} autoconf-test >/dev/null 2>&1
        if test "$?" = "0"; then
            AC_MSG_RESULT([yes])
        else
            AC_MSG_RESULT([no])
            HEAP2EXEC=false
        fi
    else
        AC_MSG_RESULT([no])
        HEAP2EXEC=false
    fi
    rm -f autoconf-test.heap.${HEAP_SUFFIX}
    rm -f autoconf-test

    AC_MSG_CHECKING([availability of smlnj-tdp tools])
    GET_SMLNJ_CM_ANCHOR_HOME([SMLNJ_TDP], [smlnj-tdp])
    if test -z "$SMLNJ_TDP"; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([$SMLNJ_TDP])
    fi
fi

# 3. MLton dialect

if test "$DIALECT" = "mlton"; then
    if test "$MLTON" = "yes" -o "$MLTON" = "<<search>>"; then
        AC_PATH_PROGS([MLTON], [mlton])
        if test -z "$MLTON"; then
            AC_MSG_ERROR([cannot find installation of MLton])
        fi
    fi
    CHECK_EXECUTABLE([mlton], [$MLTON])
else
    MLTON=false
fi

dnl COMMON_SOURCES=`ls -1 *.sml | grep -v mlton | grep -v smlnj | grep -v decimal-external | xargs echo`
dnl AC_SUBST([COMMON_SOURCES])

AC_SUBST([DIALECT])
AC_SUBST([DEBUG])
AC_SUBST([HEAP_SUFFIX])
AC_SUBST([SMLNJ_TDP])
AC_SUBST([SMLNJ])
AC_SUBST([MLBUILD])
AC_SUBST([HEAP2EXEC])
AC_SUBST([MLTON])

AC_CONFIG_FILES([
  Makefile
])

AC_OUTPUT
