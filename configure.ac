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
         [echo -e '$1;' | (${ES4_SMLNJ} >/dev/null) 2>&1])

AC_DEFUN([EVAL_SMLNJ_STRING],
         [$1=`echo -e 'TextIO.output (TextIO.stdErr, $2);' | (${ES4_SMLNJ} >/dev/null) 2>&1`])

AC_DEFUN([GET_SMLNJ_HEAP_SUFFIX],
         [EVAL_SMLNJ_STRING($1, [SMLofNJ.SysInfo.getHeapSuffix()])])

AC_DEFUN([GET_SMLNJ_CM_ANCHOR_HOME],
         [EVAL_SMLNJ_STRING($1, [valOf(case (CM.Anchor.anchor "$2") of { get, ... } => get ())])])

ES4_SML_DIALECT=smlnj
ES4_SMLNJ="<<search>>"
ES4_MLTON="<<search>>"
ES4_DEBUG=no

AC_ARG_WITH([smlnj],
            AC_HELP_STRING([--with-smlnj=FULLPATH],
                           [set full path to SML/NJ executable]),
            [ES4_SMLNJ="$withval"])
AC_ARG_WITH([mlton],
            AC_HELP_STRING([--with-mlton=FULLPATH],
                           [set full path to MLton compiler]),
            [ES4_MLTON="$withval"])
AC_ARG_WITH([dialect],
            AC_HELP_STRING([--with-dialect=(smlnj|mlton)],
                           [choose dialect of Standard ML (default smlnj)]),
            [ES4_SML_DIALECT="$withval"])
AC_ARG_ENABLE([debug],
              AC_HELP_STRING([--enable-debug],
                             [compile SML with debugging information (default no)]),
              [ES4_DEBUG="$enableval"])

AC_MSG_NOTICE([using ${ES4_SML_DIALECT} dialect for building releases])

# 1. Find SML/NJ installation information.
if test "${ES4_SMLNJ}" = "yes" -o "${ES4_SMLNJ}" = "<<search>>"; then
    AC_PATH_PROGS([ES4_SMLNJ], [sml])
fi

if test -n "${ES4_SMLNJ}" -a -x "${ES4_SMLNJ}"; then
    ES4_SMLNJ_HOME=`dirname ${ES4_SMLNJ}`
    ES4_MLBUILD="${ES4_SMLNJ_HOME}"/ml-build
    ES4_HEAP2EXEC="${ES4_SMLNJ_HOME}"/heap2exec
    AC_MSG_CHECKING([platform-specific heapfile suffix])
    GET_SMLNJ_HEAP_SUFFIX([ES4_HEAP_SUFFIX])
    AC_MSG_RESULT([${ES4_HEAP_SUFFIX}])
else
    ES4_SMLNJ=false
    ES4_SMLNJ_HOME=XXX
    ES4_MLBUILD=false
    ES4_HEAP2EXEC=false
    ES4_HEAP_SUFFIX=XXX
fi

# 2. SML/NJ dialect

if test "${ES4_SML_DIALECT}" = "smlnj"; then
    if test "${ES4_SMLNJ}" = "false"; then
        AC_MSG_ERROR([cannot execute sml])
    fi
    CHECK_EXECUTABLE([ml-build], [${ES4_MLBUILD}])
    AC_MSG_CHECKING([usability of heap2exec on this platform])
    if test -x "${ES4_HEAP2EXEC}"; then
        EXEC_SMLNJ([SMLofNJ.exportFn("autoconf-test.heap",(fn _ => OS.Process.success))])
        "${ES4_HEAP2EXEC}" autoconf-test.heap.${ES4_HEAP_SUFFIX} autoconf-test >/dev/null 2>&1
        if test "$?" = "0"; then
            AC_MSG_RESULT([yes])
        else
            AC_MSG_RESULT([no])
            ES4_HEAP2EXEC=false
        fi
    else
        AC_MSG_RESULT([no])
        ES4_HEAP2EXEC=false
    fi
    rm -f autoconf-test.heap.${ES4_HEAP_SUFFIX}
    rm -f autoconf-test

    AC_MSG_CHECKING([availability of smlnj-tdp tools])
    GET_SMLNJ_CM_ANCHOR_HOME([ES4_SMLNJ_TDP], [smlnj-tdp])
    if test -z "${ES4_SMLNJ_TDP}"; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([${ES4_SMLNJ_TDP}])
    fi
fi

# 3. MLton dialect

if test "${ES4_SML_DIALECT}" = "mlton"; then
    if test "${ES4_MLTON}" = "yes" -o "${ES4_MLTON}" = "<<search>>"; then
        AC_PATH_PROGS([ES4_MLTON], [mlton])
        if test -z "${ES4_MLTON}"; then
            AC_MSG_ERROR([cannot find installation of MLton])
        fi
    fi
    CHECK_EXECUTABLE([mlton], [${ES4_MLTON}])
else
    ES4_MLTON=false
fi

dnl COMMON_SOURCES=`ls -1 *.sml | grep -v mlton | grep -v smlnj | grep -v decimal-external | xargs echo`
dnl AC_SUBST([COMMON_SOURCES])

AC_SUBST([ES4_SML_DIALECT])
AC_SUBST([ES4_DEBUG])
AC_SUBST([ES4_HEAP_SUFFIX])
AC_SUBST([ES4_SMLNJ_TDP])
AC_SUBST([ES4_SMLNJ])
AC_SUBST([ES4_MLBUILD])
AC_SUBST([ES4_HEAP2EXEC])
AC_SUBST([ES4_MLTON])

AC_CONFIG_FILES([
  Makefile
])

AC_OUTPUT
