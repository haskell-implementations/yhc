AC_INIT(yhc, 1.0, shackell@cs.york.ac.uk)

AC_CONFIG_FILES([Makefile.inc:Makefile.in])
AC_CONFIG_HEADERS([config.h:config.h.in])

#AC_CHECK_LIB(gmp, [__gmpz_init], [], [AC_MSG_ERROR(cannot find libgmp)])
AC_CHECK_LIB(gmp, __gmpz_init, ,
  [AC_CHECK_LIB(gmp, mpz_init, ,
     [AC_CHECK_LIB(gmp2, mpz_init)])
  ])

AC_CHECK_LIB(dl, [dlopen], [], [])
AC_CHECK_LIB(pthread, [pthread_create], [], [])

AC_CHECK_HEADERS([gmp.h], [], [AC_MSG_ERROR(cannot find gmp header)])

AC_C_BIGENDIAN

AC_MSG_CHECKING(for gcc computed gotos)

AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([], [[void* label = &&L_test; L_test: goto *label;]])],
        [AC_DEFINE(HAVE_GCC_LABELS, 1) AC_MSG_RESULT(yes)],
        [AC_DEFINE(HAVE_GCC_LABELS, 0) AC_MSG_RESULT(no)])

AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(void*)

AC_OUTPUT