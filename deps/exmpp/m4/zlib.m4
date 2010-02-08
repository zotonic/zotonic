dnl
dnl Configure path for Zlib
dnl

dnl EXMPP_ZLIB([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl Substitutes
dnl   ZLIB_CPPFLAGS
dnl   ZLIB_LDFLAGS
dnl   ZLIB_LIBS
AC_DEFUN([EXMPP_ZLIB],
[
  AC_ARG_WITH(zlib,
    AC_HELP_STRING([--with-zlib=PREFIX],
      [prefix where Zlib is installed (optional)]),
    zlib_prefix="$withval",)

  no_zlib=""
  ZLIB_CPPFLAGS=""
  ZLIB_LDFLAGS=""
  ZLIB_LIBS="-lz"

  if test x"${zlib_prefix:+set}" = "xset"; then
    ZLIB_CPPFLAGS="-I${zlib_prefix%%\/}/include ${ZLIB_CPPFLAGS}"
    ZLIB_LDFLAGS="-L${zlib_prefix%%\/}/lib ${ZLIB_LDFLAGS}"
  fi

  ac_save_CPPFLAGS="$CPPFLAGS"
  ac_save_LDFLAGS="$LDFLAGS"
  ac_save_LIBS="$LIBS"
  CPPFLAGS="$CPPFLAGS $ZLIB_CPPFLAGS"
  LDFLAGS="$LDFLAGS $ZLIB_LDFLAGS"

  AC_CHECK_HEADERS(zlib.h,, no_zlib="yes",)

  AC_CHECK_LIB(z, zlibVersion,, no_zlib="yes")

  CPPFLAGS="$ac_save_CPPFLAGS"
  LDFLAGS="$ac_save_LDFLAGS"
  LIBS="$ac_save_LIBS"

  AC_MSG_CHECKING([for Zlib library])
  if test x"$no_zlib" = "x"; then
    AC_MSG_RESULT([yes])
    ifelse([$1], , :, [$1])
  else
    AC_MSG_RESULT([no])
    ifelse([$2], , :, [$2])

    ZLIB_CPPFLAGS=""
    ZLIB_LDFLAGS=""
    ZLIB_LIBS=""
  fi

  AC_SUBST(ZLIB_CPPFLAGS)
  AC_SUBST(ZLIB_LDFLAGS)
  AC_SUBST(ZLIB_LIBS)
])
