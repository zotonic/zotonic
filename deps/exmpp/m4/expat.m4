dnl
dnl Configure path for Expat
dnl

dnl EXMPP_EXPAT([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl Substitutes
dnl   EXPAT_CPPFLAGS
dnl   EXPAT_LDFLAGS
dnl   EXPAT_LIBS
AC_DEFUN([EXMPP_EXPAT],
[
  AC_ARG_WITH(expat,
    AC_HELP_STRING([--with-expat=PREFIX],
      [prefix where Expat is installed (optional)]),
    expat_prefix="$withval",)

  no_expat=""
  EXPAT_CPPFLAGS=""
  EXPAT_LDFLAGS=""
  EXPAT_LIBS="-lexpat"

  if test x"${expat_prefix:+set}" = "xset"; then
    EXPAT_CPPFLAGS="-I${expat_prefix%%\/}/include ${EXPAT_CPPFLAGS}"
    EXPAT_LDFLAGS="-L${expat_prefix%%\/}/lib ${EXPAT_LDFLAGS}"
  fi

  ac_save_CPPFLAGS="$CPPFLAGS"
  ac_save_LDFLAGS="$LDFLAGS"
  ac_save_LIBS="$LIBS"
  CPPFLAGS="$CPPFLAGS $EXPAT_CPPFLAGS"
  LDFLAGS="$LDFLAGS $EXPAT_LDFLAGS"

  AC_CHECK_HEADERS(expat.h,, no_expat="yes",)

  AC_CHECK_LIB(expat, XML_ParserCreate,, no_expat="yes")

  CPPFLAGS="$ac_save_CPPFLAGS"
  LDFLAGS="$ac_save_LDFLAGS"
  LIBS="$ac_save_LIBS"

  AC_MSG_CHECKING([for Expat library])
  if test x"$no_expat" = "x"; then
    AC_MSG_RESULT([yes])
    ifelse([$1], , :, [$1])
  else
    AC_MSG_RESULT([no])
    ifelse([$2], , :, [$2])

    EXPAT_CPPFLAGS=""
    EXPAT_LDFLAGS=""
    EXPAT_LIBS=""
  fi

  AC_SUBST(EXPAT_CPPFLAGS)
  AC_SUBST(EXPAT_LDFLAGS)
  AC_SUBST(EXPAT_LIBS)
])
