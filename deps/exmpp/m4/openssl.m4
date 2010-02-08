dnl
dnl Configure path for OpenSSL
dnl

dnl EXMPP_OPENSSL([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl Substitutes
dnl   OPENSSL_CPPFLAGS
dnl   OPENSSL_LDFLAGS
dnl   OPENSSL_LIBS
AC_DEFUN([EXMPP_OPENSSL],
[
  AC_ARG_WITH(openssl,
    AC_HELP_STRING([--with-openssl=PREFIX],
      [prefix where OpenSSL is installed (optional)]),
    openssl_prefix="$withval",)

  no_openssl=""
  OPENSSL_CPPFLAGS=""
  OPENSSL_LDFLAGS=""
  OPENSSL_LIBS="-lssl -lcrypto"

  if test x"${openssl_prefix:+set}" = "xset"; then
    OPENSSL_CPPFLAGS="-I${openssl_prefix%%\/}/include ${OPENSSL_CPPFLAGS}"
    OPENSSL_LDFLAGS="-L${openssl_prefix%%\/}/lib ${OPENSSL_LDFLAGS}"
  fi

  ac_save_CPPFLAGS="$CPPFLAGS"
  ac_save_LDFLAGS="$LDFLAGS"
  ac_save_LIBS="$LIBS"
  CPPFLAGS="$CPPFLAGS $OPENSSL_CPPFLAGS"
  LDFLAGS="$LDFLAGS $OPENSSL_LDFLAGS"

  AC_CHECK_HEADERS(openssl/ssl.h,, no_openssl="yes",)

  AC_CHECK_LIB(ssl, SSL_CTX_new,, no_openssl="yes")

  CPPFLAGS="$ac_save_CPPFLAGS"
  LDFLAGS="$ac_save_LDFLAGS"
  LIBS="$ac_save_LIBS"

  AC_MSG_CHECKING([for OpenSSL library])
  if test x"$no_openssl" = "x"; then
    AC_MSG_RESULT([yes])
    ifelse([$1], , :, [$1])
  else
    AC_MSG_RESULT([no])
    ifelse([$2], , :, [$2])

    OPENSSL_CPPFLAGS=""
    OPENSSL_LDFLAGS=""
    OPENSSL_LIBS=""
  fi

  AC_SUBST(OPENSSL_CPPFLAGS)
  AC_SUBST(OPENSSL_LDFLAGS)
  AC_SUBST(OPENSSL_LIBS)
])
