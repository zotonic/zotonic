dnl ------------------------------------------------------------------
dnl Autoconf initialization.
dnl ------------------------------------------------------------------

AC_INIT([exmpp], [0.9.3], [js.pedron@meetic-corp.com], [exmpp])
AC_CONFIG_SRCDIR([include/exmpp.hrl])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([ac-aux])

AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([foreign])

AC_PREREQ([2.60])
AC_REVISION([$Revision$])

dnl ------------------------------------------------------------------
dnl Internal functions for this configure script.
dnl ------------------------------------------------------------------

dnl EMKOPTS is used by Emakefile(s).
append_to_EMKOPTS () {
	if test -z "[$]EMKOPTS"; then
		EMKOPTS="[$]1"
	else
		EMKOPTS="[$]{EMKOPTS% }, [$]1"
	fi
}

dnl Expand shell variables to have a nice output in the final report.
expand_var () {
	local v=`eval echo '$'[$]1`
	while test "`echo [$]v | grep [[$]] > /dev/null && echo nok`"; do
		v=`eval echo [$]v`
	done
	echo [$]v
}

dnl ------------------------------------------------------------------
dnl Versionning.
dnl ------------------------------------------------------------------

dnl Release date.
dnl EXMPP_RELEASE_DATE=

dnl Is this a final release?
is_release=0
if test "x${EXMPP_RELEASE_DATE}" != "x"; then
	is_release=1
fi

dnl Subversion revision.
if test $is_release -eq 0; then
	EXMPP_SVNREV=`svnversion $srcdir | sed 's/^[[^:]]*://'`
	which svnversion > /dev/null; \
	if test $? -ne 0 -o "x$EXMPP_SVNREV" = "xexported"; then
		EXMPP_SVNREV=`cat $srcdir/SVNREV`
	else
		echo $EXMPP_SVNREV > $srcdir/SVNREV
	fi
	if test $EXMPP_SVNREV; then
		EXMPP_DISTSUFFIX="-r$EXMPP_SVNREV"
	fi
fi

dnl ------------------------------------------------------------------
dnl Compilater and other tools.
dnl ------------------------------------------------------------------

AC_PROG_CC_STDC

AC_PROG_LD
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_SED

COLORED_ECHO_INIT

dnl ------------------------------------------------------------------
dnl Libtool.
dnl ------------------------------------------------------------------

dnl Hack to skip C++/Fortran tests (stolen from Beep Media Player)
m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])

AM_DISABLE_STATIC
AM_ENABLE_SHARED

AM_PROG_LIBTOOL

dnl ------------------------------------------------------------------
dnl Options.
dnl ------------------------------------------------------------------

dnl Debugging option.
if test $is_release -eq 0; then
	default_enable_debug="yes"
else
	default_enable_debug="no"
fi
AC_ARG_ENABLE([debug],
	AC_HELP_STRING([--enable-debug],
	    [turn on debugging [[default=auto]]]),,
	enable_debug=$default_enable_debug)
if test "x${enable_debug}" = "xyes"; then
	dnl AC_DEFINE([DEBUG], [], [Enable debug])
	append_to_EMKOPTS "debug_info"
	append_to_EMKOPTS "{d, debug}"
else
	CPPFLAGS="-DNDEBUG ${CPPFLAGS# }"
fi

dnl Print any warnings.
if test "x$GCC" = "xyes"; then
	CFLAGS="${CFLAGS% } -Wall"
fi
append_to_EMKOPTS "report_warnings"
append_to_EMKOPTS "{warn_format, 1}"
append_to_EMKOPTS "warn_export_vars"
append_to_EMKOPTS "warn_shadow_vars"
append_to_EMKOPTS "warn_unused_import"

dnl Treat warnings as errors.
AC_ARG_ENABLE([warnings],
	AC_HELP_STRING([--enable-warnings],
	    [treat warnings as errors [[default=yes]]]),,
	enable_warnings="yes")
if test "x${enable_warnings}" = "xyes" -a "x${GCC}" = "xyes"; then
	CFLAGS="${CFLAGS% } -Werror"
fi

dnl Compatibility modules.
AC_ARG_ENABLE([compat],
	AC_HELP_STRING([--enable-compat],
	    [build compatibility modules [[default=no]]]),,
	enable_compat="no")
if test "x${enable_compat}" = "xyes"; then
	COMPAT_MODULES_START=","
	COMPAT_MODULES=""
else
	COMPAT_MODULES_START=" % Compatibility modules disabled:"
	COMPAT_MODULES="%"
fi
AM_CONDITIONAL(BUILD_COMPAT, test "x${enable_compat}" = "xyes")
AC_SUBST(COMPAT_MODULES_START)
AC_SUBST(COMPAT_MODULES)

dnl Documentation.
AC_ARG_ENABLE([documentation],
	AC_HELP_STRING([--enable-documentation],
	    [build documentation [[default=yes]]]),,
	enable_documentation="yes")
AM_CONDITIONAL(ENABLE_DOCUMENTATION, test "x${enable_documentation}" = "xyes")

dnl Examples.
AC_ARG_ENABLE([examples],
	AC_HELP_STRING([--enable-examples],
	    [build examples [[default=no]]]),,
	enable_examples="no")
AM_CONDITIONAL(ENABLE_EXAMPLES, test "x${enable_examples}" = "xyes")

dnl Internal escaping function to use.
AC_ARG_ENABLE([escaping-using-cdata],
	AC_HELP_STRING([--enable-escaping-using-cdata],
	    [escape XML text node with CDATA sections [[default=off]]]),,
	enable_escaping_using_cdata="no")
if test "x${enable_escaping_using_cdata}" = "xyes"; then
	append_to_EMKOPTS "{d, 'ESCAPE_USING_CDATA_SECTIONS'}"
fi

dnl ------------------------------------------------------------------
dnl Erlang environment.
dnl ------------------------------------------------------------------

echo
COLORED_ECHO([%BErlang environment%b])

dnl Available flags.
AC_ARG_WITH([erlang],
	AC_HELP_STRING([--with-erlang=PREFIX],
	    [prefix where build machine's Erlang is installed (optional)]),
	with_erlang=${withval%/},
	with_erlang="")

dnl erl(1) is used to compile Erlang modules.
if test "x${with_erlang}" = "x"; then
	AC_ERLANG_PATH_ERL
	AC_ERLANG_PATH_ERLC
else
	erl_path="${with_erlang}/bin"
	AC_ERLANG_PATH_ERL(, [$erl_path$PATH_SEPARATOR$PATH])
	AC_ERLANG_PATH_ERLC(, [$erl_path$PATH_SEPARATOR$PATH])
fi

if test "x${ERL}" = "x"; then
	AC_MSG_ERROR([
Erlang not found. Fill the ERL variable with erl(1) path or provide
Erlang prefix with --with-erlang.])
fi

dnl escript(1) is used by the testsuite.
AC_ARG_VAR([ESCRIPT], [Erlang/OTP interpreter command [autodetected]])

if test "x${ESCRIPT}" = "x"; then
	if test "x${with_erlang}" = "x"; then
		AC_PATH_PROG([ESCRIPT], [escript],,)
	else
		erl_path="${with_erlang}/bin"
		AC_PATH_PROG([ESCRIPT], [escript],,
		    [$erl_path$PATH_SEPARATOR$PATH])
	fi
else
	AC_MSG_CHECKING([for escript])
	AC_MSG_RESULT([$ESCRIPT])
fi

if test "x${ESCRIPT}" = "x"; then
	AC_MSG_WARN([
escript(1) not found. Fill the ESCRIPT variable with escript(1) path if
you want to use the testsuite.])
fi

dnl Get Erlang $ROOT dir and lib dir.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR

dnl Get ERTS version.
ERLANG_CHECK_ERTS
ERLANG_CHECK_RELEASE

dnl Erlang R12B-0 (ERTS 5.6) is required to build Exmpp.
AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [ge], [5.6],
    [is_erlang_r12b="yes"],
    [is_erlang_r12b="no"])
if test "x${is_erlang_r12b}" = "xno"; then
	AC_MSG_ERROR([
Erlang R12B is required to build Exmpp but only Erlang $ERLANG_RELEASE was found!])
fi

dnl Check for Erlang applications.

dnl Check for erl_interface (used by port drivers).
AC_ERLANG_CHECK_LIB([erl_interface],,
    [AC_MSG_ERROR([erl_interface was not found!])])

dnl Check for EUnit (used for the testsuite).
if test "x${ESCRIPT}" = "x"; then
	AC_ERLANG_CHECK_LIB([eunit],,)
fi

dnl Determine directories for installation.
if test "x${prefix}" = "xNONE"; then
	dnl Inside Erlang lib directory.
	ERLANG_INSTALL_LIB_DIR="${ERLANG_LIB_DIR}"
else
	dnl Under $prefix
	ERLANG_INSTALL_LIB_DIR="${prefix}"
fi

AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(exmpp, ${VERSION})

dnl SMP support.
dnl
dnl Support is enabled by default for Erlang R12B and later ; disabled
dnl otherwise.
AC_MSG_CHECKING([for SMP support])

AC_ARG_ENABLE([smp-support],
	AC_HELP_STRING([--enable-smp-support],
	    [enable SMP support [[default=yes]]]),,
	enable_smp_support="yes")
AM_CONDITIONAL(ENABLE_SMP_SUPPORT, test "x${enable_smp_support}" = "xyes")

if test "x${enable_smp_support}" = "xyes"; then
	AC_DEFINE(SMP_SUPPORT, [], [SMP support enabled in port drivers.])
	AC_DEFINE(USE_RWLOCK, [], [Use rwlocks.])
	AC_DEFINE(_REENTRANT, [], [_REENTRANT])

	AC_MSG_RESULT([yes (Erlang $ERLANG_RELEASE)])
else
	AC_MSG_RESULT([no (disabled by user)])
fi

dnl Do we have all the tools for the testsuite to be used.
if test "x${ERLANG_LIB_DIR_eunit}" != "xnot found" -a "x${ESCRIPT}" != "x"; then
	enable_testsuite="yes"
else
	enable_testsuite="no"
fi
AM_CONDITIONAL([ENABLE_TESTSUITE], [test "x${enable_testsuite}" = "xyes"])

dnl ------------------------------------------------------------------
dnl Dependencies.
dnl ------------------------------------------------------------------

echo
COLORED_ECHO([%BXML parsers dependencies%b])

dnl Expat port driver.
EXMPP_EXPAT(build_with_expat="yes", build_with_expat="no")
AM_CONDITIONAL(WITH_EXPAT, test "x${build_with_expat}" = "xyes")
if test "x${build_with_expat}" = "xyes"; then
	append_to_EMKOPTS "{d, 'HAVE_EXPAT'}"
fi

dnl LibXML2 port driver.
PKG_CHECK_MODULES([LIBXML2], [libxml-2.0],
    build_with_libxml2="yes", build_with_libxml2="no")
AM_CONDITIONAL(WITH_LIBXML2, test "x${build_with_libxml2}" = "xyes")
if test "x${build_with_libxml2}" = "xyes"; then
	append_to_EMKOPTS "{d, 'HAVE_LIBXML2'}"
fi

echo
COLORED_ECHO([%BTLS engines dependencies%b])

dnl OpenSSL TLS port driver.
EXMPP_OPENSSL(build_with_openssl="yes", build_with_openssl="no")
AM_CONDITIONAL(WITH_OPENSSL, test "x${build_with_openssl}" = "xyes")
if test "x${build_with_openssl}" = "xyes"; then
	append_to_EMKOPTS "{d, 'HAVE_OPENSSL'}"
fi

echo
COLORED_ECHO([%BCompression engines dependencies%b])

dnl Zlib compression port driver.
EXMPP_ZLIB(build_with_zlib="yes", build_with_zlib="no")
AM_CONDITIONAL(WITH_ZLIB, test "x${build_with_zlib}" = "xyes")
if test "x${build_with_zlib}" = "xyes"; then
	append_to_EMKOPTS "{d, 'HAVE_ZLIB'}"
fi

dnl ------------------------------------------------------------------
dnl Detect Operating System.
dnl ------------------------------------------------------------------

echo
COLORED_ECHO([%BDetect Operating System%b])

AC_CANONICAL_SYSTEM
#AC_DEFINE_UNQUOTED(CPU_VENDOR_OS, "$target")
#AC_SUBST(target_os)
case "$target_os" in
    *darwin10*)
	echo "Target OS is: Darwin10 (Mac OS X)"
	AC_LANG(Erlang)
	AC_RUN_IFELSE(
	    [AC_LANG_PROGRAM([],[dnl
	     halt(case erlang:system_info(wordsize) of
	     8 -> 0; 4 -> 1 end)])],
	    [AC_MSG_NOTICE(found 64-bit Erlang)
	     CBIT=-m64],
	    [AC_MSG_NOTICE(found 32-bit Erlang)
	     CBIT=-m32])
	;;
    *)        
	echo "Target OS is: '$target_os'"
	CBIT=""
	;;
esac
CFLAGS="$CFLAGS $CBIT"
LD_SHARED="$LD_SHARED $CBIT"
echo "CBIT = '$CBIT'"
echo "CFLAGS = '$CFLAGS'"
echo "LD_SHARED = '$LD_SHARED'"

dnl ------------------------------------------------------------------
dnl Finale substitutions.
dnl ------------------------------------------------------------------

CPPFLAGS="${CPPFLAGS# }"
CFLAGS="${CFLAGS# }"
LDFLAGS="${LDFLAGS# }"

CPPFLAGS="${CPPFLAGS% }"
CFLAGS="${CFLAGS% }"
LDFLAGS="${LDFLAGS% }"

AC_SUBST(EMKOPTS)

AC_SUBST(EXMPP_RELEASE_DATE)

AC_SUBST(EXMPP_SVNREV)
AC_SUBST(EXMPP_DISTSUFFIX)

AC_SUBST(EXMPP_BUILD_ARCH)
AC_SUBST(EXMPP_HOST_ARCH)

exp_ERLANG_INSTALL_LIB_DIR_exmpp=`expand_var ERLANG_INSTALL_LIB_DIR_exmpp`
AC_SUBST(exp_ERLANG_INSTALL_LIB_DIR_exmpp)

AC_SUBST(ERL_DRIVER_LDFLAGS)

dnl ------------------------------------------------------------------
dnl Autoconf output.
dnl ------------------------------------------------------------------

echo
AM_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
	c_src/Makefile
	include/Makefile
	include/internal/Makefile
	src/Makefile
	src/Emakefile
	ebin/Makefile
	ebin/exmpp.app
	ebin/exmpp.appup
	doc/Makefile
	testsuite/Makefile
	examples/Makefile
	examples/Emakefile
	Makefile
])
AC_CONFIG_FILES([
	testsuite/etest
], [chmod +x testsuite/etest])
AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------

echo
COLORED_ECHO([ %B== exmpp ${PACKAGE_VERSION}${EXMPP_DISTSUFFIX} ==%b])
echo
COLORED_ECHO([Configuration:])
COLORED_ECHO([    Prefix:             ${prefix}])
COLORED_ECHO([    Application dir.:   ${exp_ERLANG_INSTALL_LIB_DIR_exmpp}])
echo
COLORED_ECHO([    C compiler:         ${CC} ${CFLAGS}])
COLORED_ECHO([    Erlang emulator:    ${ERL}])
COLORED_ECHO([    Erlang compiler:    ${ERLC}])
COLORED_ECHO([    Erlang interpreter: ${ESCRIPT}])
echo
COLORED_ECHO([    Debug/warnings:     ${enable_debug}/${enable_warnings}])
COLORED_ECHO([    Compat modules:     ${enable_compat}])
COLORED_ECHO([    Documentation:      ${enable_documentation}])
COLORED_ECHO([    Testsuite:          ${enable_testsuite}])
COLORED_ECHO([    Examples:           ${enable_examples}])
echo
COLORED_ECHO([    XML parsers:])
COLORED_ECHO([      . Expat:          ${build_with_expat}])
COLORED_ECHO([      . LibXML2:        ${build_with_libxml2}])
echo
COLORED_ECHO([    TLS engines:])
COLORED_ECHO([      . OpenSSL:        ${build_with_openssl}])
echo
COLORED_ECHO([    Compression engines:])
COLORED_ECHO([      . Zlib:           ${build_with_zlib}])
echo
