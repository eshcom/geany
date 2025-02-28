# For complete documentation of this file, please see Geany's main documentation
[styling]
# Edit these in the colorscheme .conf file instead
default=default
std_word=keyword_1
add_word=keyword_2
com_word=keyword_1
std_func=identifier_1
add_func=identifier_1
std_macro=identifier_1
add_macro=identifier_1
glb_class=class
add_class=class
identifier=identifier_1
cpp_type=identifier_1
function=identifier_1
macro=identifier_1
preproc=preprocessor
operator=operator
number=number_1
string=string_1
stringraw=string_2
stringeol=string_eol
character=character
escapeseq=string_1
formatseq=string_1
userliteral=other
uuid=other
regex=regex
verbatim=string_2
tripleverbatim=string_2
hashquotedstring=string_2
string_continued=string_1
line_continued=preprocessor
taskmarker=comment
comment=comment
commentdoc=comment_doc
commentdockeyword=comment_doc_keyword
commentdockeyworderror=comment_doc_keyword_error
commentline=comment_line
commentlinedoc=comment_line_doc
preproccomment=comment
preproccommentdoc=comment_doc

[keywords]
# Standard keywords
std_words=_Alignas _Alignof _Atomic _Bool _Complex _Generic _Imaginary _Noreturn _Static_assert _Thread_local asm auto bool break case char const continue default do double else enum extern float for goto if inline int long register restrict return short signed sizeof static struct switch typedef union unsigned void volatile while
# Additional keywords
add_words=
# Common keywords
com_words=TRUE FALSE NULL NAN EOF WEOF true false noreturn stderr stdin stdout
# Doxygen keywords
doc_words=a addindex addtogroup anchor arg attention author authors b brief bug c callergraph callgraph category cite class code cond copybrief copydetails copydoc copyright date def defgroup deprecated details dir dontinclude dot dotfile e else elseif em endcode endcond enddot endhtmlonly endif endinternal endlatexonly endlink endmanonly endmsc endrtfonly endverbatim endxmlonly enum example exception extends file fn headerfile hideinitializer htmlinclude htmlonly if ifnot image implements include includelineno ingroup interface internal invariant latexonly li line link mainpage manonly memberof msc mscfile n name namespace nosubgrouping note overload p package page par paragraph param post pre private privatesection property protected protectedsection protocol public publicsection ref related relatedalso relates relatesalso remark remarks result return returns retval rtfonly sa section see short showinitializer since skip skipline snippet struct subpage subsection subsubsection tableofcontents test throw throws todo tparam typedef union until var verbatim verbinclude version warning weakgroup xmlonly xrefitem
# Standard library functions
std_funcs=^atomic_compare_exchange_ ^atomic_exchange ^atomic_fetch_add ^atomic_fetch_and ^atomic_fetch_or ^atomic_fetch_sub ^atomic_fetch_xor ^atomic_flag_clear ^atomic_flag_test_and_set ^atomic_load ^atomic_store abort abort_handler_s abs acos acosf acosh acoshf acoshl acosl asctime asctime_s asin asinf asinh asinhf asinhl asinl assert at_quick_exit atan atan2 atan2f atan2l atanf atanh atanhf atanhl atanl atexit atof atoi atol atoll atomic_init atomic_is_lock_free atomic_signal_fence atomic_thread_fence btowc c16rtomb c32rtomb c8rtomb cabs cabsf cabsl cacos cacosf cacosh cacoshf cacoshl cacosl carg cargf cargl casin casinf casinh casinhf casinhl casinl catan catanf catanh catanhf catanhl catanl cbrt cbrtf cbrtl ccos ccosf ccosh ccoshf ccoshl ccosl ceil ceilf ceill cexp cexpf cexpl cimag cimagf cimagl clearerr clock clog clogf clogl cnd_broadcast cnd_destroy cnd_init cnd_signal cnd_timedwait cnd_wait conj conjf conjl copysign copysignf copysignl cos cosf cosh coshf coshl cosl cpow cpowf cpowl cproj cprojf cprojl creal crealf creall csin csinf csinh csinhf csinhl csinl csqrt csqrtf csqrtl ctan ctanf ctanh ctanhf ctanhl ctanl ctime ctime_s difftime div erf erfc erfcf erfcl erff erfl exit exp exp2 exp2f exp2l expf expl expm1 expm1f expm1l fabs fabsf fabsl fclose fdim fdimf fdiml feclearexcept fegetenv fegetexceptflag fegetround feholdexcept feof feraiseexcept ferror fesetenv fesetexceptflag fesetround fetestexcept feupdateenv fflush fgetc fgetpos fgets fgetwc fgetws floor floorf floorl fma fmaf fmal fmax fmaxf fmaxl fmin fminf fminl fmod fmodf fmodl fopen fopen_s fpclassify fprintf fprintf_s fputc fputs fputwc fputws fread freopen freopen_s frexp frexpf frexpl fscanf fscanf_s fseek fsetpos ftell fwide fwprintf fwprintf_s fwrite fwscanf fwscanf_s getc getchar getenv getenv_s gets gets_s getwc getwchar gmtime gmtime_r gmtime_s hypot hypotf hypotl ignore_handler_s ilogb ilogbf ilogbl imaxabs imaxdiv isalnum isalpha isblank iscntrl isdigit isfinite isgraph isgreater isgreaterequal isinf isless islessequal islessgreater islower isnan isnormal isprint ispunct isspace isunordered isupper iswalnum iswalpha iswblank iswcntrl iswctype iswdigit iswgraph iswlower iswprint iswpunct iswspace iswupper iswxdigit isxdigit kill_dependency labs ldexp ldexpf ldexpl ldiv lgamma lgammaf lgammal llabs lldiv llrint llrintf llrintl llround llroundf llroundl localeconv localtime localtime_r localtime_s log log10 log10f log10l log1p log1pf log1pl log2 log2f log2l logb logbf logbl logf logl longjmp lrint lrintf lrintl lround lroundf lroundl mblen mbrlen mbrtoc16 mbrtoc32 mbrtoc8 mbrtowc mbsinit mbsrtowcs mbsrtowcs_s mbstowcs mbstowcs_s mbtowc memalignment memccpy memchr memcmp memcpy memcpy_s memmove memmove_s memory_order memset memset_explicit memset_s mktime modf modff modfl mtx_destroy mtx_init mtx_lock mtx_timedlock mtx_trylock mtx_unlock nan nanf nanl nearbyint nearbyintf nearbyintl nextafter nextafterf nextafterl nexttoward nexttowardf nexttowardl offsetof perror pow powf powl printf printf_s putc putchar puts putwc putwchar quick_exit raise remainder remainderf remainderl remove remquo remquof remquol rename rewind rint rintf rintl round roundf roundl scalbln scalblnf scalblnl scalbn scalbnf scalbnl scanf scanf_s set_constraint_handler_s setbuf setjmp setlocale setvbuf signal signbit sin sinf sinh sinhf sinhl sinl snprintf snprintf_s snwprintf_s sprintf sprintf_s sqrt sqrtf sqrtl sscanf sscanf_s static_assert strcat strcat_s strchr strcmp strcoll strcpy strcpy_s strcspn strdup strerror strerror_s strerrorlen_s strfromd strfromf strfroml strftime strlen strncat strncat_s strncmp strncpy strncpy_s strndup strnlen_s strpbrk strrchr strspn strstr strtod strtof strtoimax strtok strtok_s strtol strtold strtoll strtoul strtoull strtoumax strxfrm swprintf swprintf_s swscanf swscanf_s system tan tanf tanh tanhf tanhl tanl tgamma tgammaf tgammal thrd_create thrd_current thrd_detach thrd_equal thrd_exit thrd_join thrd_sleep thrd_yield thread_local time timespec_get timespec_getres tmpfile tmpfile_s tmpnam tmpnam_s tolower toupper towctrans towlower towupper trunc truncf truncl tss_create tss_delete tss_get tss_set ungetc ungetwc unreachable va_arg va_copy va_end va_start vfprintf vfprintf_s vfscanf vfscanf_s vfwprintf vfwprintf_s vfwscanf vfwscanf_s vprintf vprintf_s vscanf vscanf_s vsnprintf vsnprintf_s vsnwprintf_s vsprintf vsprintf_s vsscanf vsscanf_s vswprintf vswprintf_s vswscanf vswscanf_s vwprintf vwprintf_s vwscanf vwscanf_s wcrtomb wcrtomb_s wcscat wcscat_s wcschr wcscmp wcscoll wcscpy wcscpy_s wcscspn wcsftime wcslen wcsncat wcsncat_s wcsncmp wcsncpy wcsncpy_s wcsnlen_s wcspbrk wcsrchr wcsrtombs wcsrtombs_s wcsspn wcsstr wcstod wcstof wcstoimax wcstok wcstok_s wcstol wcstold wcstoll wcstombs wcstombs_s wcstoul wcstoull wcstoumax wcsxfrm wctob wctomb wctomb_s wctrans wctype wmemchr wmemcmp wmemcpy wmemcpy_s wmemmove wmemmove_s wmemset wprintf wprintf_s wscanf wscanf_s
# Additional library functions
add_funcs=
# Standard library macros
std_macros=^ATOMIC_ ^CHAR_ ^CMPLX ^DBL_ ^EXIT_ ^FE_ ^FLT_ ^FP_ ^HUGE_ ^INT ^INT16_ ^INT32_ ^INT64_ ^INT8_ ^INTMAX_ ^INTPTR_ ^INT_ ^LC_ ^LDBL_ ^LLONG_ ^LONG_ ^L_tmpnam ^MATH_ ^PTRDIFF_ ^SCHAR_ ^SEEK_ ^SHRT_ ^SIG_ ^SIZE_ ^TMP_MAX ^UCHAR_ ^UINT ^UINT16_ ^UINT32_ ^UINT64_ ^UINT8_ ^UINTMAX_ ^UINTPTR_ ^UINT_ ^ULLONG_ ^ULONG_ ^USHRT_ ^WCHAR_ ^WINT_ ^__CHAR_ ^__FILE_ ^__FP_ ^__GCC_ ^__GNU ^__GXX_ ^__INT ^__LONG_ ^__NO_ ^__OPTIMIZE_ ^__ORDER_ ^__PTRDIFF_ ^__SANITIZE_ ^__SCHAR_ ^__SHRT_ ^__SIG_ ^__SIZE ^__SSP_ ^__STDC ^__TIME ^__UINT ^__WCHAR_ ^__WINT_ BITINT_MAXWIDTH BOOL_WIDTH BUFSIZ CLOCKS_PER_SEC DECIMAL_DIG EDOM EILSEQ ERANGE FILENAME_MAX FOPEN_MAX I INFINITY MAX MB_CUR_MAX MB_LEN_MAX MIN RSIZE_MAX SIGABRT SIGFPE SIGILL SIGINT SIGSEGV SIGTERM TSS_DTOR_ITERATIONS _Complex_I _IOFBF _IOLBF _IONBF _Imaginary_I _LP64 _WIN32 _WIN64 __ASSEMBLER__ __ASSOCIATIVE_MATH__ __BASE_FILE__ __BYTE_ORDER__ __COUNTER__ __DATA__ __DATE__ __DEPRECATED __ELF__ __EXCEPTIONS __FLOAT_WORD_ORDER__ __GFORTRAN__ __HAVE_SPECULATION_SAFE_VALUE __INCLUDE_LEVEL__ __LINE__ __LP64__ __NEXT_RUNTIME__ __OBJC__ __OPTIMIZE__ __RECIPROCAL_MATH__ __REGISTER_PREFIX__ __ROUNDING_MATH__ __SANITIZE_THREAD__ __STRICT_ANSI__ __USER_LABEL_PREFIX__ __USING_SJLJ_EXCEPTIONS__ __VERSION__ __cplusplus __func__ __mips__ __unix__
# Additional library macros
add_macros=
# Additional library classes
add_classes=

[lexer_properties]
styling.within.preprocessor=1
lexer.cpp.track.preprocessor=0
lexer.cpp.escape.sequence=0
lexer.cpp.format.sequence=0

[settings]
# default extension used when saving files
extension=c

# MIME type
mime_type=text/x-csrc

# the following characters are these which a "word" can contains, see documentation
#wordchars=_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789

# single comments, like # in this file
comment_single=//
# multiline comments
comment_open=/*
comment_close=*/

# set to false if a comment character/string should start at column 0 of a line, true uses any
# indentation of the line, e.g. setting to true causes the following on pressing CTRL+d
	#command_example();
# setting to false would generate this
#	command_example();
# This setting works only for single line comments
comment_use_indent=true

# context action command (please see Geany's main documentation for details)
context_action_cmd=

[indentation]
#width=4
# 0 is spaces, 1 is tabs, 2 is tab & spaces
#type=1

[build-menu]
# %f will be replaced by the complete filename
# %e will be replaced by the filename without extension
# (use only one of it at one time)
FT_00_LB=_Compile
FT_00_LB[ru]=_Скомпилировать
FT_00_CM=gcc -Wall -c "%f"
FT_00_WD=
FT_01_LB=_Build
FT_01_LB[ru]=_Собрать
FT_01_CM=gcc -Wall -o "%e" "%f"
FT_01_WD=
FT_02_LB=_Lint
FT_02_LB[ru]=_Проверить код
FT_02_CM=cppcheck --language=c --enable=warning,style --template=gcc "%f"
FT_02_WD=
EX_00_LB=_Execute
EX_00_LB[ru]=_Выполнить
EX_00_CM="./%e"
EX_00_WD=
