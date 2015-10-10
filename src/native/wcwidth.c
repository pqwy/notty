#define _XOPEN_SOURCE
#include <wchar.h>
#include <locale.h>
#include <caml/mlvalues.h>

__attribute__((constructor))
void notty_init_locale () {
  setlocale (LC_CTYPE, "");
}

CAMLprim value caml_notty_wcwidth (value v) {
  return Val_int (wcwidth ((wchar_t) Int_val (v)));
}
