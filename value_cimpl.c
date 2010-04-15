#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "caml/custom.h"

#define Is_custom(v) (Tag_val(v) == Custom_tag)
#define Is_custom_id(v, id) (Tag_val(v) == Custom_tag && 0 == strcmp(Custom_ops_val(v)->identifier, cstr_id))

CAMLexport value inspect_custom_id(value v)
{
  CAMLparam1(v);
  CAMLlocal1(result);
  if (Is_custom(v)) {
    result = caml_copy_string(Custom_ops_val(v)->identifier);
  }
  else {
    caml_invalid_argument("Value.custom_id");
  }
  CAMLreturn(result);
}

CAMLexport value inspect_bits(value v)
{
  CAMLparam1(v);
  CAMLreturn(caml_copy_nativeint(v));
}

#define Inspect_custom_has(op)						\
  CAMLexport value inspect_custom_has_##op(value v)			\
  { return Val_bool(Is_custom(v) && Custom_ops_val(v)->op != NULL); }

Inspect_custom_has (finalize)
Inspect_custom_has (compare)
Inspect_custom_has (hash)
Inspect_custom_has (serialize)
Inspect_custom_has (deserialize)

#undef Inspect_custom_has

