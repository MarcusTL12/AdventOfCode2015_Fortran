#define K_TYPE vec_int
#define K_ALIAS vec_int
#define K_MOD vec_int_mod

#define V_TYPE integer
#define V_ALIAS int

#define M_SHOW

#include "../../fortran_utils/src/templates/hashmap.f90_template"


#define K_TYPE astring
#define K_ALIAS str
#define K_MOD astring_mod
#define K_SHOW_MOD astring_show_mod

#define V_TYPE integer
#define V_ALIAS int

#define M_SHOW

#include "../../fortran_utils/src/templates/hashmap.f90_template"


#define K_TYPE astring
#define K_ALIAS str
#define K_MOD astring_mod
#define K_SHOW_MOD astring_show_mod

#define V_TYPE vec_str
#define V_ALIAS vec_str
#define V_MOD vec_str_mod

#define M_SHOW

#include "../../fortran_utils/src/templates/hashmap.f90_template"
