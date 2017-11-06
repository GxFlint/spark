#include "erl_nif.h"

static ERL_NIF_TERM cube_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  // Actual variable declarations from the parameters
  int a;
  // Assigning the contents of 'env' to the variables
  if (!enif_get_int(env, argv[0], &a))
    return enif_make_badarg(env);
  // Your actual (mostly untached) code

  return enif_make_int(env, a * a * a);
}

static ErlNifFunc niffuncs[] = {{"cube", 1, cube_nif, 0}};

ERL_NIF_INIT(example, niffuncs, NULL, NULL, NULL, NULL)
