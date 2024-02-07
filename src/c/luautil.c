#include "luautil.h"

#include <lauxlib.h>
#include <ovutil/win32.h>

int luafn_err_(lua_State *const L, error e, char const *const funcname) {
  struct wstr msg = {0};
  struct wstr errmsg = {0};
  struct str s = {0};

  luaL_where(L, 1);
  if (strncmp(funcname, "luafn_", 6) == 0) {
    lua_pushstring(L, "error on PSDToolKitBridge.");
    lua_pushstring(L, funcname + 6);
  } else {
    lua_pushstring(L, "error on ");
    lua_pushstring(L, funcname);
  }
  lua_pushstring(L, "():\r\n");
  error err = error_to_string(e, &errmsg);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&msg, errmsg.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = to_mbcs(&errmsg, &s);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushlstring(L, s.ptr, s.len);
  lua_concat(L, 5);

cleanup:
  if (efailed(err)) {
    efree(&err);
    lua_pushstring(L, "failed to build error message");
    lua_concat(L, 5);
  }
  ereport(sfree(&msg));
  ereport(sfree(&errmsg));
  ereport(sfree(&s));
  efree(&e);
  return lua_error(L);
}
