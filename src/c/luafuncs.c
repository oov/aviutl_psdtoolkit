#include "luafuncs.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <lua5.1/lauxlib.h>
#include <lua5.1/lualib.h>

#include "cache.h"
#include "util.h"

struct ipc *g_ipc = NULL;
struct speak *g_speak = NULL;

struct wstr g_current_project_path = {0};
int32_t g_current_frame = 0;
int32_t g_current_frame_n = 0;
int32_t g_current_render_index = 0;

static int luafn_err_(lua_State *const L, error e, char const *const funcname) {
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
#define luafn_err(L, err) luafn_err_((L), (err), (__func__))

NODISCARD static error mbcs_to_utf8(struct str const *const mbcs, struct str *const utf8) {
  struct wstr tmp = {0};
  error err = from_mbcs(mbcs, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = to_utf8(&tmp, utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

static int luafn_addfile(lua_State *L) {
  char const *const path_utf8 = lua_tostring(L, 1);
  uint32_t const tag = (uint32_t)lua_tointeger(L, 2);
  error err = ipc_add_file(g_ipc, &str_unmanaged_const(path_utf8), tag);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return efailed(err) ? luafn_err(L, err) : 0;
}

static int luafn_draw(lua_State *L) {
  struct str path_utf8 = {0};
  int32_t const id = (int32_t)lua_tointeger(L, 1);
  error err = mbcs_to_utf8(&str_unmanaged_const(lua_tostring(L, 2)), &path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  void *p = base_deconster_(lua_topointer(L, 3));
  int32_t const width = lua_tointeger(L, 4);
  int32_t const height = lua_tointeger(L, 5);
  err = ipc_draw(g_ipc, id, &path_utf8, p, width, height);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&path_utf8));
  return efailed(err) ? luafn_err(L, err) : 0;
}

static int luafn_getcache(lua_State *L) {
  error err = eok();
  char const *const key = lua_tostring(L, 1);
  void *const dest = base_deconster_(lua_topointer(L, 2));
  size_t const dest_len = (size_t)lua_tointeger(L, 3);
  void *src = NULL;
  size_t src_len = 0;
  err = cache_get(&str_unmanaged_const(key), &src, &src_len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (src_len > dest_len) {
    err = errg(err_not_sufficient_buffer);
    goto cleanup;
  }
  memcpy(dest, src, src_len);
  err = errg(err_unexpected);
cleanup:
  return efailed(err) ? luafn_err(L, err) : 0;
}

static int luafn_getcurrentframe(lua_State *L) {
  lua_pushinteger(L, g_current_frame);
  lua_pushinteger(L, g_current_frame_n);
  lua_pushinteger(L, g_current_render_index);
  return 3;
}

static int luafn_getlayernames(lua_State *L) {
  struct str path_utf8 = {0};
  struct str layer_names_utf8 = {0};
  int32_t const id = (int32_t)lua_tointeger(L, 1);
  error err = mbcs_to_utf8(&str_unmanaged_const(lua_tostring(L, 2)), &path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = ipc_get_layer_names(g_ipc, id, &path_utf8, &layer_names_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_newtable(L);
  int idx = 0;
  char const *prev = layer_names_utf8.ptr, *s = layer_names_utf8.ptr;
  while (*s != '\0') {
    if (*s == '\n') {
      lua_pushlstring(L, prev, (size_t)(s - prev));
      lua_rawseti(L, -2, ++idx);
      prev = s + 1;
    }
    ++s;
  }
  lua_pushlstring(L, prev, (size_t)(s - prev));
  lua_rawseti(L, -2, ++idx);

cleanup:
  ereport(sfree(&layer_names_utf8));
  ereport(sfree(&path_utf8));
  return efailed(err) ? luafn_err(L, err) : 1;
}

static int luafn_getspeaklevel(lua_State *L) {
  char const *const filepath = lua_tostring(L, 1);
  float const pos = (float)lua_tonumber(L, 2);
  float const low_cut = (float)lua_tonumber(L, 3);
  float const high_cut = (float)lua_tonumber(L, 4);
  float level = 0.f;
  error err = speak_get_level(g_speak, &str_unmanaged_const(filepath), pos, low_cut, high_cut, &level);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushnumber(L, (double)level);
cleanup:
  return efailed(err) ? luafn_err(L, err) : 1;
}

static int luafn_getwavlabpath(lua_State *L) {
  struct wstr path = {0};
  struct wstr tmp = {0};
  struct str mbcs = {0};
  error err = from_mbcs(&str_unmanaged_const(lua_tostring(L, 1)), &path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  bool exists = false;
  err = file_exists(&path, &exists);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (exists) {
    err = scpy(&tmp, path.ptr);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  } else if (!exists && g_current_project_path.len != 0) {
    // Couldn't find the file, but I know the path to the project,
    // so I assemble the path to load from project location.
    size_t pos = 0;
    err = extract_file_name(&g_current_project_path, &pos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = sncpy(&tmp, g_current_project_path.ptr, pos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = extract_file_name(&path, &pos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = scat(&tmp, path.ptr + pos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  } else {
    lua_pushnil(L);
    lua_pushnil(L);
    goto cleanup;
  }

  size_t extpos = 0;
  err = extract_file_extension(&tmp, &extpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  tmp.ptr[extpos] = L'\0';
  tmp.len = extpos;
  err = scat(&tmp, L".wav");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = file_exists(&tmp, &exists);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (exists) {
    err = to_mbcs(&tmp, &mbcs);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    lua_pushstring(L, mbcs.ptr);
  } else {
    lua_pushnil(L);
  }

  tmp.ptr[extpos] = L'\0';
  tmp.len = extpos;
  err = scat(&tmp, L".lab");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = file_exists(&tmp, &exists);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (exists) {
    err = to_mbcs(&tmp, &mbcs);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    lua_pushstring(L, mbcs.ptr);
  } else {
    lua_pushnil(L);
  }
cleanup:
  ereport(sfree(&mbcs));
  ereport(sfree(&tmp));
  ereport(sfree(&path));
  return efailed(err) ? luafn_err(L, err) : 2;
}

static int luafn_putcache(lua_State *L) {
  void *v = NULL;
  error err = eok();
  char const *const key = lua_tostring(L, 1);
  void *const ptr = base_deconster_(lua_topointer(L, 2));
  size_t const len = (size_t)lua_tointeger(L, 3);
  err = mem(&v, len, sizeof(char));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  memcpy(v, ptr, len);
  err = cache_put(&str_unmanaged_const(key), v, len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  v = NULL;
cleanup:
  ereport(mem_free(&v));
  return efailed(err) ? luafn_err(L, err) : 0;
}

static int luafn_setprops(lua_State *L) {
  struct str path_utf8 = {0};
  struct str layer_utf8 = {0};
  struct ipc_set_properties_options opt = {0};
  struct ipc_set_properties_results rets = {0};
  int32_t const id = (int32_t)lua_tointeger(L, 1);
  error err = mbcs_to_utf8(&str_unmanaged_const(lua_tostring(L, 2)), &path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  uint32_t tag = 0;
  lua_getfield(L, 3, "tag");
  if (lua_isnumber(L, -1)) {
    tag = (uint32_t)lua_tointeger(L, -1);
    opt.tag = &tag;
  }
  lua_pop(L, 1);

  lua_getfield(L, 3, "layer");
  if (lua_isstring(L, -1)) {
    err = mbcs_to_utf8(&str_unmanaged_const(lua_tostring(L, -1)), &layer_utf8);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    opt.layer_utf8 = &layer_utf8;
  }
  lua_pop(L, 1);

  float scale = 0;
  lua_getfield(L, 3, "scale");
  if (lua_isnumber(L, -1)) {
    scale = (float)lua_tonumber(L, -1);
    opt.scale = &scale;
  }
  lua_pop(L, 1);

  int32_t offset_x = 0;
  lua_getfield(L, 3, "offsetx");
  if (lua_isnumber(L, -1)) {
    offset_x = (int32_t)lua_tointeger(L, -1);
    opt.offset_x = &offset_x;
  }
  lua_pop(L, 1);

  int32_t offset_y = 0;
  lua_getfield(L, 3, "offsety");
  if (lua_isnumber(L, -1)) {
    offset_y = (int32_t)lua_tointeger(L, -1);
    opt.offset_y = &offset_y;
  }
  lua_pop(L, 1);

  err = ipc_set_properties(g_ipc, id, &path_utf8, &opt, &rets);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushboolean(L, rets.modifiled);
  lua_pushinteger(L, (lua_Integer)rets.cache_key);
  lua_pushinteger(L, rets.width);
  lua_pushinteger(L, rets.height);

cleanup:
  ereport(sfree(&layer_utf8));
  ereport(sfree(&path_utf8));
  return efailed(err) ? luafn_err(L, err) : 4;
}

static int luafn_type(lua_State *L) {
  lua_pushstring(L, lua_typename(L, lua_type(L, 1)));
  return 1;
}

int luaopen_PSDToolKitBridge(lua_State *L) {
  static luaL_Reg funcs[] = {
      {"addfile", luafn_addfile},
      {"draw", luafn_draw},
      {"getcache", luafn_getcache},
      {"getcurrentframe", luafn_getcurrentframe},
      {"getlayernames", luafn_getlayernames},
      {"getspeaklevel", luafn_getspeaklevel},
      {"getwavlabpath", luafn_getwavlabpath},
      {"putcache", luafn_putcache},
      {"setprops", luafn_setprops},
      {"type", luafn_type},
      {NULL, NULL},
  };
  luaL_register(L, "PSDToolKitBridge", funcs);
  return 1;
}
