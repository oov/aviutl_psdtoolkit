#pragma once

#include <lualib.h>
#include <ovbase.h>

int luafn_err_(lua_State *const L, error e, char const *const funcname);
#define luafn_err(L, err) luafn_err_((L), (err), (__func__))
