#pragma once

#include <ovbase.h>

#include <lua5.1/lauxlib.h>
#include <lua5.1/lualib.h>

int luafn_wordwrap(lua_State *L);
void cleanup_wordwrap(void);
