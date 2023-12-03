#pragma once

#include <lua5.1/lualib.h>

int luafn_wordwrap(lua_State *L);
void cleanup_wordwrap(void);
