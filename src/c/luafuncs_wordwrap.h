#pragma once

#include <lualib.h>

int luafn_wordwrap(lua_State *L);
void cleanup_wordwrap(void);
