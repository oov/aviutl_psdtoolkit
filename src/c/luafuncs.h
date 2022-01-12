#pragma once

#include "ovbase.h"

#include <lua5.1/lualib.h>

#include "ipc.h"
#include "speak.h"

extern struct wstr g_current_project_path;
extern struct ipc *g_ipc;
extern struct speak *g_speak;
extern int32_t g_current_frame;
extern int32_t g_current_frame_n;
extern int32_t g_current_render_index;

int luaopen_PSDToolKitBridge(lua_State *L);
