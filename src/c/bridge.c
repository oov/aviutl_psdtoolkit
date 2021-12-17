#include <lua5.1/lualib.h>

static lua_CFunction g_impl = NULL;

int __declspec(dllexport) luaopen_PSDToolKitBridge(lua_State *L);
int __declspec(dllexport) luaopen_PSDToolKitBridge(lua_State *L) {
  if (!g_impl) {
    lua_pushliteral(L, "not ready yet");
    lua_error(L);
  }
  return g_impl(L);
}

void __declspec(dllexport) init(lua_CFunction impl);
void __declspec(dllexport) init(lua_CFunction impl) { g_impl = impl; }
