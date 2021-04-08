/*
** $Id: linit.c $
** Initialization of libraries for lua.c and other clients
** See Copyright Notice in lua.h
*/


#define linit_c
#define LUA_LIB

/*
** If you embed Lua in your program and need to open the standard
** libraries, call luaL_openlibs in your program. If you need a
** different set of libraries, copy this file to your project and edit
** it to suit your needs.
**
** You can also *preload* libraries, so that a later 'require' can
** open the library, which is already linked to the application.
** For that, do the following code:
**
**  luaL_getsubtable(L, LUA_REGISTRYINDEX, LUA_PRELOAD_TABLE);
**  lua_pushcfunction(L, luaopen_modname);
**  lua_setfield(L, -2, modname);
**  lua_pop(L, 1);  // remove PRELOAD table
*/

#include "lprefix.h"


#include <stddef.h>

#include "lua.h"

#include "lualib.h"
#include "lauxlib.h"


/*
** these libs are loaded by lua.c and are readily available to any Lua
** program
*/
static const luaL_Reg loadedlibs[] = {
  {LUA_GNAME, luaopen_base},                          // 基础库
  {LUA_LOADLIBNAME, luaopen_package},           // 包加载库
  {LUA_COLIBNAME, luaopen_coroutine},             // 协程库       
  {LUA_TABLIBNAME, luaopen_table},                  // table 库
  {LUA_IOLIBNAME, luaopen_io},                         // io 库
  {LUA_OSLIBNAME, luaopen_os},                        // os 库
  {LUA_STRLIBNAME, luaopen_string},                  // 字符串库
  {LUA_MATHLIBNAME, luaopen_math},                // 数学库
  {LUA_UTF8LIBNAME, luaopen_utf8},                  // utf8 库
  {LUA_DBLIBNAME, luaopen_debug},                  // 调试库
  {NULL, NULL}
};


/*
初始化完了L 以后，要在要在其中luaL_openlibs
否则虚拟机里就太空了
*/
LUALIB_API void luaL_openlibs (lua_State *L) {
  const luaL_Reg *lib;
  /* "require" functions from 'loadedlibs' and set results to global table */
  for (lib = loadedlibs; lib->func; lib++) {
    luaL_requiref(L, lib->name, lib->func, 1);
    lua_pop(L, 1);  /* remove lib */
  }
}

