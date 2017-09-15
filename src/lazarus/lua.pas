(******************************************************************************
 *                                                                            *
 *  File:        lua.pas                                                      *
 *  Authors:     TeCGraf           (C headers + actual Lua libraries)         *
 *               Lavergne Thomas   (original translation to Pascal)           *
 *               Bram Kuijvenhoven (update to Lua 5.1.1 for FreePascal)       *
 *  Description: Basic Lua library                                            *
 *                                                                            *
 ******************************************************************************)

(*
** $Id: lua.h,v 1.175 2003/03/18 12:31:39 roberto Exp $
** Lua - An Extensible Extension Language
** TeCGraf: Computer Graphics Technology Group, PUC-Rio, Brazil
** http://www.lua.org   mailto:info@lua.org
** See Copyright Notice at the end of this file
*)
(*
** Updated to Lua 5.1.1 by Bram Kuijvenhoven (bram at kuijvenhoven dot net),
**   Hexis BV (http://www.hexis.nl), the Netherlands
** Notes:
**    - Only tested with FPC (FreePascal Compiler)
**    - Using LuaBinaries styled DLL/SO names, which include version names
**    - LUA_YIELD was suffixed by '_' for avoiding name collision
*)
(*
** Translated to pascal by Lavergne Thomas
** Notes :
**    - Pointers type was prefixed with 'P'
**    - lua_upvalueindex constant was transformed to function
**    - Some compatibility function was isolated because with it you must have
**      lualib.
**    - LUA_VERSION was suffixed by '_' for avoiding name collision.
** Bug reports :
**    - thomas.lavergne@laposte.net
**   In french or in english
*)

{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

unit lua;

interface

const
  LUA_LIB_NAME = 'lua51.dll';

type
  size_t = cardinal;
  Psize_t = ^size_t;

const
  LUA_VERSION = 'Lua 5.1';
  LUA_RELEASE = 'Lua 5.1.1';
  LUA_VERSION_NUM = 501;
  LUA_COPYRIGHT = 'Copyright (C) 1994-2006 Lua.org, PUC-Rio';
  LUA_AUTHORS = 'R. Ierusalimschy, L. H. de Figueiredo & W. Celes';

  (* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET = -1;

(*
** pseudo-indices
*)
  LUA_REGISTRYINDEX = -10000;
  LUA_ENVIRONINDEX = -10001;
  LUA_GLOBALSINDEX = -10002;

function lua_upvalueindex(I: integer): integer;

const
  (* thread status; 0 is OK *)
  LUA_YIELD_ = 1;
  LUA_ERRRUN = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM = 4;
  LUA_ERRERR = 5;

type
  Plua_State = Pointer;

  lua_CFunction = function(L: Plua_State): integer; cdecl;

(*
** functions that read/write blocks when loading/dumping Lua chunks
*)
type
  lua_Reader = function(L: Plua_State; ud: Pointer; sz: Psize_t): PChar; cdecl;
  lua_Writer = function(L: Plua_State; const p: Pointer; sz: size_t;
    ud: Pointer): integer; cdecl;

(*
** prototype for memory-allocation functions
*)
  lua_Alloc = function(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;

(*
** basic types
*)
const
  LUA_TNONE = -1;

  LUA_TNIL = 0;
  LUA_TBOOLEAN = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER = 3;
  LUA_TSTRING = 4;
  LUA_TTABLE = 5;
  LUA_TFUNCTION = 6;
  LUA_TUSERDATA = 7;
  LUA_TTHREAD = 8;

  (* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;

type
  (* Type of Numbers in Lua *)
  lua_Number = double;
  lua_Integer = PtrInt;

var
(*
** state manipulation
*)
  lua_newstate: function(f: lua_Alloc; ud: Pointer): Plua_state; cdecl;
  lua_close: procedure(L: Plua_State); cdecl;
  lua_newthread: function(L: Plua_State): Plua_State; cdecl;
  lua_atpanic: function(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl;

(*
** basic stack manipulation
*)
  lua_gettop: function(L: Plua_State): integer; cdecl;
  lua_settop: procedure(L: Plua_State; idx: integer); cdecl;
  lua_pushvalue: procedure(L: Plua_State; Idx: integer); cdecl;
  lua_remove: procedure(L: Plua_State; idx: integer); cdecl;
  lua_insert: procedure(L: Plua_State; idx: integer); cdecl;
  lua_replace: procedure(L: Plua_State; idx: integer); cdecl;
  lua_checkstack: function(L: Plua_State; sz: integer): longbool; cdecl;

  lua_xmove: procedure(from, to_: Plua_State; n: integer); cdecl;

(*
** access functions (stack -> C)
*)
  lua_isnumber: function(L: Plua_State; idx: integer): longbool; cdecl;
  lua_isstring: function(L: Plua_State; idx: integer): longbool; cdecl;
  lua_iscfunction: function(L: Plua_State; idx: integer): longbool; cdecl;
  lua_isuserdata: function(L: Plua_State; idx: integer): longbool; cdecl;
  lua_type: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_typename: function(L: Plua_State; tp: integer): PChar; cdecl;

  lua_equal: function(L: Plua_State; idx1, idx2: integer): longbool; cdecl;
  lua_rawequal: function(L: Plua_State; idx1, idx2: integer): longbool; cdecl;
  lua_lessthan: function(L: Plua_State; idx1, idx2: integer): longbool; cdecl;

  lua_tonumber: function(L: Plua_State; idx: integer): lua_Number; cdecl;
  lua_tointeger: function(L: Plua_State; idx: integer): lua_Integer; cdecl;
  lua_toboolean: function(L: Plua_State; idx: integer): longbool; cdecl;
  lua_tolstring: function(L: Plua_State; idx: integer;
  len: Psize_t): PChar; cdecl;
  lua_objlen: function(L: Plua_State; idx: integer): size_t; cdecl;
  lua_tocfunction: function(L: Plua_State;
  idx: integer): lua_CFunction; cdecl;
  lua_touserdata: function(L: Plua_State; idx: integer): Pointer; cdecl;
  lua_tothread: function(L: Plua_State; idx: integer): Plua_State; cdecl;
  lua_topointer: function(L: Plua_State; idx: integer): Pointer; cdecl;

(*
** push functions (C -> stack)
*)
  lua_pushnil: procedure(L: Plua_State); cdecl;
  lua_pushnumber: procedure(L: Plua_State; n: lua_Number); cdecl;
  lua_pushinteger: procedure(L: Plua_State; n: lua_Integer); cdecl;
  lua_pushlstring: procedure(L: Plua_State; const s: PChar; l_: size_t); cdecl;
  lua_pushstring: procedure(L: Plua_State; const s: PChar); cdecl;
  lua_pushvfstring: function(L: Plua_State; const fmt: PChar;
  argp: Pointer): PChar; cdecl;
  lua_pushfstring: function(L: Plua_State; const fmt: PChar): PChar;
  cdecl; varargs;
  lua_pushcclosure: procedure(L: Plua_State; fn: lua_CFunction;
  n: integer); cdecl;
  lua_pushboolean: procedure(L: Plua_State; b: longbool); cdecl;
  lua_pushlightuserdata: procedure(L: Plua_State; p: Pointer); cdecl;
  lua_pushthread: procedure(L: Plua_State); cdecl;

(*
** get functions (Lua -> stack)
*)
  lua_gettable: procedure(L: Plua_State; idx: integer); cdecl;
  lua_getfield: procedure(L: Plua_state; idx: integer; k: PChar); cdecl;
  lua_rawget: procedure(L: Plua_State; idx: integer); cdecl;
  lua_rawgeti: procedure(L: Plua_State; idx, n: integer); cdecl;
  lua_createtable: procedure(L: Plua_State; narr, nrec: integer); cdecl;
  lua_newuserdata: function(L: Plua_State; sz: size_t): Pointer; cdecl;
  lua_getmetatable: function(L: Plua_State;
  objindex: integer): integer; cdecl;
  lua_getfenv: procedure(L: Plua_State; idx: integer); cdecl;

(*
** set functions (stack -> Lua)
*)
  lua_settable: procedure(L: Plua_State; idx: integer); cdecl;
  lua_setfield: procedure(L: Plua_State; idx: integer; k: PChar); cdecl;
  lua_rawset: procedure(L: Plua_State; idx: integer); cdecl;
  lua_rawseti: procedure(L: Plua_State; idx, n: integer); cdecl;
  lua_setmetatable: function(L: Plua_State;
  objindex: integer): integer; cdecl;
  lua_setfenv: function(L: Plua_State; idx: integer): integer; cdecl;

(*
** `load' and `call' functions (load and run Lua code)
*)
  lua_call: procedure(L: Plua_State; nargs, nresults: integer); cdecl;
  lua_pcall: function(L: Plua_State;
  nargs, nresults, errf: integer): integer; cdecl;
  lua_cpcall: function(L: Plua_State; func: lua_CFunction;
  ud: Pointer): integer; cdecl;
  lua_load: function(L: Plua_State; reader: lua_Reader;
  dt: Pointer; const chunkname: PChar): integer; cdecl;

  lua_dump: function(L: Plua_State; writer: lua_Writer;
  Data: Pointer): integer; cdecl;

(*
** coroutine functions
*)
  lua_yield: function(L: Plua_State; nresults: integer): integer; cdecl;
  lua_resume: function(L: Plua_State; narg: integer): integer; cdecl;
  lua_status: function(L: Plua_State): integer; cdecl;

(*
** Garbage-collection functions and options
*)
const
  LUA_GCSTOP = 0;
  LUA_GCRESTART = 1;
  LUA_GCCOLLECT = 2;
  LUA_GCCOUNT = 3;
  LUA_GCCOUNTB = 4;
  LUA_GCSTEP = 5;
  LUA_GCSETPAUSE = 6;
  LUA_GCSETSTEPMUL = 7;

var
  lua_gc: function(L: Plua_State; what, Data: integer): integer; cdecl;

(*
** miscellaneous functions
*)
  lua_error: function(L: Plua_State): integer; cdecl;

  lua_next: function(L: Plua_State; idx: integer): integer; cdecl;

  lua_concat: procedure(L: Plua_State; n: integer); cdecl;

  lua_getallocf: function(L: Plua_State; ud: PPointer): lua_Alloc; cdecl;
  lua_setallocf: procedure(L: Plua_State; f: lua_Alloc; ud: Pointer); cdecl;

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

procedure lua_pop(L: Plua_State; n: integer);

procedure lua_newtable(L: Plua_state);

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);

function lua_strlen(L: Plua_state; i: integer): size_t;

function lua_isfunction(L: Plua_State; n: integer): boolean;
function lua_istable(L: Plua_State; n: integer): boolean;
function lua_islightuserdata(L: Plua_State; n: integer): boolean;
function lua_isnil(L: Plua_State; n: integer): boolean;
function lua_isboolean(L: Plua_State; n: integer): boolean;
function lua_isthread(L: Plua_State; n: integer): boolean;
function lua_isnone(L: Plua_State; n: integer): boolean;
function lua_isnoneornil(L: Plua_State; n: integer): boolean;

procedure lua_pushliteral(L: Plua_State; s: PChar);

procedure lua_setglobal(L: Plua_State; const s: PChar);
procedure lua_getglobal(L: Plua_State; const s: PChar);

function lua_tostring(L: Plua_State; i: integer): PChar;

(*
** compatibility macros and functions
*)

procedure lua_getregistry(L: Plua_State);

function lua_getgccount(L: Plua_State): integer;

type
  lua_Chunkreader = lua_Reader;
  lua_Chunkwriter = lua_Writer;

(*
** {======================================================================
** Debug API
** =======================================================================
*)

const
  LUA_HOOKCALL = 0;
  LUA_HOOKRET = 1;
  LUA_HOOKLINE = 2;
  LUA_HOOKCOUNT = 3;
  LUA_HOOKTAILRET = 4;

const
  LUA_MASKCALL = 1 shl Ord(LUA_HOOKCALL);
  LUA_MASKRET = 1 shl Ord(LUA_HOOKRET);
  LUA_MASKLINE = 1 shl Ord(LUA_HOOKLINE);
  LUA_MASKCOUNT = 1 shl Ord(LUA_HOOKCOUNT);

const
  LUA_IDSIZE = 60;

type
  lua_Debug = record           (* activation record *)
    event: integer;
    Name: PChar;               (* (n) *)
    namewhat: PChar;           (* (n) `global', `local', `field', `method' *)
    what: PChar;               (* (S) `Lua', `C', `main', `tail'*)
    Source: PChar;             (* (S) *)
    currentline: integer;      (* (l) *)
    nups: integer;             (* (u) number of upvalues *)
    linedefined: integer;      (* (S) *)
    lastlinedefined: integer;  (* (S) *)
    short_src: array[0..LUA_IDSIZE - 1] of char; (* (S) *)
    (* private part *)
    i_ci: integer;              (* active function *)
  end;
  Plua_Debug = ^lua_Debug;

  lua_Hook = procedure(L: Plua_State; ar: Plua_Debug); cdecl;

var
  lua_getstack: function(L: Plua_State; level: integer;
  ar: Plua_Debug): integer; cdecl;
  lua_getinfo: function(L: Plua_State; const what: PChar;
  ar: Plua_Debug): integer; cdecl;
  lua_getlocal: function(L: Plua_State; const ar: Plua_Debug;
  n: integer): PChar; cdecl;
  lua_setlocal: function(L: Plua_State; const ar: Plua_Debug;
  n: integer): PChar; cdecl;
  lua_getupvalue: function(L: Plua_State; funcindex: integer;
  n: integer): PChar; cdecl;
  lua_setupvalue: function(L: Plua_State; funcindex: integer;
  n: integer): PChar; cdecl;

  lua_sethook: function(L: Plua_State; func: lua_Hook; mask: integer;
  Count: integer): integer; cdecl;
  //lua_gethook     : function  (L: Plua_State): lua_Hook; cdecl;
  lua_gethookmask: function(L: Plua_State): integer; cdecl;
  lua_gethookcount: function(L: Plua_State): integer; cdecl;

function LoadLua(Path: WideString = LUA_LIB_NAME): boolean;
procedure FreeLua;
function LuaLoaded(): boolean;

implementation

uses
  Windows;

function lua_upvalueindex(I: integer): integer;
begin
  Result := LUA_GLOBALSINDEX - i;
end;

procedure lua_pop(L: Plua_State; n: integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable(L: Plua_State);
begin
  lua_createtable(L, 0, 0);
end;

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, n);
end;

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

function lua_strlen(L: Plua_State; i: integer): size_t;
begin
  Result := lua_objlen(L, i);
end;

function lua_isfunction(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) <= 0;
end;

procedure lua_pushliteral(L: Plua_State; s: PChar);
begin
  lua_pushlstring(L, s, Length(s));
end;

procedure lua_setglobal(L: Plua_State; const s: PChar);
begin
  lua_setfield(L, LUA_GLOBALSINDEX, s);
end;

procedure lua_getglobal(L: Plua_State; const s: PChar);
begin
  lua_getfield(L, LUA_GLOBALSINDEX, s);
end;

function lua_tostring(L: Plua_State; i: integer): PChar;
begin
  Result := lua_tolstring(L, i, nil);
end;


procedure lua_getregistry(L: Plua_State);
begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

function lua_getgccount(L: Plua_State): integer;
begin
  Result := lua_gc(L, LUA_GCCOUNT, 0);
end;

var
  h: THandle;

function LoadLua(Path: WideString): boolean;
begin
  h := LoadLibraryW(PWideChar(Path));
  if h = 0 then
  begin
    Result := False;
    Exit;
  end;

  Pointer(lua_newstate) := GetProcAddress(h, 'lua_newstate');
  Pointer(lua_close) := GetProcAddress(h, 'lua_close');
  Pointer(lua_newthread) := GetProcAddress(h, 'lua_newthread');
  Pointer(lua_atpanic) := GetProcAddress(h, 'lua_atpanic');

  Pointer(lua_gettop) := GetProcAddress(h, 'lua_gettop');
  Pointer(lua_settop) := GetProcAddress(h, 'lua_settop');
  Pointer(lua_pushvalue) := GetProcAddress(h, 'lua_pushvalue');
  Pointer(lua_remove) := GetProcAddress(h, 'lua_remove');
  Pointer(lua_insert) := GetProcAddress(h, 'lua_insert');
  Pointer(lua_replace) := GetProcAddress(h, 'lua_replace');
  Pointer(lua_checkstack) := GetProcAddress(h, 'lua_checkstack');

  Pointer(lua_xmove) := GetProcAddress(h, 'lua_xmove');

  Pointer(lua_isnumber) := GetProcAddress(h, 'lua_isnumber');
  Pointer(lua_isstring) := GetProcAddress(h, 'lua_isstring');
  Pointer(lua_iscfunction) := GetProcAddress(h, 'lua_iscfunction');
  Pointer(lua_isuserdata) := GetProcAddress(h, 'lua_isuserdata');
  Pointer(lua_type) := GetProcAddress(h, 'lua_type');
  Pointer(lua_typename) := GetProcAddress(h, 'lua_typename');

  Pointer(lua_equal) := GetProcAddress(h, 'lua_equal');
  Pointer(lua_rawequal) := GetProcAddress(h, 'lua_rawequal');
  Pointer(lua_lessthan) := GetProcAddress(h, 'lua_lessthan');

  Pointer(lua_tonumber) := GetProcAddress(h, 'lua_tonumber');
  Pointer(lua_tointeger) := GetProcAddress(h, 'lua_tointeger');
  Pointer(lua_toboolean) := GetProcAddress(h, 'lua_toboolean');
  Pointer(lua_tolstring) := GetProcAddress(h, 'lua_tolstring');
  Pointer(lua_objlen) := GetProcAddress(h, 'lua_objlen');
  Pointer(lua_tocfunction) := GetProcAddress(h, 'lua_tocfunction');
  Pointer(lua_touserdata) := GetProcAddress(h, 'lua_touserdata');
  Pointer(lua_tothread) := GetProcAddress(h, 'lua_tothread');
  Pointer(lua_topointer) := GetProcAddress(h, 'lua_topointer');

  Pointer(lua_pushnil) := GetProcAddress(h, 'lua_pushnil');
  Pointer(lua_pushnumber) := GetProcAddress(h, 'lua_pushnumber');
  Pointer(lua_pushinteger) := GetProcAddress(h, 'lua_pushinteger');
  Pointer(lua_pushlstring) := GetProcAddress(h, 'lua_pushlstring');
  Pointer(lua_pushstring) := GetProcAddress(h, 'lua_pushstring');
  Pointer(lua_pushvfstring) := GetProcAddress(h, 'lua_pushvfstring');
  Pointer(lua_pushfstring) := GetProcAddress(h, 'lua_pushfstring');
  Pointer(lua_pushcclosure) := GetProcAddress(h, 'lua_pushcclosure');
  Pointer(lua_pushboolean) := GetProcAddress(h, 'lua_pushboolean');
  Pointer(lua_pushlightuserdata) := GetProcAddress(h, 'lua_pushlightuserdata');
  Pointer(lua_pushthread) := GetProcAddress(h, 'lua_pushthread');

  Pointer(lua_gettable) := GetProcAddress(h, 'lua_gettable');
  Pointer(lua_getfield) := GetProcAddress(h, 'lua_getfield');
  Pointer(lua_rawget) := GetProcAddress(h, 'lua_rawget');
  Pointer(lua_rawgeti) := GetProcAddress(h, 'lua_rawgeti');
  Pointer(lua_createtable) := GetProcAddress(h, 'lua_createtable');
  Pointer(lua_newuserdata) := GetProcAddress(h, 'lua_newuserdata');
  Pointer(lua_getmetatable) := GetProcAddress(h, 'lua_getmetatable');
  Pointer(lua_getfenv) := GetProcAddress(h, 'lua_getfenv');

  Pointer(lua_settable) := GetProcAddress(h, 'lua_settable');
  Pointer(lua_setfield) := GetProcAddress(h, 'lua_setfield');
  Pointer(lua_rawset) := GetProcAddress(h, 'lua_rawset');
  Pointer(lua_rawseti) := GetProcAddress(h, 'lua_rawseti');
  Pointer(lua_setmetatable) := GetProcAddress(h, 'lua_setmetatable');
  Pointer(lua_setfenv) := GetProcAddress(h, 'lua_setfenv');

  Pointer(lua_call) := GetProcAddress(h, 'lua_call');
  Pointer(lua_pcall) := GetProcAddress(h, 'lua_pcall');
  Pointer(lua_cpcall) := GetProcAddress(h, 'lua_cpcall');
  Pointer(lua_load) := GetProcAddress(h, 'lua_load');

  Pointer(lua_dump) := GetProcAddress(h, 'lua_dump');

  Pointer(lua_yield) := GetProcAddress(h, 'lua_yield');
  Pointer(lua_resume) := GetProcAddress(h, 'lua_resume');
  Pointer(lua_status) := GetProcAddress(h, 'lua_status');

  Pointer(lua_gc) := GetProcAddress(h, 'lua_gc');
  Pointer(lua_error) := GetProcAddress(h, 'lua_error');

  Pointer(lua_next) := GetProcAddress(h, 'lua_next');

  Pointer(lua_concat) := GetProcAddress(h, 'lua_concat');

  Pointer(lua_getallocf) := GetProcAddress(h, 'lua_getallocf');
  Pointer(lua_setallocf) := GetProcAddress(h, 'lua_setallocf');

  Pointer(lua_getstack) := GetProcAddress(h, 'lua_getstack');
  Pointer(lua_getinfo) := GetProcAddress(h, 'lua_getinfo');
  Pointer(lua_getlocal) := GetProcAddress(h, 'lua_getlocal');
  Pointer(lua_setlocal) := GetProcAddress(h, 'lua_setlocal');
  Pointer(lua_getupvalue) := GetProcAddress(h, 'lua_getupvalue');
  Pointer(lua_setupvalue) := GetProcAddress(h, 'lua_setupvalue');

  Pointer(lua_sethook) := GetProcAddress(h, 'lua_sethook');
  //Pointer(lua_gethook) := GetProcAddress(h, 'lua_gethook');
  Pointer(lua_gethookmask) := GetProcAddress(h, 'lua_gethookmask');
  Pointer(lua_gethookcount) := GetProcAddress(h, 'lua_gethookcount');
  Result := True;
end;

procedure FreeLua;
begin
  if h <> 0 then
    FreeLibrary(h);
  h := 0;

  lua_newstate := nil;
  lua_close := nil;
  lua_newthread := nil;
  lua_atpanic := nil;

  lua_gettop := nil;
  lua_settop := nil;
  lua_pushvalue := nil;
  lua_remove := nil;
  lua_insert := nil;
  lua_replace := nil;
  lua_checkstack := nil;

  lua_xmove := nil;

  lua_isnumber := nil;
  lua_isstring := nil;
  lua_iscfunction := nil;
  lua_isuserdata := nil;
  lua_type := nil;
  lua_typename := nil;

  lua_equal := nil;
  lua_rawequal := nil;
  lua_lessthan := nil;

  lua_tonumber := nil;
  lua_tointeger := nil;
  lua_toboolean := nil;
  lua_tolstring := nil;
  lua_objlen := nil;
  lua_tocfunction := nil;
  lua_touserdata := nil;
  lua_tothread := nil;
  lua_topointer := nil;

  lua_pushnil := nil;
  lua_pushnumber := nil;
  lua_pushinteger := nil;
  lua_pushlstring := nil;
  lua_pushstring := nil;
  lua_pushvfstring := nil;
  lua_pushfstring := nil;
  lua_pushcclosure := nil;
  lua_pushboolean := nil;
  lua_pushlightuserdata := nil;
  lua_pushthread := nil;

  lua_gettable := nil;
  lua_getfield := nil;
  lua_rawget := nil;
  lua_rawgeti := nil;
  lua_createtable := nil;
  lua_newuserdata := nil;
  lua_getmetatable := nil;
  lua_getfenv := nil;

  lua_settable := nil;
  lua_setfield := nil;
  lua_rawset := nil;
  lua_rawseti := nil;
  lua_setmetatable := nil;
  lua_setfenv := nil;

  lua_call := nil;
  lua_pcall := nil;
  lua_cpcall := nil;
  lua_load := nil;

  lua_dump := nil;

  lua_yield := nil;
  lua_resume := nil;
  lua_status := nil;

  lua_gc := nil;
  lua_error := nil;

  lua_next := nil;

  lua_concat := nil;

  lua_getallocf := nil;
  lua_setallocf := nil;

  lua_getstack := nil;
  lua_getinfo := nil;
  lua_getlocal := nil;
  lua_setlocal := nil;
  lua_getupvalue := nil;
  lua_setupvalue := nil;

  lua_sethook := nil;
  //lua_gethook := nil;
  lua_gethookmask := nil;
  lua_gethookcount := nil;
end;

function LuaLoaded(): boolean;
begin
  Result := h <> 0;
end;

initialization
  h := 0;

(******************************************************************************
* Copyright (C) 1994-2003 Tecgraf, PUC-Rio.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
end.
