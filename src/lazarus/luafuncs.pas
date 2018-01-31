unit LuaFuncs;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Lua;

function luaopen_Extram(L: Plua_State): integer; cdecl;

implementation

uses
  SysUtils, ExtramMain;

type
  ShiftJISString = type ansistring(932);

var
  Extram: TExtram;

function LuaReturn(L: Plua_State; const Ret: integer): integer;
begin
  if Ret = -1 then
    // *** IMPORTANT ***
    // Both Lua and FreePascal are using setjmp/longjmp for error/exception handling.
    // So while using lua_error(luaL_error), we cannot use try statement, any string type, etc.
    // If we do not follow this rule the program crashes in random places.
    Result := luaL_error(L, '%s', lua_tostring(L, -1))
  else
    Result := Ret;
end;

function LuaPushError(L: Plua_State; E: Exception): integer;
var
  SJIS: ShiftJISString;
begin
  SJIS := ShiftJISString(E.Message);
  lua_pushlstring(L, @SJIS[1], Length(SJIS));
  Result := -1;
end;

function LuaPut(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    Key: PChar;
    Data: Pointer;
    SrcLen: integer;
    Size: size_t;
  begin
    try
      if not Assigned(Extram.API) then
        raise Exception.Create('Extram is not ready yet');

      Key := lua_tostring(L, 1);
      if lua_isuserdata(L, 2) then
      begin
        if (lua_gettop(L) < 3) or (not lua_isnumber(L, 3)) then
          raise Exception.Create(
            'you must pass length if you put an userdata. ex) Extram.put(key, userdata, length)');
        Data := lua_touserdata(L, 2);
        SrcLen := lua_tointeger(L, 3);
      end
      else if lua_isstring(L, 2) then
      begin
        Data := lua_tolstring(L, 2, @Size);
        SrcLen := Size and $7fffffff;
      end
      else
        raise Exception.Create('unsupprted type: ' + lua_typename(L, lua_type(L, 2)));

      if SrcLen > Extram.API^.ViewLen then
        raise Exception.Create('data is too large');
      Move(Data^, Extram.API^.View^, SrcLen);
      if Extram.API^.Put(Key, SrcLen) = 0 then
        Exception.Create('failed to put');
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaDel(L: Plua_State): integer; cdecl;

  function Main(): integer;
  begin
    try
      if not Assigned(Extram.API) then
        raise Exception.Create('Extram is not ready yet');
      Extram.API^.Del(lua_tostring(L, 1));
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaGet(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    Key: PChar;
    Data: Pointer;
    DestLen, SrcLen: integer;
  begin
    try
      if not Assigned(Extram.API) then
        raise Exception.Create('Extram is not ready yet');

      Key := lua_tostring(L, 1);
      if (lua_gettop(L) > 2) and lua_isuserdata(L, 2) then
      begin
        if (lua_gettop(L) < 3) or (not lua_isnumber(L, 3)) then
          raise Exception.Create(
            'you must pass length if you write to the userdata. ex) Extram.get(key, userdata, length)');
        Data := lua_topointer(L, 2);
        DestLen := lua_tointeger(L, 3);
        SrcLen := Extram.API^.Get(Key);
        if SrcLen <= 0 then
          raise Exception.Create('data not found');
        if DestLen < SrcLen then
          raise Exception.Create('buffer is too small');
        Move(Extram.API^.View^, Data^, SrcLen);
        lua_pushinteger(L, SrcLen);
        Result := 1;
      end
      else begin
        SrcLen := Extram.API^.Get(Key);
        if SrcLen <= 0 then
          raise Exception.Create('data not found');
        lua_pushlstring(L, Extram.API^.View, SrcLen);
        Result := 1;
      end;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function luaopen_Extram(L: Plua_State): integer; cdecl;
type
  TEntry = record
    Name: PChar;
    Func: lua_CFunction;
  end;
const
  Functions: array[0..2] of TEntry = (
    (Name: 'put'; Func: @LuaPut),
    (Name: 'del'; Func: @LuaDel),
    (Name: 'get'; Func: @LuaGet));
var
  i: integer;
begin
  lua_newtable(L);
  for i := Low(Functions) to High(Functions) do
  begin
    lua_pushcfunction(L, Functions[i].Func);
    lua_setfield(L, 2, Functions[i].Name);
  end;

  lua_newtable(L);
  lua_setfield(L, 2, 'meta');

  lua_pushvalue(L, 2);
  lua_setglobal(L, 'Extram');
  Result := 1;
end;

initialization
  Extram := TExtram.Create();

finalization
  Extram.Free();

end.
