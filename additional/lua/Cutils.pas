unit Cutils;

(*
 * A library of C operator replacements for Pascal code.
 *
 * Created by Geo Massar, 2006
 * Distributed as free/open source.
 *)

interface

function AssignInt(var v : Integer; e : Integer) : Integer;
function WhichStr(cond : Boolean; s1, s2 : PChar) : PChar;
function WhichInt(cond : Boolean; i1, i2 : Integer) : Integer;
function PostInc(var i : Integer) : Integer;
function PreInc(var i : Integer) : Integer;

implementation

function AssignInt(var v : Integer; e : Integer) : Integer;
begin
  v := e;
  result := v;
end;

function WhichStr(cond : Boolean; s1, s2 : PChar) : PChar;
begin
  if cond then result := s1 else result := s2;
end;

function WhichInt(cond : Boolean; i1, i2 : Integer) : Integer;
begin
  if cond then result := i1 else result := i2;
end;

// Returns i++ in C
function PostInc(var i : Integer) : Integer;
begin
  result := i;
  Inc(i);
end;

// Returns ++i in C
function PreInc(var i : Integer) : Integer;
begin
  Inc(i);
  result := i;
end;

end.
