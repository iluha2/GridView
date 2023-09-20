{
  TGridView component (grid)
  (C) Roman M. Mochalov, 1997-2019
  (C) Iluha Companets  , 2002-2023
  License: MIT
}

unit Ex_Utils;

interface

uses
  LazUTF8, Classes;

type
  TGridCell = record
    Col: Longint;
    Row: Longint;
  end;

  TFindStrParams = set of (fsIgnoreCase, fsFromBegin);

function GridCell(Col, Row: Longint): TGridCell;
function IsCellEqual(Cell1, Cell2: TGridCell): Boolean;
function IsCellEmpty(Cell: TGridCell): Boolean;
function OffsetCell(Cell: TGridCell; C, R: Longint): TGridCell;

function FindInStrings(const s: string; ss: TStrings; const par: TFindStrParams = []): Integer;

implementation

function GridCell(Col, Row: Longint): TGridCell;
begin
  Result.Col := Col;
  Result.Row := Row;
end;

function IsCellEqual(Cell1, Cell2: TGridCell): Boolean;
begin
  Result := (Cell1.Col = Cell2.Col) and (Cell1.Row = Cell2.Row);
end;

function IsCellEmpty(Cell: TGridCell): Boolean;
begin
  Result := (Cell.Col = -1) or (Cell.Row = -1);
end;

function OffsetCell(Cell: TGridCell; C, R: Longint): TGridCell;
begin
  Result.Col := Cell.Col + C;
  Result.Row := Cell.Row + R;
end;

function FindInStrings(const s: string; ss: TStrings; const par: TFindStrParams = []): Integer;
var
  i,p: Integer;
begin
  Result:= -1;
  for i := 0 to ss.Count-1 do
  begin
    if fsIgnoreCase in par then
      p:= UTF8Pos(UTF8Uppercase(s), UTF8Uppercase(ss[i]))
    else
      p:= UTF8Pos(s, ss[i]);
    if (    (fsFromBegin in par) and (p = 1)) or
       (not (fsFromBegin in par) and (p > 0)) then
      Exit( i );
  end;
end;

end.
