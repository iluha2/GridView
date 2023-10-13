{
  TGridView component (grid)
  (C) Roman M. Mochalov, 1997-2019
  (C) Iluha Companets  , 2002-2023
  License: MIT
}

unit Ex_Utils;

interface

uses
  LazUTF8, LCLType, LCLIntf, Types, Classes;

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

{$IFNDEF WINDOWS}
function MapWindowPoints(hWndFrom, hWndTo: HWND; Points: Pointer; cPoints: UINT): Integer;
procedure PolyPolyline(DC: HDC; const Points: array of TPoint; const PNums: array of DWORD);
{$ENDIF}

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
  up_s: String;
begin
  Result:= -1;
  up_s:= UTF8Uppercase(s);
  for i := 0 to ss.Count-1 do
  begin
    if fsIgnoreCase in par then
      p:= UTF8Pos(up_s, UTF8Uppercase(ss[i]))
    else
      p:= UTF8Pos(s, ss[i]);
    if (    (fsFromBegin in par) and (p = 1)) or
       (not (fsFromBegin in par) and (p > 0)) then
      Exit( i );
  end;
end;

{$IFNDEF WINDOWS}

function MapWindowPoints(hWndFrom, hWndTo: HWND; Points: Pointer; cPoints: UINT): Integer;
var
  PtsArr: PPoint absolute Points;
  i: Integer;
  XOffset, YOffset: SmallInt;
  FromPoint, ToPoint: TPoint;
begin
  FromPoint := Point(0, 0);
  ToPoint   := Point(0, 0);
  if hWndFrom <> 0 then ClientToScreen(hWndFrom, FromPoint);
  if hWndTo   <> 0 then ClientToScreen(hWndTo,   ToPoint);
  XOffset := (FromPoint.X - ToPoint.X);
  YOffset := (FromPoint.Y - ToPoint.Y);
  for i := 0 to cPoints - 1 do
  begin
    PtsArr[i].x := XOffset + PtsArr[i].x;
    PtsArr[i].y := YOffset + PtsArr[i].y;
  end;
  Result := MakeLong(XOffset, YOffset);
end;

procedure PolyPolyline(DC: HDC; const Points: array of TPoint; const PNums: array of DWORD);
var
  len,i,j,p: DWORD;
begin
  len:= Length(Points);
  p:= 0;
  for i:= 0 to Length(PNums)-1 do
  begin
    j:= PNums[i];
    if p + j > len then
      Break;
    Polyline(DC, @Points[p], j);
    Inc(p, j);
  end;
end;

{$ENDIF}

end.
