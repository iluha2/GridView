unit Ex_GridUtils; // GridView utilities

interface

uses
  LazUTF8, LazUtilities, LCLIntf, SysUtils, Classes, Graphics, Registry,
  Ex_Grid, Ex_Utils;

function GV_IsCursorIn(Grid: TGridView): boolean;
procedure GV_DrawStandartCell(Grid: TGridView; Cell: TGridCell; Canvas: TCanvas);
procedure GV_DrawNoFocusCell(Grid: TGridView; Cell: TGridCell; Canvas: TCanvas);
procedure GV_RegStorColumns(const Path: string; Grid: TGridView);
procedure GV_RegRetrColumns(const Path: string; Grid: TGridView);
{$IFDEF WINDOWS}
function GV_ExportToExcel(GV: TCustomGridView; const Caption: String): Boolean; // NOTE !! not working !!
{$ENDIF}
procedure GV_ExportToStrings(GV: TCustomGridView; const Caption: String; Ss: TStrings);


implementation

{$IFDEF WINDOWS}
uses
  Forms, Controls, ComObj, Variants, DB, Ex_DBGrid;
{$ENDIF}

function GV_IsCursorIn(Grid: TGridView): boolean;
var
  Pt: TPoint;
begin
  Pt:= Default(TPoint);
  GetCursorPos(Pt);
  Pt := Grid.ScreenToClient(Pt);
  Result := (Pt.Y > Grid.Header.Height) and
    (Grid.GetColumnAt(Pt.X, Pt.Y) <> -1) and (Grid.GetRowAt(Pt.X, Pt.Y) <> -1);
end;

procedure GV_DrawStandartCell(Grid: TGridView; Cell: TGridCell; Canvas: TCanvas);
begin
  Canvas.Font.Assign(Grid.Font);
  if Cell.Col < Grid.Fixed.Count then
  begin
    Canvas.Brush.Color := Grid.Fixed.Color; // clBTNFACE
    // Canvas.Font.Color := clWINDOWTEXT;
  end
  else if Cell.Row = Grid.Row then
  begin
    Canvas.Brush.Color := clHIGHLIGHT;
    Canvas.Font.Color := clHIGHLIGHTTEXT;
  end
  else
  begin
    Canvas.Brush.Color := Grid.Color; // clWINDOW
    // Canvas.Font.Color := clWINDOWTEXT;
  end;
end;

procedure GV_DrawNoFocusCell(Grid: TGridView; Cell: TGridCell; Canvas: TCanvas);
begin
  Canvas.Font.Assign(Grid.Font);
  if Cell.Col < Grid.Fixed.Count then
  begin
    Canvas.Brush.Color := Grid.Fixed.Color; // clBTNFACE
    // Canvas.Font.Color := clWINDOWTEXT;
  end
  else
  begin
    Canvas.Brush.Color := Grid.Color; // clWINDOW
    // Canvas.Font.Color := clWINDOWTEXT;
  end;
end;

procedure GV_RegStorColumns(const Path: string; Grid: TGridView);
var
  Reg: TRegistry;
  i: Integer;
begin
  Reg := TRegistry.Create;
  TRY
    if Reg.OpenKey(Path, True) then
    begin
      for i:= 0 to Grid.Columns.Count-1 do
        Reg.WriteInteger('Col'+ IntToStr(i), Grid.Columns[i].Width);
      Reg.CloseKey;
    end;
  EXCEPT
  END;
  FreeThenNil(Reg);
end;

procedure GV_RegRetrColumns(const Path: string; Grid: TGridView);
var
  Reg: TRegistry;
  i: Integer;
begin
  Reg := TRegistry.Create;
  TRY
    if Reg.OpenKey(Path, False) then
    begin
      for i:= 0 to Grid.Columns.Count-1 do
        Grid.Columns[i].Width:= Reg.ReadInteger('Col'+ IntToStr(i));
      Reg.CloseKey;
    end
    else
      Grid.SizeAllColumnsToFit;
  EXCEPT
  END;
  FreeThenNil(Reg);
end;

{$IFDEF WINDOWS}

function GV_ExportToExcel(GV: TCustomGridView; const Caption: String): Boolean;
const
  xlHAlignCenter = $FFFFEFF4;
  xlVAlignCenter = $FFFFEFF4;
var
  XL,                 // Приложение Excel
  TableVals: Variant; // Врем. массив для переноса значений в Excel
  DS: TDataSet;
  i,j,cols,rows: Integer;
  Str1: String;

  procedure ApplyHeaderSections(GHSections: TGridHeaderSections);
  var
    s: Integer;
    GHS: TGridHeaderSection;
    MergeRect: TRect;
    MergeRange: Variant;
  begin
    if GHSections.Count = 0 then
      Exit;
    for s:= 0 to GHSections.Count-1 do
    begin
      GHS:= GHSections[s];
      MergeRect.TopLeft:= Point(GHS.FirstColumnIndex+1, GHS.Level+2);
      MergeRect.BottomRight:= Point(GHS.ColumnIndex+1, GHS.Level+2);
      if GHS.Sections.Count = 0 then
        Inc(MergeRect.Bottom, GV.Header.MaxLevel{GHSections.MaxLevel} - GHS.Level);
      XL.Cells[MergeRect.Top, MergeRect.Left].Value:= GHS.DisplayText;
      if (MergeRect.Left <> MergeRect.Right) or (MergeRect.Top <> MergeRect.Bottom) then
        XL.Range[
          XL.Cells[MergeRect.Top, MergeRect.Left],
          XL.Cells[MergeRect.Bottom, MergeRect.Right]].Merge;
      ApplyHeaderSections(GHS.Sections);
    end;
    MergeRange:= XL.Range[XL.Cells[2, 1],
                          XL.Cells[GV.Header.MaxLevel{GHSections.MaxLevel}+2,
                                   GHSections[GHSections.Count-1].ColumnIndex+1]];
    MergeRange.HorizontalAlignment:= xlHAlignCenter;
    MergeRange.VerticalAlignment:= xlVAlignCenter;
  end;

begin
  Screen.Cursor:= crSQLWait;
  Result:= True;
  TRY
    TRY
      XL:= GetActiveOleObject('Excel.Application');
    EXCEPT
      XL:= CreateOleObject('Excel.Application');
    END;
  EXCEPT
    Result:= False;
    Screen.Cursor:= crDefault;
    Exit;
  END;

  cols:= GV.Columns.Count;
  rows:= GV.Rows.Count;

  XL.Visible:= True;
  XL.Workbooks.Add;
//  XL.ActiveWindow.DisplayZeros:= False;

  XL.Cells[1, 1].Value:= Caption;
  XL.Range[XL.Cells[1, 1], XL.Cells[1, cols]].Merge;

  ApplyHeaderSections(GV.Header.Sections);

  if GV is TCustomDBGridView then
  begin
    DS:= TCustomDBGridView(GV).DataSource.DataSet;
    rows:= DS.RecordCount;
  end
  else
    DS:= nil;

  if rows > 0 then
  begin
    TableVals:= VarArrayCreate([1, rows, 1, cols], varVariant);
    for j:= 1 to rows do
    begin
      if GV is TCustomDBGridView then
      begin
        DS.RecNo:= j;
        for i:= 1 to cols do
        begin
          Str1:= DS.Fields[i-1].DisplayText;
          if Str1 = '(BYTES)' then
            Str1:= DS.Fields[i-1].AsString // dsStr2HexStrWithDiv(DS.Fields[i-1].AsString, ' ')
          else if Str1 = '(MEMO)' then
            Str1:= DS.Fields[i-1].AsString;
          TableVals[j, i]:= Str1;
        end;
      end
      else
        for i:= 1 to cols do
          TableVals[j, i]:= GV.Cells[i-1, j-1];
    end;
    XL.Range[XL.Cells[GV.Header.MaxLevel+3,1],
             XL.Cells[GV.Header.MaxLevel+rows+2, cols]].Value:= TableVals;
  end;

  for i:= 1 to cols do
  begin
    XL.Columns[i].AutoFit;
    if not GV.Columns[i-1].Visible then XL.Columns[i].Hidden:= True;
  end;

  XL.Range[XL.Cells[1,1],
           XL.Cells[GV.Header.MaxLevel+rows+2, cols]].Borders.Weight:= 2;

  Screen.Cursor:= crDefault;
end;

{$ENDIF}

procedure GV_ExportToStrings(GV: TCustomGridView; const Caption: String; Ss: TStrings);
var
  Str1, Str2, OutStr: String;
  i,j,w: Integer;
  EmptyString: Boolean;
  Levels, Columns: TList;
  ColSymbWidths: array of Integer = [];

  procedure ApplyHeaderSections(GHSections: TGridHeaderSections);
  var
    s, l: Integer;
    GHS: TGridHeaderSection;
  begin
    for s:= 0 to GHSections.Count-1 do
    begin
      GHS:= GHSections[s];
      if s > 0 then
        for l:= 0 to GHS.Level-1 do TStringList(Levels[l]).Add('~');
      TStringList(Levels[GHS.Level]).Add(GHS.DisplayText);
      if GHS.Sections.Count = 0 then
      begin
        for l:= GHS.Level+1 to Levels.Count-1 do TStringList(Levels[l]).Add('');
      end
      else
        ApplyHeaderSections(GHS.Sections);
    end;
  end;

begin
  Ss.Clear;
  Ss.Add(Caption);
  Ss.Add(#32);

  Levels:= TList.Create;
  Columns:= TList.Create;

  for i:= 0 to GV.Header.MaxLevel do Levels.Add(TStringList.Create);

  ApplyHeaderSections(GV.Header.Sections);

  for i:= 0 to GV.Columns.Count-1 do
  begin
    Columns.Add(TStringList.Create);
    SetLength(ColSymbWidths, i+1);
    ColSymbWidths[i]:= 0;
    for j:= 0 to Levels.Count-1 do
    begin
      Str1:= TStringList(Levels[j])[i];
      TStringList(Columns[i]).Add(Str1);
//      if ((i = GV.Columns.Count-1) or (TStringList(Levels[j])[i+1] <> '~')) and
      if ((i = TStringList(Levels[j]).Count-1) or (TStringList(Levels[j])[i+1] <> '~')) and
         (ColSymbWidths[i] < UTF8Length(Str1)) then
          ColSymbWidths[i]:= UTF8Length(Str1);
    end;
    for j:= 0 to GV.Rows.Count-1 do
    begin
      Str1:= GV.Cells[i, j];
      TStringList(Columns[i]).Add(Str1);
      if ColSymbWidths[i] < UTF8Length(Str1) then
         ColSymbWidths[i]:= UTF8Length(Str1);
    end;
  end;

  OutStr:= '';
  for i:= 0 to Levels.Count-1 do
  begin
    OutStr:= '|';
    Str2:= '';
    w:= 0;
    for j:= 0 to Columns.Count-1 do
      if GV.Columns[j].Visible then
      begin
        Str1:= TStringList(Levels[i])[j];
        if Str1 = '~' then
          Inc(w, ColSymbWidths[j]+1)
        else
        begin
          if w > 0 then OutStr:= OutStr+ UTF8PadCenter(Str2, w)+ '|';
          w:= ColSymbWidths[j];
          Str2:= Str1; // сохраняем заголовок
        end;
      end;
    if w > 0 then OutStr:= OutStr+ UTF8PadCenter(Str2, w)+ '|';
    Ss.Add(OutStr);
  end;
  if OutStr <> '' then Ss.Add(StringOfChar('-', UTF8Length(OutStr)));

  for i:= 0 to GV.Rows.Count-1 do
  begin
    EmptyString:= True;
    OutStr:= '|';
    for j:= 0 to Columns.Count-1 do
    if GV.Columns[j].Visible then
    begin
      w:= ColSymbWidths[j];
      Str1:= TStringList(Columns[j])[Levels.Count+i];
      EmptyString:= EmptyString and (Str1 = '');
      case GV.Columns[j].Alignment of
        taRightJustify: Str1:= UTF8PadLeft(Str1, w);
        taCenter: Str1:= UTF8PadCenter(Str1, w);
      else // taLeftJustify
        Str1:= UTF8PadRight(Str1, w)
      end;
      OutStr:= OutStr+ Str1+ '|';
    end;
    if not EmptyString then Ss.Add(OutStr);
  end;

  for i:= 0 to Levels.Count-1 do TObject(Levels[i]).Free;
  for i:= 0 to Columns.Count-1 do TObject(Columns[i]).Free;
  FreeThenNil(Levels);
  FreeThenNil(Columns);
end;

end.
