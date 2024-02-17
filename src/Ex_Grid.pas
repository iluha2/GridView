{
  TGridView component (grid)
  (C) Roman M. Mochalov, 1997-2019
  (C) Iluha Companets  , 2002-2023
  License: MIT
}

unit Ex_Grid;

{$mode delphi}{$H+}

interface

{$I Ex_Grid.inc}

implementation

{ TGridHeaderSection }

procedure TGridHeaderSection.DefineProperties(Filer: TFiler);
begin
  inherited;
  { backward compatibility }
  Filer.DefineProperty('Width', ReadWidth, nil, False);
end;

destructor TGridHeaderSection.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSections);
end;

function TGridHeaderSection.IsSectionsStored: Boolean;
begin
  Result := (FSections <> nil) and (FSections.Count > 0);
end;

procedure TGridHeaderSection.ReadWidth(Reader: TReader);
begin
  Reader.ReadInteger;
end;

function TGridHeaderSection.GetAllowClick: Boolean;
var
  I: Integer;
begin
  Result := False;
  { можно ли щелкать на колонке }
  { column AllowClick property can disable header click }
  if (Header <> nil) and (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
      Result := Header.Grid.Columns[I].AllowClick;
  end;
end;

function TGridHeaderSection.GetBoundsRect: TRect;
var
  R: TRect;
begin
  { нет заголовка - нет размеров }
  if (Header = nil) or (Header.Grid = nil) then
    Exit( Classes.Rect(0, 0, 0, 0) );
  { get bounds relative to the upper-left corner of the header }
  Result := FBoundsRect;
  R := Header.Grid.GetHeaderRect;
  OffsetRect(Result, R.Left, R.Top);
  { если это не фиксированный заголовок - смещаем его на величину сдвига
    таблицы скроллером }
  if not FixedColumn then
    OffsetRect(Result, Header.Grid.GetGridOrigin.X, 0);
end;

function TGridHeaderSection.GetDisplayName: string;
begin
  Result := DisplayText;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TGridHeaderSection.GetDisplayText: string;
var
  I: Integer;
begin
  Result := Caption;
  if (Result = '') and (not IsSectionsStored) and (Header <> nil) and
    (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
      Result := Header.Grid.Columns[I].Caption;
  end;
end;

function TGridHeaderSection.GetFirstColumnIndex: Integer;
begin
  if Sections.Count > 0 then
    Result := Sections[0].FirstColumnIndex
  else
    Result := ColumnIndex;
end;

function TGridHeaderSection.GetFixedColumn: Boolean;
begin
  if Sections.Count > 0 then
    Result := Sections[0].FixedColumn
  else if (Header = nil) or (Header.Grid = nil) then
    Result := False
  else
    Result := ColumnIndex < Header.Grid.Fixed.Count;
end;

function TGridHeaderSection.GetHeader: TCustomGridHeader;
begin
  if ParentSections <> nil then
    Result := ParentSections.Header
  else
    Result := nil;
end;

function TGridHeaderSection.GetLevel: Integer;
begin
  if Parent <> nil then
    Result := Parent.Level + 1
  else
    Result := 0
end;

function TGridHeaderSection.GetParent: TGridHeaderSection;
begin
  if ParentSections <> nil then
    Result := ParentSections.OwnerSection
  else
    Result := nil;
end;

function TGridHeaderSection.GetParentSections: TGridHeaderSections;
begin
  if Collection <> nil then
    Result := TGridHeaderSections(Collection)
  else
    Result := nil;
end;

function TGridHeaderSection.GetSections: TGridHeaderSections;
begin
  if FSections = nil then
    FSections := TGridHeaderSections.Create(Header, Self);
  Result := FSections;
end;

function TGridHeaderSection.GetResizeColumnIndex: Integer;
var
  I: Integer;
begin
  { если есть подзаголовки возвращаем колонку последнего из них }
  { the resize column is the column of the rightmost subsection }
  for I := Sections.Count-1 downto 0 do
    if Sections[I].Visible then
      Exit( Sections[I].ResizeColumnIndex );
  { возвращаем расчитанный индекс колонки }
  Result := FColumnIndex;
end;

function TGridHeaderSection.GetVisible: Boolean;
var
  I: Integer;
begin
  { если есть подзаголовки, то смотрим видимость их }
  if Sections.Count > 0 then
    for I := 0 to Sections.Count-1 do
      if Sections[I].Visible then
        Exit( True );
  { иначе смотрим видимость колонки }
  { section visibility is the visibility of the corresponding column }
  if (Header <> nil) and (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
      Exit( Header.Grid.Columns[I].Visible );
  end;
  { нет колонки - секция видна }
  { sections without columns (nonsense) are always visible }
  Result := True;
end;

function TGridHeaderSection.GetWidth: Integer;
var
  I: Integer;
  S: TGridHeaderSection;
begin
  { если есть подзаголовки, то ширина есть сумма ширин подзаголовков }
  if Sections.Count > 0 then
  begin
    Result := 0;
    for I := 0 to Sections.Count-1 do
    begin
      S := Sections[I];
      Result := Result + S.Width;
    end;
    Exit;
  end;
  { иначе возвращаем ширину соответствующей колонки }
  { section with is the with of the corresponding column }
  if (Header <> nil) and (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
      Exit( Header.Grid.Columns[I].Width );
  end;
  { sections without columns (nonsense) do not have width }
  Result := 0;
end;

procedure TGridHeaderSection.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.SetSections(Value: TGridHeaderSections);
begin
  Sections.Assign(Value);
end;

procedure TGridHeaderSection.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.Assign(Source: TPersistent);
begin
  if Source is TGridHeaderSection then
  begin
    Sections  := TGridHeaderSection(Source).Sections;
    Caption   := TGridHeaderSection(Source).Caption;
    Alignment := TGridHeaderSection(Source).Alignment;
    WordWrap  := TGridHeaderSection(Source).WordWrap;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TGridHeaderSections }

constructor TGridHeaderSections.Create(AHeader: TCustomGridHeader; AOwnerSection: TGridHeaderSection);
begin
  inherited Create(TGridHeaderSection);
  FHeader := AHeader;
  FOwnerSection := AOwnerSection;
end;

function TGridHeaderSections.GetMaxColumn: Integer;
begin
  if Count > 0 then
    Result := Sections[Count-1].ColumnIndex
  else
    Result := 0;
end;

function TGridHeaderSections.GetMaxLevel: Integer;

  procedure DoGetMaxLevel(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := 0 to Sections.Count-1 do
    begin
      S := Sections[I];
      if Result < S.Level then Result := S.Level;
      DoGetMaxLevel(S.Sections);
    end;
  end;

begin
  Result := 0;
  DoGetMaxLevel(Self);
end;

function TGridHeaderSections.GetSection(Index: Integer): TGridHeaderSection;
begin
  Result := TGridHeaderSection(inherited GetItem(Index));
end;

procedure TGridHeaderSections.SetSection(Index: Integer; Value: TGridHeaderSection);
begin
  inherited SetItem(Index, Value);
end;

function TGridHeaderSections.GetOwner: TPersistent;
begin
  Result := OwnerSection;
  if Result = nil then Result := Header;
end;

procedure TGridHeaderSections.Update(Item: TCollectionItem);
begin
  if Header <> nil then Header.Change;
end;

function TGridHeaderSections.Add: TGridHeaderSection;
begin
  if (Header = nil) or (Header.Grid = nil) then
    Result := TGridHeaderSection(inherited Add)
  else
    Result := Header.Grid.CreateHeaderSection(Self);
end;

{ TCustomGridHeader }

constructor TCustomGridHeader.Create(AGrid: TCustomGridView);
begin
  inherited Create;
  FGrid          := AGrid;
  FAutoHeight    := True;
  FColor         := clBtnFace;
  FSections      := TGridHeaderSections.Create(Self, nil);
  FSectionHeight := 17;
  FColor         := clBtnFace;
  FFont          := TFont.Create;
  FFont.OnChange := FontChange;
  FGridFont      := True;
  FFLat          := True;
  FImagesLink    := TChangeLink.Create;
  FImagesLink.OnChange := ImagesChange;
end;

destructor TCustomGridHeader.Destroy;
begin
  FOnChange := nil;
  FreeAndNil(FImagesLink);
  inherited Destroy;
  FreeAndNil(FSections);
  FreeAndNil(FFont);
end;

procedure TCustomGridHeader.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  { backward compatibility }
  Filer.DefineProperty('FullSynchronizing', ReadFullSynchronizing, nil, False);
  Filer.DefineProperty('Synchronized',      ReadFullSynchronizing, nil, False);
  Filer.DefineProperty('AutoSynchronize',   ReadFullSynchronizing, nil, False);
end;

function TCustomGridHeader.IsColorStored: Boolean;
begin
  Result := not GridColor;
end;

function TCustomGridHeader.IsFontStored: Boolean;
begin
  Result := not GridFont;
end;

function TCustomGridHeader.IsSectionHeightStored: Boolean;
begin
  Result := (not FAutoHeight) and (FSectionHeight <> 17);
end;

function TCustomGridHeader.IsSectionsStored: Boolean;
var
  I: Integer;
  S: TGridHeaderSection;
begin
  Result := False;
  for I := 0 to Sections.Count-1 do
  begin
    S := Sections[I];
    if (S.Caption <> '') or (S.Alignment <> taLeftJustify) or S.WordWrap or
       ((S.FSections <> nil) and (S.Sections.Count > 0)) then
      Exit( True );
  end;
end;

procedure TCustomGridHeader.ImagesChange(Sender: TObject);
begin
  Change;
end;

procedure TCustomGridHeader.FontChange(Sender: TObject);
begin
  FGridFont := False;
  { подправляем высоту, изменения }
  SetSectionHeight(SectionHeight);
  Change;
end;

function TCustomGridHeader.GetHeight: Integer;
begin
  Result := (GetMaxLevel + 1) * SectionHeight;
end;

function TCustomGridHeader.GetMaxColumn: Integer;
begin
  Result := Sections.MaxColumn;
end;

function TCustomGridHeader.GetMaxLevel: Integer;
begin
  Result := Sections.MaxLevel;
end;

function TCustomGridHeader.GetNamePath: string;
begin
  if FGrid <> nil then
    Result := FGrid.Name + '.Header'
  else
    Result := inherited GetNamePath;
end;

function TCustomGridHeader.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TCustomGridHeader.GetWidth: Integer;
var
  I: Integer;
  S: TGridHeaderSection;
begin
  Result := 0;
  for I := 0 to Sections.Count-1 do
  begin
    S := Sections[I];
    Result := Result + S.Width;
  end;
end;

procedure TCustomGridHeader.ReadFullSynchronizing(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TCustomGridHeader.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    if Value then SetSectionHeight(SectionHeight);
  end;
end;

procedure TCustomGridHeader.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FGridColor := False;
    Change;
  end;
end;

procedure TCustomGridHeader.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then FImages.UnRegisterChanges(FImagesLink);
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImagesLink);
      if Grid <> nil then FImages.FreeNotification(Grid);
    end;
    { подправляем высоту, изменения }
    SetSectionHeight(SectionHeight);
    Change;
  end;
end;

procedure TCustomGridHeader.SetPopupMenu(const Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    FPopupMenu := Value;
    if (Value <> nil) and (Grid <> nil) then Value.FreeNotification(Grid);
  end;
end;

procedure TCustomGridHeader.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    { подправляем 3D эффект фиксированных }
    { fixed columns and header must have the same 3D effects }
    if Value and (Grid <> nil) then
      Grid.Fixed.Flat := True;
    { подправляем высоту, изменения }
    SetSectionHeight(SectionHeight);
    Change;
  end;
end;

procedure TCustomGridHeader.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCustomGridHeader.SetGridColor(Value: Boolean);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    if Grid <> nil then GridColorChanged(Grid.Color);
    { подправляем высоту, изменения }
    SetSectionHeight(SectionHeight);
    Change;
  end;
end;

procedure TCustomGridHeader.SetGridFont(Value: Boolean);
begin
  if FGridFont <> Value then
  begin
    FGridFont := Value;
    if Grid <> nil then GridFontChanged(Grid.Font);
    Change;
  end;
end;

procedure TCustomGridHeader.SetSections(Value: TGridHeaderSections);
begin
  { устанавливаем заголовок }
  FSections.Assign(Value);
end;

procedure TCustomGridHeader.SetSectionHeight(Value: Integer);
var
  TH, IH: Integer;
begin
  { проверяем автоподбор }
  if AutoHeight then
  begin
    { высота текста }
    TH := Grid.GetFontHeight(Font) + 2 * 2;
    { высота картинки }
    IH := 0;
    if Images <> nil then
    begin
      IH := Images.Height + 2{+ 1};
      if not GridColor then Inc(IH, 1);
      if not Flat then Inc(IH, 1);
    end;
    { высота текста }
    Value := MaxIntValue([0, TH, IH]);
    { take into account borders (conditions are made in the order of drawing
      the header background in PaintHeaderBackground() }
    if not ThemeServices.ThemesEnabled then
    begin
      { double line or 3D frame }
      if Flat then Inc(Value, 2)
      else Inc(Value, 4)
    end
    else if Grid <> nil then
    begin
    {$IFDEF WINDOWS}
    { in Windows XP with themes enabled, we need additional space at the
      bottom of the header for rounding, and in Windows Vista - for
      the sort arrow }
      if CheckWin32Version(6, 0) then
        Inc(Value, Grid.GetSortArrowSize.cy) // + 2
      else
    {$ENDIF}
        Inc(Value, 3);
    end;
  end;
  { высота секций не может быть нулевой }
  if Value < 0 then Value := 0;
  { устанавливаем }
  if FSectionHeight <> Value then
  begin
    FSectionHeight := Value;
    Change;
  end;
end;

procedure TCustomGridHeader.Change;
begin
  { обновляем секции заголовка }
  UpdateSections;
  { событие }
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomGridHeader.GridColorChanged(NewColor: TColor);
begin
  if FGridColor then
  begin
    SetColor(NewColor);
    FGridColor := True;
  end;
end;

procedure TCustomGridHeader.GridFontChanged(NewFont: Tfont);
begin
  if FGridFont then
  begin
    SetFont(NewFont);
    FGridFont := True;
  end;
end;

procedure TCustomGridHeader.Assign(Source: TPersistent);
begin
  if Source is TCustomGridHeader then
  begin
    Sections      := TCustomGridHeader(Source).Sections;
    SectionHeight := TCustomGridHeader(Source).SectionHeight;
    Color         := TCustomGridHeader(Source).Color;
    GridColor     := TCustomGridHeader(Source).GridColor;
    Font          := TCustomGridHeader(Source).Font;
    GridFont      := TCustomGridHeader(Source).GridFont;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TCustomGridHeader.SynchronizeSections;
var
  C: Integer;

  procedure DoAddSections(Column: Integer);
  var
    R: TRect;
  begin
    { абсолютные границы заголовка }
    R.Left   := Grid.GetColumnLeftRight(Column).Left;
    R.Right  := R.Left;
    R.Top    := Grid.ClientRect.Top;
    R.Bottom := R.Top + Height;
    { добавляем }
    while Column < Grid.Columns.Count do
    begin
      { прямоугольник секции }
      R.Left  := R.Right;
      R.Right := R.Left + Grid.Columns[Column].Width;
      { добавляем секцию }
      with Sections.Add do
      begin
        FColumnIndex := Column;
        FBoundsRect  := R;
      end;
      { следующия колонка }
      Inc(Column);
    end;
  end;

  procedure DoDeleteSections(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := Sections.Count-1 downto 0 do
    begin
      S := Sections[I];
      DoDeleteSections(S.Sections);
      if (S.Sections.Count = 0) and
         (S.ColumnIndex > Grid.Columns.Count-1) then
        S.Free;
    end;
  end;

  procedure DoSynchronizeSections(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := Sections.Count-1 downto 0 do
    begin
      S := Sections[I];
      if S.Sections.Count = 0 then
        C := S.ColumnIndex
      else
        DoSynchronizeSections(S.Sections);
    end;
  end;

begin
  { синхронизируем секции }
  if (Grid <> nil) and
     (Grid.ComponentState * [csReading, csLoading] = []) and
     (Grid.Columns <> nil) then
  begin
    Sections.BeginUpdate;
    try
      { необходимо обновить внутренние параметры секций }
      UpdateSections;
      { заголовок пуст - добавляем все колонки }
      if Sections.Count = 0 then
      begin
        DoAddSections(0);
        Exit;
      end;
      { если секций меньше - добавляем, иначе удаляем лишние }
      C := Sections[Sections.Count-1].ColumnIndex;
      if C < Grid.Columns.Count-1 then
        DoAddSections(C + 1)
      else if C > Grid.Columns.Count-1 then
        DoDeleteSections(Sections);
      { у нижних секций синхронизируем заголовок, выравнивание и шинрину }
      DoSynchronizeSections(Sections);
    finally
      Sections.EndUpdate;
    end;
  end;
end;

procedure TCustomGridHeader.UpdateSections;
var
  R: TRect;
  C: Integer;

  procedure DoUpdateColumnIndex(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := 0 to Sections.Count-1 do
    begin
      S := Sections[I];
      { есть ли подзаголовки }
      if S.Sections.Count = 0 then
      begin
        { это нижняя секция }
        S.FColumnIndex := C;
        Inc(C);
      end
      else
      begin
        { рекурсия на все подзаголовки снизу }
        DoUpdateColumnIndex(S.Sections);
        { индекс есть индекс последнего }
        S.FColumnIndex := S.Sections[S.Sections.Count-1].FColumnIndex;
      end;
    end;
  end;

  procedure DoUpdateSecionsBounds(Sections: TGridHeaderSections; Rect: TRect);
  var
    I: Integer;
    S: TGridHeaderSection;
    R, SR: TRect;
  begin
    R := Rect;
    R.Right := R.Left;
    { перебираем подзаголовки }
    for I := 0 to Sections.Count-1 do
    begin
      S := Sections[I];
      R.Left := R.Right;
      R.Right := R.Left + S.Width;
      { прямоугольник }
      SR := R;
      if S.Sections.Count > 0 then SR.Bottom := R.Top + SectionHeight;
      { запоминаем }
      S.FBoundsRect := SR;
      { подзаголовки }
      if S.Sections.Count > 0 then
      begin
        { вычитаем строку сверху }
        SR.Top := SR.Bottom;
        SR.Bottom := R.Bottom;
        { подзаголовки снизу }
        DoUpdateSecionsBounds(S.Sections, SR);
      end;
    end;
  end;

begin
  if (Grid <> nil) and
     (Grid.ComponentState * [csReading, csLoading] = []) and
     (Grid.Columns <> nil) then
  begin
    { определяеи индексы колонок }
    C := 0;
    DoUpdateColumnIndex(Sections);
    { абсолютные границы заголовка }
    R.Left   := Grid.ClientRect.Left;
    R.Right  := R.Left + Grid.GetColumnsWidth(0, Grid.Columns.Count-1);
    R.Top    := Grid.ClientRect.Top;
    R.Bottom := R.Top + Height;
    { определяем границы секций }
    DoUpdateSecionsBounds(Sections, R);
  end;
end;

{ TCustomGridColumn }

constructor TCustomGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColumns        := TGridColumns(Collection);
  FWidth          := 64;
  FMinWidth       := 0;
  FMaxWidth       := 10000;
  FAlignment      := taLeftJustify;
  FTabStop        := True;
  FVisible        := True;
  FAllowClick     := True;
  FAllowEdit      := True;
  FCheckAlignment := taLeftJustify;
  FDefaultPopup   := True;
end;

destructor TCustomGridColumn.Destroy;
begin
  inherited;
  FreeAndNil(FPickList);
end;

function TCustomGridColumn.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

function TCustomGridColumn.GetCaption2: string;
var
  S: TGridHeaderSection;
begin
  Result := Caption;
  S := HeaderSection;
  if S <> nil then
  begin
    if S.Caption <> '' then
      Result := S.Caption;
    S := S.Parent;
    while S <> nil do
    begin
      Result := S.Caption + ' - ' + Result;
      S := S.Parent;
    end;
  end;
end;

function TCustomGridColumn.GetEditAlignment: TAlignment;
begin
  if AlignEdit then
    Result := Alignment
  else
    Result := taLeftJustify;
end;

function TCustomGridColumn.GetGrid: TCustomGridView;
begin
  if Columns <> nil then
    Result := TCustomGridView(Columns.Grid)
  else
    Result := nil;
end;

function TCustomGridColumn.GetHeaderSection: TGridHeaderSection;
begin
  if Grid <> nil then
    Result := Grid.GetHeaderSection(Index, -1)
  else
    Result := nil;
end;

function TCustomGridColumn.GetPickList: TStrings;
begin
  if FPickList = nil then
     FPickList := TStringList.Create;
  Result := FPickList;
end;

function TCustomGridColumn.GetPickListCount: Integer;
begin
  if FPickList <> nil then
    Result := FPickList.Count
  else
    Result := 0;
end;

function TCustomGridColumn.IsPickListStored: Boolean;
begin
  Result := GetPickListCount <> 0;
end;

procedure TCustomGridColumn.ReadMultiline(Reader: TReader);
begin
  WantReturns := Reader.ReadBoolean;
end;

procedure TCustomGridColumn.SetAlignEdit(Value: Boolean);
begin
  if FAlignEdit <> Value then
  begin
    FAlignEdit := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetAllowEdit(Value: Boolean);
begin
  if AllowEdit <> Value then
  begin
    FAllowEdit := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetCheckAlignment(Value: TAlignment);
begin
  if FCheckAlignment <> Value then
  begin
    FCheckAlignment := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetCheckKind(Value: TGridCheckKind);
begin
  if FCheckKind <> Value then
  begin
    FCheckKind := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetEditWordWrap(Value: TGridEditWordWrap);
begin
  if FEditWordWrap <> Value then
  begin
    FEditWordWrap := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetFixedSize(Value: Boolean);
begin
  if FFixedSize <> Value then
  begin
    FFixedSize := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetMaxWidth(Value: Integer);
begin
  if Value < FMinWidth then Value := FMinWidth;
  if Value > 10000 then Value := 10000;
  FMaxWidth := Value;
  SetWidth(FWidth);
end;

procedure TCustomGridColumn.SetMinWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > FMaxWidth then Value := FMaxWidth;
  FMinWidth := Value;
  { patch: prevent TDBGridColumn.DefaultColumn property reset when setting
    minimum width }
  Inc(FWidthLock);
  try
    SetWidth(FWidth);
  finally
    Dec(FWidthLock);
  end;
end;

procedure TCustomGridColumn.SetPickList(Value: TStrings);
begin
  if Value = nil then
    FreeAndNil(FPickList)
  else
    PickList.Assign(Value);
end;

procedure TCustomGridColumn.SetTabStop(Value: Boolean);
begin
  if FTabStop <> Value then
  begin
    FTabStop := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetWantReturns(Value: Boolean);
begin
  if FWantReturns <> Value then
  begin
    FWantReturns := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed(False);
  end;
end;

function TCustomGridColumn.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName
end;

procedure TCustomGridColumn.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetEditMask(const Value: string);
begin
  if FEditMask <> Value then
  begin
    FEditMask := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetEditStyle(Value: TGridEditStyle);
begin
  if FEditStyle <> Value then
  begin
    FEditStyle := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetMaxLength(Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

procedure TCustomGridColumn.SetWidth(Value: Integer);
begin
  if Value < FMinWidth then Value := FMinWidth;
  if Value > FMaxWidth then Value := FMaxWidth;
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(True);
  end;
end;

procedure TCustomGridColumn.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  { для совместимости со старыми версиями, где вместо свойства WantReturns
    было свойство Multiline }
  Filer.DefineProperty('Multiline', ReadMultiline, nil, False);
end;

procedure TCustomGridColumn.Assign(Source: TPersistent);
begin
  if Source is TCustomGridColumn then
  begin
    Caption        := TCustomGridColumn(Source).Caption;
    DefWidth       := TCustomGridColumn(Source).DefWidth;
    MinWidth       := TCustomGridColumn(Source).MinWidth;
    MaxWidth       := TCustomGridColumn(Source).MaxWidth;
    FixedSize      := TCustomGridColumn(Source).FixedSize;
    MaxLength      := TCustomGridColumn(Source).MaxLength;
    Alignment      := TCustomGridColumn(Source).Alignment;
    ReadOnly       := TCustomGridColumn(Source).ReadOnly;
    EditStyle      := TCustomGridColumn(Source).EditStyle;
    EditMask       := TCustomGridColumn(Source).EditMask;
    CheckKind      := TCustomGridColumn(Source).CheckKind;
    CheckAlignment := TCustomGridColumn(Source).CheckAlignment;
    WantReturns    := TCustomGridColumn(Source).WantReturns;
    WordWrap       := TCustomGridColumn(Source).WordWrap;
    TabStop        := TCustomGridColumn(Source).TabStop;
    Visible        := TCustomGridColumn(Source).Visible;
    PickList       := TCustomGridColumn(Source).FPickList;
    Tag            := TCustomGridColumn(Source).Tag;
    AllowClick     := TCustomGridColumn(Source).AllowClick;
    AllowEdit      := TCustomGridColumn(Source).AllowEdit;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TGridColumns }

constructor TGridColumns.Create(AGrid: TCustomGridView);
var
  AClass: TGridColumnClass;
begin
  AClass := TGridColumn;
  if AGrid <> nil then AClass := AGrid.GetColumnClass;
  inherited Create(AClass);
  FGrid := AGrid;
end;

function TGridColumns.GetColumn(Index: Integer): TCustomGridColumn;
begin
  Result := TCustomGridColumn(inherited GetItem(Index));
end;

function TGridColumns.GetLayout: string;
var
  I, W: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    { добавляем в список ширины колонок }
    for I := 0 to Count-1 do
    begin
      { отрицательное значение ширины соотвествует невидимой колонке }
      W := Columns[I].DefWidth;
      if not Columns[I].Visible then W := W * (-1);
      { добавляем в список }
      Strings.Add(IntToStr(W));
    end;
    { результат - ширины колонок, разделенные запятой }
    Result := Strings.CommaText;
  finally
    Strings.Free;
  end;
end;

procedure TGridColumns.SetColumn(Index: Integer; Value: TCustomGridColumn);
begin
  inherited SetItem(Index, Value);
end;

procedure TGridColumns.SetLayout(const Value: string);
var
  I, W: Integer;
  Strings: TStringList;
begin
  BeginUpdate;
  try
    Strings := TStringList.Create;
    try
      { разбиваем ширины колонок, разделенные запятой, на строки }
      Strings.CommaText := Value;
      { меняем ширины колонок }
      for I := 0 to Strings.Count-1 do
      begin
        { проверяем количество колонок }
        if I > Count - 1 then Break;
        { see comment in SetMinWidth() }
        Inc(Columns[I].FWidthLock);
        try
          { получаем ширину }
          W := StrToIntDef(Strings[I], Columns[I].DefWidth);
          { отрицательное значение ширины соотвествует невидимой колонке }
          Columns[I].DefWidth := Abs(W);
          Columns[I].Visible := W > 0;
        finally
          Dec(Columns[I].FWidthLock);
        end;
      end;
    finally
      Strings.Free;
    end;
  finally
    EndUpdate;
  end;
end;

function TGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid; 
end;

procedure TGridColumns.Update(Item: TCollectionItem);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TGridColumns.Add: TCustomGridColumn;
begin
  if Grid = nil then
    Result := TCustomGridColumn(inherited Add)
  else
    Result := Grid.CreateColumn(Self);
end;

{ TCustomGridRows }

constructor TCustomGridRows.Create(AGrid: TCustomGridView);
begin
  inherited Create;
  FGrid := AGrid;
  FHeight := 17;
  FAutoHeight := True;
end;

destructor TCustomGridRows.Destroy;
begin
  FOnChange := nil;
  SetCount(0);
  inherited Destroy;
end;

function TCustomGridRows.GetMaxCount: Integer;
begin
  Result := MaxInt - 2;
  if Height > 0 then
    Result := Result div Height - 2;
end;

function TCustomGridRows.IsHeightStored: Boolean;
begin
  Result := (not FAutoHeight) and (FHeight <> 17);
end;

procedure TCustomGridRows.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;            
    if Value then SetHeight(Height);
  end;
end;

procedure TCustomGridRows.SetHeight(Value: Integer);
var
  TH, FH, CH, IH, GH: Integer;
begin
  { проверяем автоподбор }
  if AutoHeight and (Grid <> nil) then
  begin
    { высота текста }
    { the height of text }
    TH := Grid.GetFontHeight(Grid.Font)       + Grid.TextTopIndent + 1;
    FH := Grid.GetFontHeight(Grid.Fixed.Font) + Grid.TextTopIndent + 1;
    { высота флажков }
    { the height of check boxes }
    if not Grid.CheckBoxes then
      CH := 0
    else
      CH := Grid.CheckHeight + Grid.CheckTopIndent + 1;
    { высота картинки }
    { the height of images }
    if Grid.Images = nil then
      IH := 0
    else
      IH := Grid.Images.Height + Grid.ImageTopIndent + 1;
    { учет сетки }
    { grid lines }
    if not (Grid.GridLines and (gsHorzLine in Grid.GridStyle)) then
      GH := 0
    else
    begin
      GH := Grid.FGridLineWidth;
      if (Grid.Fixed.Count > 0) and
         (not Grid.Fixed.Flat) and
         (not ThemeServices.ThemesEnabled) then
        Inc(GH, 1); // <- 3D
    end;
    { высота строки }
    Value := MaxIntValue([0, TH, FH, CH, IH]) + GH + 1;
  end;
  { высота строк не может быть нулевой }
  if Value < 0 then Value := 0;
  { устанавливаем }
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Count > MaxCount then
      SetCount(Count)
    else
      Change;
  end;
end;

procedure TCustomGridRows.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomGridRows.SetCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > MaxCount then
     Value:= MaxCount;
  if FCount <> Value then
  begin
    FCount := Value;
    Change;
  end;
end;

procedure TCustomGridRows.Assign(Source: TPersistent);
begin
  if Source is TCustomGridRows then
  begin
    Count  := TCustomGridRows(Source).Count;
    Height := TCustomGridRows(Source).Height;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TCustomGridFixed }

constructor TCustomGridFixed.Create(AGrid: TCustomGridView);
begin
  inherited Create;
  FGrid          := AGrid;
  FColor         := clBtnFace;
  FFont          := TFont.Create;
  FFont.OnChange := FontChange;
  FGridFont      := True;
  FFLat          := True;
  FShowDivider   := True;
end;

destructor TCustomGridFixed.Destroy;
begin
  FOnChange := nil;
  inherited Destroy;
  FreeandNil(FFont);
end;

function TCustomGridFixed.IsColorStored: Boolean;
begin
  Result := not GridColor;
end;

function TCustomGridFixed.IsFontStored: Boolean;
begin
  Result := not GridFont;
end;

procedure TCustomGridFixed.FontChange(Sender: TObject);
begin
  FGridFont := False;
  Change;
end;

procedure TCustomGridFixed.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FGridColor := False;
    Change;
  end;
end;

procedure TCustomGridFixed.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    { подправляем 3D эффект заголовка }
    { fixed columns and header must have the same 3D effects }
    if (not Value) and (Grid <> nil) then
      Grid.Header.Flat := False;
    Change;
  end;
end;

procedure TCustomGridFixed.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCustomGridFixed.SetGridColor(Value: Boolean);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    if Grid <> nil then GridColorChanged(Grid.Color);
    Change;
  end;
end;

procedure TCustomGridFixed.SetGridFont(Value: Boolean);
begin
  if FGridFont <> Value then
  begin
    FGridFont := Value;
    if Grid <> nil then GridFontChanged(Grid.Font);
    Change;
  end;
end;

procedure TCustomGridFixed.SetShowDivider(Value: Boolean);
begin
  if FShowDivider <> Value then
  begin
    FShowDivider := Value;
    Change;
  end;
end;

procedure TCustomGridFixed.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomGridFixed.GridColorChanged(NewColor: TColor);
begin
  if FGridColor then
  begin
    SetColor(NewColor);
    FGridColor := True;
  end;
end;

procedure TCustomGridFixed.GridFontChanged(NewFont: TFont);
begin
  if FGridFont then
  begin
    SetFont(NewFont);
    FGridFont := True;
  end;
end;

procedure TCustomGridFixed.SetCount(Value: Integer);
begin
  { подправляем значение }
  if (Grid <> nil) and (Value > Grid.Columns.Count-1) then
    Value := Grid.Columns.Count-1;
  if Value < 0 then Value := 0;
  { устанавливаем }
  if FCount <> Value then
  begin
    FCount := Value;
    Change;
  end;
end;

procedure TCustomGridFixed.Assign(Source: TPersistent);
begin
  if Source is TCustomGridFixed then
  begin
    Count     := TCustomGridFixed(Source).Count;
    Color     := TCustomGridFixed(Source).Color;
    GridColor := TCustomGridFixed(Source).GridColor;
    Font      := TCustomGridFixed(Source).Font;
    GridFont  := TCustomGridFixed(Source).GridFont;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TGridScrollBar }

constructor TGridScrollBar.Create(AGrid: TCustomGridView; AKind: TScrollBarKind);
begin
  inherited Create;
  FGrid := AGrid;
  FKind := AKind;
  if AKind = sbHorizontal then
    FBarCode := SB_HORZ
  else
    FBarCode := SB_VERT;
  FPageStep := 100;
  FLineStep := 8;
  FLineSize := 1;
  FTracking := True;
  FVisible  := True;
end;

procedure TGridScrollBar.SetLineSize(Value: Integer);
begin
  if Value < 1 then
    FLineSize := 1
  else
    FLineSize := Value;
end;

procedure TGridScrollBar.SetLineStep(Value: Integer);
begin
  SetParams(Min, Max, PageStep, Value);
end;

procedure TGridScrollBar.SetMax(Value: Integer);
begin
  SetParams(Min, Value, PageStep, LineStep);
end;

procedure TGridScrollBar.SetMin(Value: Integer);
begin
  SetParams(Value, Max, PageStep, LineStep);
end;

procedure TGridScrollBar.SetPageStep(Value: Integer);
begin
  SetParams(Min, Max, Value, LineStep);
end;

procedure TGridScrollBar.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Update;
  end;
end;

procedure TGridScrollBar.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGridScrollBar.ChangeParams;
begin
  if Assigned(FOnChangeParams) then FOnChangeParams(Self);
end;

procedure TGridScrollBar.Scroll(ScrollCode: Integer; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then FOnScroll(Self, ScrollCode, ScrollPos);
end;

procedure TGridScrollBar.ScrollMessage(var Message: TLMScroll);
var
  ScrollInfo: TScrollInfo;
begin
  ScrollInfo := Default(TScrollInfo);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_TRACKPOS;
  if GetScrollInfo(FGrid.Handle, FBarCode, ScrollInfo) then
  begin
    LockUpdate;
    try
      case Message.ScrollCode of
        SB_LINELEFT:      SetPositionEx(Position - LineStep,  Message.ScrollCode);
        SB_LINERIGHT:     SetPositionEx(Position + LineStep,  Message.ScrollCode);
        SB_PAGELEFT:      SetPositionEx(Position - PageStep,  Message.ScrollCode);
        SB_PAGERIGHT:     SetPositionEx(Position + PageStep,  Message.ScrollCode);
        SB_THUMBPOSITION: SetPositionEx(ScrollInfo.nTrackPos, Message.ScrollCode);
        SB_THUMBTRACK:    if Tracking then SetPositionEx(ScrollInfo.nTrackPos, Message.ScrollCode);
        SB_ENDSCROLL:     SetPositionEx(Position, Message.ScrollCode);
      end;
    finally
      UnLockUpdate;
    end;
  end
  else
    inherited;
end;

procedure CheckScrollPos(Min, Max, PageStep: Integer; var Position: Integer);
begin
  if Position > Max - PageStep + 1 then
     Position:= Max - PageStep + 1;
  if Position < Min then
     Position:= Min;
end;

procedure TGridScrollBar.SetParams(AMin, AMax, APageStep, ALineStep: Integer);
begin
  { подправляем новые значения }
  if AMax < AMin then
     AMax:= AMin;
  if APageStep > AMax - AMin + 1 then
     APageStep:= AMax - AMin + 1;
  if APageStep < 0 then APageStep := 0;
  if ALineStep < 0 then ALineStep := 0;
  { изменилось ли что нибудь }
  if (FMin <> AMin) or (FMax <> AMax) or (FPageStep <> APageStep) or (FLineStep <> ALineStep) then
  begin
    { устанавливаем новые значения }
    FMin := AMin;
    FMax := AMax;
    FPageStep := APageStep;
    FLineStep := ALineStep;
    { подправляем позицию }
    CheckScrollPos(FMin, FMax, FPageStep, FPosition);
    { обновляем скроллер }
    Update;
    { событие }
    ChangeParams;
  end;
end;

procedure TGridScrollBar.SetPosition(Value: Integer);
begin
  SetPositionEx(Value, SB_ENDSCROLL);
end;

procedure TGridScrollBar.SetPositionEx(Value: Integer; ScrollCode: Integer);
var
  R: TRect;
begin
  { проверяем позицию }
  CheckScrollPos(FMin, FMax, FPageStep, Value);
  if Value <> FPosition then
  begin
    Scroll(ScrollCode, Value);
    CheckScrollPos(FMin, FMax, FPageStep, Value);
  end;
  { изменилась ли позиция }
  if Value <> FPosition then
  begin
    { сдвигаем сетку }
    with FGrid do
    begin
      { гасим фокус }
      { focus rect should be hidden while scrolling grid }
      HideFocus;
      { сдвигаем }
      if FKind = sbHorizontal then
      begin
        R := Default(TRect);
        UnionRect(R, GetHeaderRect, GetGridRect);
        R.Left := GetFixedRect.Right;
        ScrollWindowEx(Handle, (FPosition - Value) * FLineSize, 0, @R, @R, 0, nil, SW_INVALIDATE);
      end
      else
      begin
        R := GetGridRect;
        ScrollWindowEx(Handle, 0, (FPosition - Value) * FLineSize, @R, @R, 0, nil, SW_INVALIDATE);
      end;
      { устанавливаем новую позицию }
      FPosition :=  Value;
      { показываем фокус }
      ShowFocus;
    end;
    { устанавливаем скроллер }
    Update;
    { изменение }
    Change;
  end;
end;

procedure TGridScrollBar.Update;
var
  ScrollInfo: TScrollInfo;
begin
  if FGrid.HandleAllocated and (FUpdateLock = 0) then
  begin
    ScrollInfo := Default(TScrollInfo);
    { параметры скроллера }
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    if Visible and (Max <> Min) then
    begin
      ScrollInfo.nMin  := Min;
      ScrollInfo.nMax  := Max;
      ScrollInfo.nPage := PageStep;
      ScrollInfo.nPos  := Position;
    end;
    SetScrollInfo(FGrid.Handle, FBarCode, ScrollInfo, True);
  end;
end;

procedure TGridScrollBar.Assign(Source: TPersistent);
begin
  if Source is TGridScrollBar then
  begin
    LockUpdate;
    try
      PageStep := TGridScrollBar(Source).PageStep;
      LineStep := TGridScrollBar(Source).LineStep;
      Min      := TGridScrollBar(Source).Min;
      Max      := TGridScrollBar(Source).Max;
      Position := TGridScrollBar(Source).Position;
      Tracking := TGridScrollBar(Source).Tracking;
      Visible  := TGridScrollBar(Source).Visible;
      Exit;
    finally
      UnLockUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TGridScrollBar.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TGridScrollBar.UnLockUpdate;
begin
  Dec(FUpdateLock);
  if FUpdateLock = 0 then Update;
end;

{ TGridListBox }

constructor TGridListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IntegralHeight := True;
  ParentShowHint := False;
  ShowHint       := False;
end;

procedure TGridListBox.CreateParams(var Params: TCreateParams);
const
  CS_SAVEBITS = $800;
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TGridListBox.CreateWnd;
begin
  inherited CreateWnd;
  CallWindowProc(DefWndProc, Handle, LM_SETFOCUS, 0, 0);
end;

procedure TGridListBox.KeyPress(var Key: Char);
var
  TickCount: UInt64;
  i: Integer;
begin
  case Key of
    #8, #27:
      { сбрасываем текст поиска }
      FSearchText := '';
    #32..#255:
      { инициируем поиск }
      begin
        TickCount := GetTickCount64;
        if TickCount - FSearchTime > 2000 then
          FSearchText := '';
        FSearchTime := TickCount;
        if UTF8Length(FSearchText) < 32 then
          FSearchText := FSearchText + Key;
        i := FindInStrings(FSearchText, Items, [fsIgnoreCase, fsFromBegin]);
        if i <> -1 then ItemIndex:= i;
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TGridListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Grid <> nil) and (Grid.Edit <> nil) then
    Grid.Edit.CloseUp((X >= 0) and (Y >= 0) and (X < Width) and (Y < Height));
end;

{ TCustomGridEdit }

constructor TCustomGridEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { внутренние переменные }
  FEditStyle     := geSimple;
  FDropDownCount := 0;
  FButtonWidth   := GetSystemMetrics(SM_CXVSCROLL);
  { параметры внешнего вида }
  TabStop        := False;
  BorderStyle    := bsNone;
  ParentShowHint := False;
  ShowHint       := False;
end;

function TCustomGridEdit.GetButtonRect: TRect;
begin
  Result := Classes.Rect(Width - FButtonWidth, 0, Width, Height);
end;

function TCustomGridEdit.GetClosingUp: Boolean;
begin
  Result := FCloseUpCount <> 0;
end;

function TCustomGridEdit.GetLineCount: Integer;
var
  P: PChar;
begin
  Result := 0;
  P := Pointer(Text);
  while P^ <> #0 do
  begin
    while not CharInSet(P^, [#0, #10, #13]) do Inc(P);
    Inc(Result);
    if P^ = #13 then Inc(P);
    if P^ = #10 then Inc(P);
  end;
end;

function TCustomGridEdit.GetPressing: Boolean;
begin
  Result := FPressCount <> 0;
end;

procedure TCustomGridEdit.ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (FActiveList <> nil) then
    CloseUp(PtInRect(FActiveList.ClientRect, Classes.Point(X, Y)));
end;

procedure TCustomGridEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd(Self);
  end;
end;

procedure TCustomGridEdit.SetButtonWidth(Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    Repaint;
  end;
end;

procedure TCustomGridEdit.SetDropListVisible(Value: Boolean);
begin
  if Value then
    DropDown
  else
    CloseUp(False);
end;

procedure TCustomGridEdit.SetEditStyle(Value: TGridEditStyle);
begin
  if FEditStyle <> Value then
  begin
    FEditStyle := Value;
    Repaint;
  end;
end;

procedure TCustomGridEdit.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RecreateWnd(Self);
  end;
end;

procedure TCustomGridEdit.LMGetDlgCode(var Message: TLMGetDlgCode);
begin
  inherited;
  { символы, переход по нажатию TAB }
  with Message do
  begin
    Result := Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
    if (Grid <> nil) and (gkTabs in Grid.CursorKeys) then
      Result := Result or DLGC_WANTTAB;
  end;
end;

procedure TCustomGridEdit.LMCancelMode(var Message);
begin
  StopButtonTracking;
  inherited;
end;

procedure TCustomGridEdit.LMKillFocus(var Message);
begin
  inherited;
  { ignore the focus message if the dialog box appears after selecting
    a drop-down list item, for example, a ColorDialog when selecting the
    "More Color ..." item in color list }
  if ClosingUp or Pressing then
    Exit;
  { stop editing the cell text when focus is lost }
  try
    CloseUp(False);
  except
    Application.HandleException(Self);
  end;
end;

procedure TCustomGridEdit.LMWindowPosChanged(var Message);
begin
  inherited;
  Invalidate;
end;

procedure TCustomGridEdit.LMPaint(var Message: TLMPaint);
begin
  PaintHandler(Message);
end;

procedure TCustomGridEdit.LMLButtonDown(var Message: TLMLButtonDown);
begin
  //SendCancelMode(Self); not implemented in Lazarus
  with Message do
    // чтобы выделение текста не гасло при нажатии на кнопку, обрабатываем нажатие сами
    if (EditStyle <> geSimple) and PtInrect(ButtonRect, Classes.Point(XPos, YPos)) then
    begin
      if csCaptureMouse in ControlStyle then
        MouseCapture := True;
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
    end
    else
    begin
      CloseUp(False);
      inherited;
    end;
end;

procedure TCustomGridEdit.LMLButtonDblClk(var Message: TLMLButtonDblClk);
var
  P: TPoint;
begin
  with Message do
  begin
    P := Classes.Point(XPos, YPos);
    if (FEditStyle <> geSimple) and PtInRect(GetButtonRect, P) then
      Exit;
  end;
  inherited;
end;

procedure TCustomGridEdit.LMSetCursor(var Message);
//var
//  P: TPoint;
begin
  { не попала ли мышка на кнопку }
  //GetCursorPos(P);
  //if (FEditStyle <> geSimple) and PtInRect(GetButtonRect, ScreenToClient(P)) then
  if (FEditStyle <> geSimple) and PtInRect(GetButtonRect, ScreenToClient(Mouse.CursorPos)) then
  begin
    //LCLIntf.SetCursor(Screen.Cursors[crArrow]);
    Cursor := crArrow;
    Exit;
  end;
  inherited;
end;

procedure TCustomGridEdit.LMPaste(var Message);
begin
  if EditCanModify then
    inherited;
end;

procedure TCustomGridEdit.LMCut(var Message);
begin
  if EditCanModify then
    inherited;
end;

procedure TCustomGridEdit.LMClear(var Message);
begin
  if EditCanModify then
    inherited;
end;

procedure TCustomGridEdit.Undo;
begin
  if (Grid = nil) or Grid.EditCanUndo(Grid.EditCell) then
    inherited;
end;

procedure TCustomGridEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then
    CloseUp(False);
end;

procedure TCustomGridEdit.CMEnabledChanged(var Message);
begin
  inherited;
  Invalidate;
end;

procedure TCustomGridEdit.CMShowingChanged(var Message);
begin
  { игнорируем изменение видимости через изменение свойства Visible }
  { ignore visibility change through Visible property change }
end;

procedure TCustomGridEdit.LMContextMenu(var Message: TLMessage);
begin
  { если свойство DefaultPopupMenu таблицы установлено в True, то по правой
    кнопке надо показать стандартное popup меню строки ввода, а не Popup
    меню таблицы }
  if (Grid <> nil) and Grid.DefaultEditMenu and Assigned(Grid.PopupMenu) then
    with Message do
      Result := CallWindowProc(DefWndProc, Handle, Msg, WParam, LParam)
  else
    inherited;
end;

procedure TCustomGridEdit.CMFontChanged(var Message);
begin
  inherited;
  { in Borland C ++ Builder, after changing the font, the position of the
    inplace editor is shifted, it must be corrected }
  UpdateBounds(Visible, False);
end;

procedure TCustomGridEdit.CMMouseEnter(var Message);
begin
  inherited;
  if ThemeServices.ThemesEnabled and not FButtonHot then
  begin
    FButtonHot := True;
    InvalidateButton;
  end;
end;

procedure TCustomGridEdit.CMMouseLeave(var Message);
begin
  inherited;
  if ThemeServices.ThemesEnabled and FButtonHot then
  begin
    FButtonHot := False;
    InvalidateButton;
  end;
end;

procedure TCustomGridEdit.ApplyListValue(Accept: Boolean);
var
  I: Integer;
  Items: TStrings;
  ItemText: string;
begin
  if (FActiveList <> nil) and (FActiveList is TGridListBox) then
  begin
    { selected text can be rejected, accepted or modified using OnEditCloseUp
      grid event }
    I     := TGridListBox(FActiveList).ItemIndex;
    Items := TGridListBox(FActiveList).Items;
    if I <> -1 then
      ItemText := Items[I]
    else
      ItemText := '';
    if Grid <> nil then
      Grid.EditCloseUp(Grid.EditCell, Items, I, ItemText, Accept);
    if Accept and (I <> -1) then
    begin
      Text := ItemText;
      SetSel(0, -1);
    end;
  end;
end;

procedure TCustomGridEdit.Change;
begin
  if Grid <> nil then Grid.EditChange(Grid.EditCell);
end;

procedure TCustomGridEdit.CreateParams(var Params: TCreateParams);
const
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
  Aligns: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and
    (not WordWraps[FWordWrap]) or ES_MULTILINE or Aligns[FAlignment];
end;

procedure TCustomGridEdit.DblClick;
begin
  { событие таблицы }
  if Grid <> nil then Grid.DblClick;
  { двойной щелчок - эмуляция нажатия на кнопку }
  case EditStyle of
    geEllipsis: Press;
    gePickList,
    geDataList: if not FDropListVisible then SelectNext;
  else
    //
  end;
end;

function TCustomGridEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := (Grid <> nil) and Grid.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TCustomGridEdit.EditCanModify: Boolean;
begin
  Result := (Grid <> nil) and Grid.EditCanModify(Grid.EditCell);
end;

function TCustomGridEdit.GetDropList: TWinControl;
begin
  if FPickListBox = nil then
  begin
    FPickListBox := TGridListBox.Create(Self);
    FPickListBox.FGrid := Grid;
  end;
  Result := FPickListBox;
end;

procedure TCustomGridEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    if Grid <> nil then
    begin
      Grid.KeyDown(Key, Shift);
      Key := 0;
    end;
  end;

begin
  case Key of
    VK_UP, // перемещение фокуса
    VK_DOWN: if (Shift = [ssCtrl]) or ((Shift = []) and (not (WantReturns or WordWrap))) then SendToParent;
    VK_PRIOR, // перемещение фокуса
    VK_NEXT: {if Shift = [ssCtrl] then} SendToParent;
    VK_ESCAPE: SendToParent;
    VK_DELETE: if not EditCanModify then SendToParent;
    VK_INSERT: if (not EditCanModify) or (Shift = []) then SendToParent;
//  VK_LEFT,
//  VK_RIGHT,
    VK_HOME, // перемещение фокуса при нажатом Ctrl
    VK_END: if Shift = [ssCtrl] then SendToParent;
    VK_TAB: if not (ssAlt in Shift) then SendToParent;
  end;
  if Key <> 0 then // если кнопка не обработана
  begin
    if (Grid <> nil) and Assigned(Grid.OnKeyDown) then
      Grid.OnKeyDown(Grid, Key, Shift);
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TCustomGridEdit.KeyPress(var Key: Char);
begin
  if Grid <> nil then
  begin
    { отсылаем клавишу таблице }
    Grid.KeyPress(Key);
    { проверяем доступность символа }
    { char can be dropped by grid OnEditAcceptKey event  }
    if (Key in [#32..#255]) and not Grid.EditCanAcceptKey(Grid.EditCell, Key) then
    begin
      Key := #0;
      Beep;
    end;
    case Key of
      #9, #27, #13: Key := #0; // TAB, ESC, ENTER убираем
      ^H, ^V, ^X, #32..#255:   // BACKSPACE, обычные символы убираем, если нельзя редактировать
        if not EditCanModify then // drop BACKSPACE and chars when cell is readonly
        begin
          Key := #0;
          Beep;
        end;
    end;
  end;
  if Key <> #0 then // если символ не обработан
    inherited KeyPress(Key);
end;

procedure TCustomGridEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Grid <> nil then Grid.KeyUp(Key, Shift);
end;

procedure TCustomGridEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  { проверяем нажатие на кнопку }
  if (Button = mbLeft) and (EditStyle <> geSimple) and PtInrect(ButtonRect, Classes.Point(X, Y)) then
  begin
    { видим ли список }
    if FDropListVisible then
      { закрываем его }
      CloseUp(False)
    else
    begin
      { начинаем нажатие на кнопку и, если нужно, открываем список }
      StartButtonTracking(X, Y);
      if EditStyle <> geEllipsis then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomGridEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  M: TSmallPoint;
begin
  if FButtonTracking then
  begin
    { нажатие на кнопку }
    StepButtonTracking(X, Y);
    { для открытого списка }
    { stop button tracking when moving over drop-down list }
    if FDropListVisible then
    begin
      { получаем точку на списке }
      P := FActiveList.ScreenToClient(ClientToScreen(Classes.Point(X, Y)));
      { если попали на список }
      if PtInRect(FActiveList.ClientRect, P) then
      begin
        { прекращаем нажатие на кнопку }
        StopButtonTracking;
        { эмулируем нажатие на список }
        { emulate a click on the drop-down list to close it when you release
          the mouse button }
        M := PointToSmallPoint(P);
        SendMessage(FActiveList.Handle, LM_LBUTTONDOWN, 0, LongInt(M));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomGridEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: Boolean;
begin
  P := FButtonPressed;
  { завершаем нажатие }
  StopButtonTracking;
  { нажатие на кнопку }
  if (Button = mbLeft) and (EditStyle = geEllipsis) and P then
    Press;
  { обработка по умолчанию }
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomGridEdit.PaintButton(DC: HDC; Rect: TRect);
var
  Detail1: TThemedButton;
  Detail2: TThemedComboBox;
  Flags: Integer;
begin
  case EditStyle of
    geEllipsis:
      if ThemeServices.ThemesEnabled then
      begin
        if FButtonPressed then
          Detail1 := tbPushButtonPressed
        //else if Hot then
        //  Detail1 := tbPushButtonHot
        else
          Detail1 := tbPushButtonNormal;
        with ThemeServices do
          DrawElement(DC, GetElementDetails(Detail1), Rect);
      end
      else
      begin
        Flags := 0;
        if FButtonPressed then
          Flags := BF_FLAT;
        DrawEdge(DC, Rect, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
      end;
    gePickList, geDataList:
      if ThemeServices.ThemesEnabled then
      begin
        if FButtonPressed then
          Detail2 := tcDropDownButtonPressed
        //else if Hot then
        //  Detail2 := tcDropDownButtonHot
        else
          Detail2 := tcDropDownButtonNormal;
        with ThemeServices do
          DrawElement(DC, GetElementDetails(Detail2), Rect);
      end
      else
      begin
        Flags := 0;
        if FButtonPressed then
          Flags := DFCS_FLAT;
        DrawEdge(DC, Rect, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
      end;
  else
    //
  end;
end;

procedure TCustomGridEdit.PaintWindow(DC: HDC);
var
  R: TRect;
begin
  if (EditStyle <> geSimple) then
  begin
    R := GetButtonRect;
    { рисуем кнопку }
    PaintButton(DC, R);
    { убираем прямоугольник кнопки из области отрисовки }
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited;
end;

procedure TCustomGridEdit.StartButtonTracking(X, Y: Integer);
begin
  MouseCapture := True;
  FButtonTracking := True;
  StepButtonTracking(X, Y);
end;

procedure TCustomGridEdit.StepButtonTracking(X, Y: Integer);
var
  R: TRect;
  P: Boolean;
begin
  R := GetButtonRect;
  P := PtInRect(R, Classes.Point(X, Y));
  if FButtonPressed <> P then
  begin
    FButtonPressed := P;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TCustomGridEdit.StopButtonTracking;
begin
  if FButtonTracking then
  begin
    StepButtonTracking(-1, -1);
    FButtonTracking := False;
    MouseCapture := False;
  end;
end;

type
  THackWinControl = class(TWinControl);

procedure TCustomGridEdit.UpdateBounds(Showing, ScrollCaret: Boolean);
const
  Flags: array[Boolean] of Integer = (0, SWP_SHOWWINDOW or SWP_NOREDRAW);
  EM_SETRECTNP = 180;
  EM_SCROLLCARET = 183;
var
  R, F: TRect;
  L, T, W, H: Integer;
  TI: TPoint;
begin
  if Grid <> nil then
  begin
    { определяем прямоугольник ячейки строки ввоа }
    R := Grid.GetEditRect(Grid.EditCell);
    F := R;
    { подправляем строку в соотвествии с фиксированными }
    with Grid.GetFixedRect do
    begin
      if R.Left < Right then R.Left := Right;
      if R.Right < Right then R.Right := Right;
    end;
    { подправляем строку в соотвествии с заголовком }
    with Grid.GetHeaderRect do
    begin
      if R.Top < Bottom then R.Top := Bottom;
      if R.Bottom < Bottom then R.Bottom := Bottom;
    end;
    { устанавливаем положение }
    W := R.Right - R.Left;
    H := R.Bottom - R.Top;
    SetWindowPos(Handle, HWND_TOP, R.Left, R.Top, W, H, Flags[Showing]);
    { вычисляем новые границы текста }
    L := F.Left - R.Left;
    T := F.Top - R.Top;
    W := F.Right - F.Left;
    H := F.Bottom - F.Top;
    { смещение текста }
    TI := Grid.GetCellTextIndent(Grid.EditCell);
    { учитываем кнопку }
    if EditStyle <> geSimple then Dec(W, ButtonWidth + 4) else Dec(W, Grid.TextRightIndent);
    { устанавливаем границы текста }
    R := Bounds(L + TI.X, T + TI.Y, W - TI.X + Ord(Alignment = taRightJustify), H);
    SendMessage(Handle, EM_SETRECTNP, 0, {%H-}LPARAM(@R));
    { курсор в конец строки }
    if ScrollCaret then SendMessage(Handle, EM_SCROLLCARET, 0, 0);
    { when scrolling the grid horizontally, inplace editor should be redrawn
      immediately, otherwise a black rectangle (garbage) will flash
      in its place if grid Doublebuffered is True }
    if Grid.DoubleBuffered then UpdateWindow(Handle);
  end
end;

procedure TCustomGridEdit.UpdateColors;
var
  Canvas: TCanvas;
begin
  if Grid <> nil then
  begin
    Canvas := TCanvas.Create;
    try
      { получаем цвета ячейки }
      Grid.GetCellColors(Grid.EditCell, Canvas);
      { запоминаем их }
      Color := Canvas.Brush.Color;
      Font := Canvas.Font;
    finally
      Canvas.Free;
    end;
  end;
end;

procedure TCustomGridEdit.UpdateContents;
begin
  if (Grid = nil) or (not Grid.IsCellValid(Grid.EditCell)) then
    Exit;
  { обновляем параметры строки }
  with Grid do
  begin
    Self.MaxLength   := Columns[EditCell.Col].MaxLength;
    Self.ReadOnly    := IsCellReadOnly(EditCell) or (Self.MaxLength = -1);
    Self.WantReturns := Columns[EditCell.Col].WantReturns;
    case Columns[EditCell.Col].EditWordWrap of
      ewAuto:    Self.WordWrap := Columns[EditCell.Col].WordWrap;
      ewEnabled: Self.WordWrap := True;
    else
      Self.WordWrap := False;
    end;
    Self.Alignment := Columns[EditCell.Col].EditAlignment;
    Self.EditMask  := GetEditMask(EditCell);
    Self.Text      := GetEditText(EditCell);
  end;
end;

procedure TCustomGridEdit.UpdateList;
begin
  if FActiveList <> nil then
  begin
    FActiveList.Visible := False;
    FActiveList.Parent := Self;
    THackWinControl(FActiveList).OnMouseUp := ListMouseUp;
    THackWinControl(FActiveList).Font := Font;
  end;
end;

procedure TCustomGridEdit.UpdateListBounds;
var
  I, X, W: Integer;
  R, Rect: TRect;
  P: TPoint;
  Monitor: TMonitor;
begin
  if (Grid = nil) or (FActiveList = nil) then
    Exit;
  { get an active monitor, it will be used below to determine the height of
    the screen instead of the Screen }
  R := Self.ClientRect;
  P := Self.ClientOrigin;
  OffsetRect(R, P.X, P.Y);
  Monitor := Screen.MonitorFromRect(R);
  if Monitor <> nil then
    Rect := Monitor.WorkareaRect
  else
    Rect := Screen.WorkareaRect;
  { define bounds only for TGridListBox and descendants }
  if FActiveList is TGridListBox then
    with TGridListBox(FActiveList) do
    begin
      Canvas.Font := Font;
      if Items.Count > 0 then
      begin
        W := 0;
        for I := 0 to Items.Count-1 do
        begin
          X := Canvas.TextWidth(Items[I]);
          if W < X then W := X;
        end;
        ClientWidth := W + 6;
      end
      else
        ClientWidth := 100;
      { if the maximum number of items displayed in the drop-down list is not
        specified, then make it equal to 1/3 of the screen height }
      if Items.Count = 0 then
        ClientHeight := ItemHeight
      else
      begin
        I := FDropDownCount;
        if I = 0 then
        begin
          I := ((Rect.Bottom - Rect.Top) div 3) div ItemHeight;
          if I < 1 then I := 1;
        end;
        if I > Items.Count then I := Items.Count;
        ClientHeight := I * ItemHeight;
      end;
    end;
  { подправлям размеры списка в зависимости от размеров колонки и его
    положение на экране }
  with FActiveList do
  begin
    { подправляем по ширине колонки }
    R := Grid.GetCellRect(Grid.EditCell);
    Width := MaxIntValue([Width, R.Right - R.Left]);
    { положение }
    Left := P.X + Self.Width - Width;
    Top := P.Y + Self.Height;
    if Top + Height > Rect.Bottom - Rect.Top then Top := P.Y - Height;
    { подправляем в соотвествием с пожеланием пользователя }
    { list bounds can be redefined in OnGetEditListBounds event }
    R := BoundsRect;
    Grid.GetEditListBounds(Grid.EditCell, R);
    BoundsRect := R;
  end;
end;

procedure TCustomGridEdit.UpdateListItems;
begin
  if (Grid = nil) or (FActiveList = nil) or (not (FActiveList is TGridListBox)) then
    Exit;
  { обновляем выпадающий список }
  with TGridListBox(FActiveList) do
  begin
    { очищаем старый список, заполняем новый }
    Items.Clear;
    Grid.GetEditList(Grid.EditCell, Items);
    { устанавливаем выделенную позицию }
    ItemIndex := Grid.GetEditListIndex(Grid.EditCell, Items, Self.Text);
  end;
end;

procedure TCustomGridEdit.UpdateStyle;
var
  Style: TGridEditStyle;
begin
  { получаем стиль строки }
  Style := geSimple;
  if (Grid <> nil) and (not Grid.ReadOnly) then 
    Style := Grid.GetEditStyle(Grid.EditCell);
  { устанавливаем }
  EditStyle := Style;
end;

{
  Delete the requested message from the queue, but throw back
  any LM_QUIT msgs that PeekMessage may also return.
}
procedure KillMessage(Wnd: HWND; Msg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, PM_REMOVE) and (M.Message = LM_QUIT) then
    PostMessage(M.hwnd, LM_QUIT, M.wParam, M.lParam);
end;

procedure TCustomGridEdit.WndProc(var Message: TLMessage);

  procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
  begin
    case Key of
      VK_UP, VK_DOWN:
        { открытие или закрытие }
        if ssAlt in Shift then
        begin
          if FDropListVisible then CloseUp(True) else DropDown;
          Key := 0;
        end;
      VK_RETURN, VK_ESCAPE:
        { закрытие списка }
        if (not (ssAlt in Shift)) and FDropListVisible then
        begin
          KillMessage(Handle, LM_CHAR);
          CloseUp(Key = VK_RETURN);
          Key := 0;
        end;
    end;
  end;

  procedure DoButtonKeys(var Key: Word; Shift: TShiftState);
  begin
    if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
    begin
      KillMessage(Handle, LM_CHAR);
      Key := 0;
      { эмуляция нажатия на кнопку }
      { Ctrl+Enter works like a button click }
      case EditStyle of
        geEllipsis: Press;
        gePickList, geDataList: if not FDropListVisible then SelectNext;
      else
        //
      end;
    end;
  end;

var
  Form: TCustomForm;
begin
  case Message.Msg of
    LM_KEYDOWN,
    LM_SYSKEYDOWN,
    LM_CHAR:
        with TLMKey(Message) do
        begin
          { открытие списка }
          if EditStyle in [gePickList, geDataList] then
          begin
            DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
            { передаем оставшееся событие списку }
            if (CharCode <> 0) and FDropListVisible then
            begin
              with Message do SendMessage(FActiveList.Handle, Msg, WParam, LParam);
              Exit;
            end;
          end;
          { эмуляция нажатия на кнопку }
          if not WantReturns then
          begin
            DoButtonKeys(CharCode, KeyDataToShiftState(KeyData));
            if CharCode = 0 then Exit;
          end;
        end;
    LM_SETFOCUS:
      begin
        Form := GetParentForm(Self);
        if (Form = nil) or Form.SetFocusedControl(Grid) then Dispatch(Message);
        Exit;
      end;
    LM_LBUTTONDOWN:
      { двойное нажатие мышки }
      with TLMLButtonDown(Message) do
      begin
        { на нажатие на кнопку не реагируем }
        if (EditStyle = geSimple) or (not PtInrect(ButtonRect, Classes.Point(XPos, YPos))) then
          { смотрим время повторного щелчка }
          if GetTickCount64 - FClickTime < GetDoubleClickTime then
            { меняем сообщение на двойной щелчок }
            Message.Msg := LM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TCustomGridEdit.CloseUp(Accept: Boolean);
const
  Flags = SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW;
begin
  if FDropListVisible then
  begin
    if GetCapture <> 0 then 
      SendMessage(GetCapture, LM_CANCELMODE, 0, 0);
    { скрываем список }
    //SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, Flags);
    FActiveList.Visible := False;
    FDropListVisible := False;
    Invalidate;
    { set ClosingUp state for WMKillFocus }
    Inc(FCloseUpCount);
    try
      ApplyListValue(Accept);
      { return focus to the inplace editor in case the OnEditCloseUp event
        handler displayed a dialog, for example, a color dialog }
      SetFocus;
    finally
      Dec(FCloseUpCount);
    end;
  end;
end;

procedure TCustomGridEdit.Deselect;
begin
  SetSel(0, -1);
end;

procedure TCustomGridEdit.DropDown;
const
  Flags = SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW;
begin
  if (not FDropListVisible) and (Grid <> nil) and (EditStyle in [gePickList, geDataList]) then
  begin
    { получаем выпадающий список }
    FActiveList := GetDropList;
    if FActiveList <> nil then
    begin
      { заполяем список, устанавливаем размеры }
      UpdateList;
      UpdateListItems;
      { protection against exotic errors: if, when retrieving a list the user
        decides to cancel editing, then the list should not be shown }
      if not Grid.Editing then
      begin
        StopButtonTracking;
        Exit;
      end;
      UpdateListBounds;
      { показываем список }
      //SetWindowPos(FActiveList.Handle, HWND_TOP, FActiveList.Left, FActiveList.Top, 0, 0, Flags);
      FActiveList.Visible := True;
      FDropListVisible := True;
      Invalidate;
      { устанавливаем на него фокус }
      //LCLIntf.SetFocus(FActiveList.Handle);
      FActiveList.SetFocus;
    end;
  end;
end;

procedure TCustomGridEdit.Invalidate;
var
  Cur: TRect;
begin
  { проверяем таблицу }
  if Grid = nil then
  begin
    inherited Invalidate;
    Exit;
  end;
  { перерисовываемся }
  InvalidateRect(Handle, nil, True);
  { обновляем прямоугольник таблицы }
  { grid under the inplace editor must be invalidated too }
  Cur := Default(TRect);
  LCLIntf.GetClientRect(Handle, Cur);
  MapWindowPoints(Handle, Grid.Handle, @Cur, 2);
  InvalidateRect(Grid.Handle, @Cur, False);
end;

procedure TCustomGridEdit.InvalidateButton;
var
  R: TRect;
begin
  R := GetButtonRect;
  InvalidateRect(Handle, @R, False);
end;

procedure TCustomGridEdit.Hide;
const
  Flags = SWP_HIDEWINDOW or SWP_NOZORDER or SWP_NOREDRAW;
begin
  if (Grid <> nil) and HandleAllocated and Visible then
  begin
    { сбрасываем флаг редактирования }
    Grid.FEditing := False;
    { скрываем строку ввода }
    Invalidate;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, Flags);
    { удаляем фокус }
    { the grid should not lose focus }
    if Focused then
    begin
      FDefocusing := True;
      try
        //LCLIntf.SetFocus(Grid.Handle);
        Grid.SetFocus;
      finally
        FDefocusing := False;
      end;
    end;
  end;
end;

procedure TCustomGridEdit.Press;
begin
  // if EditCanModify then <- a read-only editor can have an ellipsis button
  begin
    Inc(FPressCount);
    try
      Grid.EditButtonPress(Grid.EditCell);
      { after closing the dialog box (if any), the focus should be restored
        to the inplace editor }
      SetFocus;
    finally
      Dec(FPressCount);
    end;
  end;
end;

procedure TCustomGridEdit.SelectNext;
var
  OldText, NewText: string;
begin
  if Grid <> nil then
  begin
    OldText := Text;
    NewText := OldText;
    { вызываем метод таблицы }
    Grid.EditSelectNext(Grid.EditCell, NewText);
    { устанавливаем новое значение }
    if NewText <> OldText then
    begin
      Text := NewText;
      SetSel(0, -1);
    end;
  end;
end;

procedure TCustomGridEdit.SetFocus;
begin
  //if IsWindowVisible(Handle) then LCLIntf.SetFocus(Handle);
  if Visible then inherited SetFocus;
end;

procedure TCustomGridEdit.Show;
var
  CursorPos: TPoint;
  ScrollCaret: Boolean;
begin
  if Grid <> nil then
  begin
    ScrollCaret := not Grid.FEditing;
    { поднимаем флаг редактирования }
    Grid.FEditing := True;
    Grid.FCellSelected := True;
    { подправляем цвета (следует делать до установки границ, так как
      они выставляются в зависимости от размера шрифта) }
    UpdateColors;
    { получаем размеры }
    UpdateBounds(True, ScrollCaret);
    { hot flag to draw a button when themes are enabled }
    CursorPos := Default(TPoint);
    if LCLIntf.GetCursorPos(CursorPos) then
    begin
      CursorPos := ScreenToClient(CursorPos);
      FButtonHot := PtInRect(Bounds(0, 0, Width, Height), CursorPos);
    end
    else
      FButtonHot := False;
    { устанавливаем фокус }
    if Grid.Focused then LCLIntf.SetFocus(Handle);
  end;
end;

{ TGridTipsWindow }

constructor TGridTipsWindow.Create(AOwner: TComponent);
begin
  inherited;
  { Delphi hint color is different from Windows hit color }
  Color := clInfoBk;
end;

procedure TGridTipsWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  Monitor: TMonitor;
  R: TRect;
  X, Y: Integer;
begin
  Caption := AHint;
  //UpdateBoundsRect(Rect);
  BoundsRect := Rect;
  { do not show a hint behind the screen }
  Monitor := Screen.MonitorFromPoint(Rect.TopLeft);
  if Monitor <> nil then
  begin
    R := Monitor.BoundsRect;
    X := Rect.Left;
    if Rect.Right > R.Right then X := X - (Rect.Right - R.Right);
    if X < R.Left then X := Monitor.Left;
    Y := Rect.Top;
    if Rect.Bottom > R.Bottom then Y := Y - (Rect.Bottom - R.Bottom);
    if Y < R.Top then Y := R.Top;
    OffsetRect(Rect, X - Rect.Left, Y - Rect.Top);
  end;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
    SWP_SHOWWINDOW or SWP_NOACTIVATE);
  Invalidate;
end;

procedure TGridTipsWindow.ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer);
begin
  FGrid := AData;
  inherited ActivateHintData(Rect, AHint, AData);
end;

function TGridTipsWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
var
  R: TRect;
begin
  FGrid := AData;
  if FGrid = nil then
  begin
    Result := inherited CalcHintRect(MaxWidth, AHint, AData);
    InflateRect(Result, 2, 2); // <- see CreateParams()
    Exit;
  end;
  { the hint rectangle matches the cell rect with border }
  R := FGrid.GetTipsRect(FGrid.FTipsCell, AHint);
  OffsetRect(R, -R.Left, -R.Top);
  Result := R;
end;

procedure TGridTipsWindow.CMTextChanged(var Message);
begin
  { ignore message to prevent flickering hint window }
end;

procedure TGridTipsWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  { the smooth animation function always draws a rectangular frame, but
    in Windows Vista with themes enabled it should have rounded edges,
    so we will draw the window frame manually }
  with Params do
  begin
    Style := WS_POPUP and not WS_BORDER;
    ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
  end;
end;

procedure TGridTipsWindow.Paint;
var
  TI: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
  T: string;
  R: TRect;
begin
  if FGrid = nil then
  begin
    inherited;
    Exit;
  end;
  FGrid.GetCellColors(FGrid.FTipsCell, Canvas);
  { theme text color is not supported }
  Canvas.Font.Color := Screen.HintFont.Color;
  { use grid PaintText() function to draw hint text like cell }
  with FGrid do
  begin
    TI := GetCellTextIndent(FTipsCell);
    A := Columns[FTipsCell.Col].Alignment;
    WR := Pos(#13, FTipsText) <> 0; // Columns[FTipsCell.Col].WantReturns;
    WW := Columns[FTipsCell.Col].WordWrap;
    T := FTipsText;
  end;
  R := ClientRect;
  InflateRect(R, -1, -1);
  FGrid.PaintText(Canvas, R, TI.X, A, WR, WW, T);
end;

procedure TGridTipsWindow.LMEraseBkgnd(var Message: TLMEraseBkgnd);
var
  R: TRect;
  Details: TThemedElementDetails;
  //C: COLORREF;
begin
  inherited;
{ when themes are enabled in Windows Vista and above, draw the background
  with the frame using the current theme, with the themes disabled, draw
  a solid frame like the explorer (the background is already filled by
  inherited call) }
  if ThemeServices.ThemesEnabled {and CheckWin32Version(6, 0)} then
  begin
    Details := ThemeServices.GetElementDetails(tttStandardNormal);
    ThemeServices.DrawElement(Message.DC, Details, ClientRect);
    { patch for Window 7 and higher: the standard theme draw the bottom
      right pixel with a light color, so a bright dot is visible on the
      shadow background }
    //if (Win32MajorVersion = 6) and (Win32MinorVersion < 2) then
    //begin
    //  C := GetPixel(Message.DC, 1, 0);
    //  SetPixel(Message.DC, Width - 1, Height - 1, C);
    //end;
  end
  else
  begin
    R := ClientRect;
    DrawEdge(Message.DC, R, BDR_RAISEDOUTER, BF_RECT or BF_MONO);
  end;
end;

{ TGridFindDialog }

procedure TGridFindDialog.DoClose;
begin
  inherited;
  FVisible := False;
end;

procedure TGridFindDialog.DoShow;
begin
  inherited;
  FVisible := True;
end;

procedure TGridFindDialog.ShowModal;
var
  ActiveWindow: HWnd;
  Forms: TList;
begin
  ActiveWindow := GetActiveWindow;
  Forms := TList.Create;
  Screen.DisableForms(nil, Forms);
  try
    if Execute then
      repeat
        Application.HandleMessage;
        if Application.Terminated then FVisible := False;
      until not FVisible;
  finally
    Screen.EnableForms(Forms);
    FreeThenNil(Forms);
    SetActiveWindow(ActiveWindow);
  end;
end;

{ TCustomGridView }

constructor TCustomGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csNeedsBorderPaint];
  //Width := 185;
  //Height := 105;
  //Color := clWindow;
  //ParentColor := False;
  //TabStop := True;
  FHorzScrollBar := CreateScrollBar(sbHorizontal);
  FHorzScrollBar.OnScroll := HorzScroll;
  FHorzScrollBar.OnChange := HorzScrollChange;
  FVertScrollBar := CreateScrollBar(sbVertical);
  FVertScrollBar.LineSize := 17;
  FVertScrollBar.OnScroll := VertScroll;
  FVertScrollBar.OnChange := VertScrollChange;
  FHeader := CreateHeader;
  FHeader.OnChange := HeaderChange;
  FColumns := CreateColumns;
  FColumns.OnChange := ColumnsChange;
  FRows := CreateRows;
  FRows.OnChange := RowsChange;
  FFixed := CreateFixed;
  FFixed.OnChange := FixedChange;
  FImagesLink := TChangeLink.Create;
  FImagesLink.OnChange := ImagesChange;
  FBorderStyle := bsSingle;
  FShowHeader := True;
  FGridLines := True;
  FGridLineWidth := 1; // NOTE !! не менять !!
  FGridStyle := [gsHorzLine, gsVertLine];
  FGridColor := clWindow;
  FEndEllipsis := True;
  FImageLeftIndent := 2;
  FImageTopIndent := 1;
  FImageHighlight := True;
  FTextLeftIndent := 5;
  FTextRightIndent := 5;
  FTextTopIndent := 0;
  FShowFocusRect := True;
  FRightClickSelect := True;
  FAllowSelect := True;
  FCursorKeys := [gkArrows, gkMouse, gkMouseWheel];
  FColumnsResize := True;
  FColumnClick := True;
  FEditCell := GridCell(-1, -1);
  FCheckStyle := csWin95;
  FCheckWidth := 16;
  FCheckHeight := 16;
  FCheckLeftIndent := 0;
  FCheckTopIndent := 0;
  FCheckBuffer := Graphics.TBitmap.Create;
  FSortLeftIndent := 4;
  FSortTopIndent := 0;
  FSortBuffer := Graphics.TBitmap.Create;
  FPatternBitmap := Graphics.TBitmap.Create;
  FPatternBitmap.Width := 2;
  FPatternBitmap.Height := 2;
  FCancelOnExit := True;
  FGridHintColor := clGrayText;
  Width := 185;
  Height := 105;
  Color := clWindow;
  ParentColor := False;
  TabStop := True;
end;

destructor TCustomGridView.Destroy;
begin
  FreeandNil(FPatternBitmap);
  FreeandNil(FSortBuffer);
  FreeandNil(FCheckBuffer);
  FreeandNil(FImagesLink);
  inherited Destroy;
  FreeandNil(FFixed);
  FreeandNil(FRows);
  FreeandNil(FColumns);
  FreeandNil(FHeader);
  FreeandNil(FHorzScrollBar);
  FreeandNil(FVertScrollBar);
end;

function TCustomGridView.GetCell(Col, Row: Longint): string;
begin
  Result := GetCellText(GridCell(Col, Row));
end;

function TCustomGridView.GetChecked(Col, Row: Longint): Boolean;
begin
  Result := GetCheckState(GridCell(Col, Row)) in [cbChecked, cbGrayed];
end;

function TCustomGridView.GetCheckBoxEnabled(Col, Row: Longint): Boolean;
begin
  GetCheckStateEx(GridCell(Col, Row), Result);
end;

function TCustomGridView.GetCheckBoxState(Col, Row: Longint): TCheckBoxState;
begin
  Result := GetCheckState(GridCell(Col, Row));
end;

function TCustomGridView.GetCol: Longint;
begin
  Result := CellFocused.Col;
end;

function TCustomGridView.GetFixed: TCustomGridFixed;
begin
  Result := FFixed;
end;

function TCustomGridView.GetEdit: TCustomGridEdit;
begin
  Result := FEdit;
end;

function TCustomGridView.GetEditColumn: TCustomGridColumn;
begin
  Result := nil;
  if (EditCell.Col >= 0) and (EditCell.Col < Columns.Count) then Result := Columns[EditCell.Col];
end;

function TCustomGridView.GetEditDropDown: Boolean;
begin
  Result := (Edit <> nil) and Edit.DropListVisible;
end;

function TCustomGridView.GetEditFocused: Boolean;
begin
  Result := Editing and FEdit.Focused;
end;

function TCustomGridView.GetEditing: Boolean;
begin
  Result := FEditing and (FEdit <> nil);
end;

function TCustomGridView.GetHeader: TCustomGridHeader;
begin
  Result := FHeader;
end;

function TCustomGridView.GetLeftCol: Longint;
begin
  Result := VisOrigin.Col;
end;

function TCustomGridView.GetLightenColor(Color: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
begin
  if Color < 0 then Color := GetSysColor(Color and $000000FF);
  R := Color and $FF + Amount;
  G := Color shr 8 and $FF + Amount;
  B := Color shr 16 and $FF + Amount;
  if R < 0 then R := 0 else if R > 255 then R := 255;
  if G < 0 then G := 0 else if G > 255 then G := 255;
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := R or (G shl 8) or (B shl 16);
end;

function TCustomGridView.GetRow: Longint;
begin
  Result := CellFocused.Row;
end;

function TCustomGridView.GetRows: TCustomGridRows;
begin
  Result := FRows;
end;

function TCustomGridView.GetTopRow: Longint;
begin
  Result := VisOrigin.Row;
end;

function TCustomGridView.GetVisibleColCount: Longint;
begin
  Result := VisSize.Col;
end;

function TCustomGridView.GetVisibleRowCount: Longint;
begin
  Result := VisSize.Row;
end;

procedure TCustomGridView.ColumnsChange(Sender: TObject);
begin
  if [csReading, csLoading] * ComponentState = [] then
  begin
    UpdateFixed;
    UpdateHeader;
  end;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursor;
  UpdateEdit(Editing);
  Invalidate;
  ChangeColumns;
end;

function TCustomGridView.CompareStrings(const S1, S2: string; WholeWord,
  MatchCase: Boolean): Boolean;
begin
  if WholeWord then
    if MatchCase then
      Result := UTF8CompareStr(S1, S2) = 0
    else
      Result := UTF8CompareText(S1, S2) = 0
  else
    if MatchCase then
      Result := UTF8Pos(S1, S2) > 0
    else
      Result := UTF8Pos(UTF8Uppercase(S1), UTF8Uppercase(S2)) > 0;
end;

procedure TCustomGridView.FixedChange(Sender: TObject);
begin
  UpdateRows;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursor;
  UpdateEdit(Editing);
  Invalidate;
  ChangeFixed; 
end;

procedure TCustomGridView.HandlerFind(Sender: TObject);
begin
  FindText(FindDialog.FindText, FindDialog.Options);
end;

procedure TCustomGridView.HandlerFindMenu(Sender: TObject);
begin
  FindDialog.ShowModal;
end;

procedure TCustomGridView.HandlerFindNext(Sender: TObject);
begin
  with FindDialog do
    if FindText = '' then
      ShowModal
    else
      Self.FindText(FindText, Options + [frDown]);
end;

procedure TCustomGridView.HandlerFindPrev(Sender: TObject);
begin
  with FindDialog do
    if FindText = '' then
      ShowModal
    else
      Self.FindText(FindText, Options - [frDown]);
end;

procedure TCustomGridView.HeaderChange(Sender: TObject);
begin
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateEdit(Editing);
  UpdateCursor;
  Invalidate;
end;

procedure TCustomGridView.HorzScroll(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer);
begin
  CancelCellTips;
  if FocusOnScroll then UpdateFocus;
end;

procedure TCustomGridView.HorzScrollChange(Sender: TObject);
begin
  UpdateVisOriginSize;
  UpdateEdit(Editing);
end;

procedure TCustomGridView.ImagesChange(Sender: TObject);
begin
  InvalidateGrid;
  UpdateRows;
end;

procedure TCustomGridView.RowsChange(Sender: TObject);
begin
  { при изменении количества строк гасим подсказку }
  CancelCellTips;
  { обновляем свойства }
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursor;
  UpdateEdit(Editing);
  Invalidate;
  ChangeRows;
end;

procedure TCustomGridView.SetAllowEdit(Value: Boolean);
begin
  if FAllowEdit <> Value then
  begin
    FAllowEdit := Value;
    { гасим строку редактирвоания или построчное выделение }
    if not Value then
    begin
      AlwaysEdit := False;
      HideEdit;
    end
    else
      RowSelect := False;
    { событие }
    ChangeEditMode;
  end;
end;

procedure TCustomGridView.SetAllowSelect(Value: Boolean);
begin
  if FAllowSelect <> Value then
  begin
    FAllowSelect := Value;
    RowSelect := FRowSelect or (not Value);
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetAlwaysEdit(Value: Boolean);
begin
  if FAlwaysEdit <> Value then
  begin
    FAlwaysEdit := Value;
    if Value then
    begin
      AllowEdit := True;
      Editing := True;
    end
    else
      HideEdit;
  end;
end;

procedure TCustomGridView.SetAlwaysSelected(Value: Boolean);
begin
  if FAlwaysSelected <> Value then
  begin
    FAlwaysSelected := Value;
    FCellSelected := FCellSelected or Value;
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    inherited SetBorderStyle(Value);
    RecreateWnd(Self);
  end;
end;

procedure TCustomGridView.SetCell(Col, Row: Longint; Value: string);
begin
  SetEditText(GridCell(Col, Row), Value);
end;

procedure TCustomGridView.SetCellFocused(Value: TGridCell);
begin
  SetGridCursor(Value, CellSelected, True);
end;

procedure TCustomGridView.SetCellSelected(Value: Boolean);
begin
  SetGridCursor(CellFocused, Value, True);
end;

procedure TCustomGridView.SetCheckBoxes(Value: Boolean);
begin
  if FCheckBoxes <> Value then
  begin
    FCheckBoxes := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCheckLeftIndent(Value: Integer);
begin
  if FCheckLeftIndent <> Value then
  begin
    FCheckLeftIndent := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCheckStyle(Value: TGridCheckStyle);
begin
  if FCheckStyle <> Value then
  begin
    FCheckStyle := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCheckTopIndent(Value: Integer);
begin
  if FCheckTopIndent <> Value then
  begin
    FCheckTopIndent := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCol(Value: Longint);
begin
  CellFocused := GridCell(Value, CellFocused.Row);
end;

procedure TCustomGridView.SetColumns(Value: TGridColumns);
begin
  FColumns.Assign(Value);
end;

procedure TCustomGridView.SetCursorKeys(Value: TGridCursorKeys);
begin
  { проверяем несовместимые флаги }
  if gkMouseMove in Value then Include(Value, gkMouse);
  if not (gkMouse in Value) then Exclude(Value, gkMouseMove);
  { устанавливаем значение }
  FCursorKeys := Value;
end;

procedure TCustomGridView.SetEditDropDown(Value: Boolean);
begin
  { переводим ячейку в режим редактирвания }
  Editing := True;
  { показываем выпадающий список }
  if Edit <> nil then Edit.DropListvisible := True;
end;

procedure TCustomGridView.SetEditing(Value: Boolean);
var
  WasEditing: Boolean;
begin
  WasEditing := Editing;
  { проверяем начало редактирования }
  if Value and AllowEdit then
  begin
    { фокус на таблицу, показываем строку ввода }
    if AcquireFocus then
    begin
      CancelDrag;
      ShowEdit;
    end;
  end
  { смотрим завершение ввода }
  else if (not Value) and FEditing then
  begin
    ApplyEditText;

    if not AlwaysEdit then HideEdit;
  end;
  if WasEditing <> Editing then ChangeEditing;
end;

procedure TCustomGridView.SetEndEllipsis(Value: Boolean);
begin
  if FEndEllipsis <> Value then
  begin
    FEndEllipsis := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetFlatBorder(Value: Boolean);
begin
  if FFlatBorder <> Value then
  begin
    FFlatBorder := Value;
    RecreateWnd(Self);
  end;
end;

procedure TCustomGridView.SetFixed(Value: TCustomGridFixed);
begin
  FFixed.Assign(Value);
end;

procedure TCustomGridView.SetHeader(Value: TCustomGridHeader);
begin
  FHeader.Assign(Value);
end;

procedure TCustomGridView.SetHideSelection(Value: Boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetHighlightEvenRows(const Value: Boolean);
begin
  if FHighlightEvenRows <> Value then
  begin
    FHighlightEvenRows := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetHighlightFocusCol(const Value: Boolean);
begin
  if FHighlightFocusCol <> Value then
  begin
    FHighlightFocusCol := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetHighlightFocusRow(const Value: Boolean);
begin
  if FHighlightFocusRow <> Value then
  begin
    FHighlightFocusRow := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetHorzScrollBar(Value: TGridScrollBar);
begin
  FHorzScrollBar.Assign(Value);
end;

procedure TCustomGridView.SetGrayReadOnly(const Value: Boolean);
begin
  if FGrayReadOnly <> Value then
  begin
    FGrayReadOnly := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetGridColor(Value: TColor);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetGridHint(const Value: string);
begin
  if FGridHint <> Value then
  begin
    FGridHint := Value;
    if IsGridHintVisible then Invalidate;
  end;
end;

procedure TCustomGridView.SetGridHintColor(Value: TColor);
begin
  if FGridHintColor <> Value then
  begin
    FGridHintColor := Value;
    if IsGridHintVisible then Invalidate;
  end;
end;

procedure TCustomGridView.SetGridLines(Value: Boolean);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    UpdateRows;
    UpdateEdit(Editing);
    Invalidate;
  end;
end;

procedure TCustomGridView.SetGridStyle(Value: TGridStyles);
begin
  if FGridStyle <> Value then
  begin
    FGridStyle := Value;
    UpdateRows;
    UpdateEdit(Editing);
    Invalidate;
  end;
end;

procedure TCustomGridView.SetImageIndexDef(Value: Integer);
begin
  if FImageIndexDef <> Value then
  begin
    FImageIndexDef := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImageHighlight(Value: Boolean);
begin
  if FImageHighlight <> Value then
  begin
    FImageHighlight := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImageLeftIndent(Value: Integer);
begin
  if FImageLeftIndent <> Value then
  begin
    FImageLeftIndent := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then FImages.UnRegisterChanges(FImagesLink);
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImagesLink);
      FImages.FreeNotification(Self);
    end;
    { подправляем параметры }
    UpdateRows;
    UpdateEdit(Editing);
    { перерисовываем таблицу }
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImageTopIndent(Value: Integer);
begin
  if FImageTopIndent <> Value then
  begin
    FImageTopIndent := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetLeftCol(Value: Longint);
begin
  VisOrigin := GridCell(Value, VisOrigin.Row);
end;

procedure TCustomGridView.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    UpdateEditContents(True);
  end;
end;

procedure TCustomGridView.SetRow(Value: Longint);
begin
  CellFocused := GridCell(CellFocused.Col, Value);
end;

procedure TCustomGridView.SetRows(Value: TCustomGridRows);
begin
  FRows.Assign(Value);
end;

procedure TCustomGridView.SetRowSelect(Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    if Value then AllowEdit := False;
    AllowSelect := AllowSelect or (not Value);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetShowCellTips(Value: Boolean);
begin
  if FShowCellTips <> Value then
  begin
    FShowCellTips := Value;
    ShowHint := ShowHint or Value;
  end;
end;

procedure TCustomGridView.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetShowGridHint(Value: Boolean);
begin
  if FShowGridHint <> Value then
  begin
    FShowGridHint := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetShowHeader(Value: Boolean);
begin
  if FShowHeader <> Value then
  begin
    FShowHeader := Value;
    HeaderChange(Header);
  end;
end;

procedure TCustomGridView.SetSortLeftIndent(Value: Integer);
begin
  if FSortLeftIndent <> Value then
  begin
    FSortLeftIndent := Value;
    UpdateHeader;
    InvalidateHeader;
  end;
end;

procedure TCustomGridView.SetSortTopIndent(Value: Integer);
begin
  if FSortTopIndent <> Value then
  begin
    FSortTopIndent := Value;
    UpdateHeader;
    InvalidateHeader;
  end;
end;

procedure TCustomGridView.SetTextLeftIndent(Value: Integer);
begin
  if FTextLeftIndent <> Value then
  begin
    FTextLeftIndent := Value;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetTextRightIndent(Value: Integer);
begin
  if FTextRightIndent <> Value then
  begin
    FTextRightIndent := Value;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetTextTopIndent(Value: Integer);
begin
  if FTextTopIndent <> Value then
  begin
    FTextTopIndent := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetTopRow(Value: Longint);
begin
  VisOrigin := GridCell(VisOrigin.Col, Value);
end;

procedure TCustomGridView.SetVertScrollBar(Value: TGridScrollBar);
begin
  FVertScrollBar.Assign(Value);
end;

procedure TCustomGridView.SetVisOrigin(Value: TGridCell);
begin
  if (FVisOrigin.Col <> Value.Col) or (FVisOrigin.Row <> Value.Row) then
  begin
    FVisOrigin := Value;
    { подправляем положение движков скроллеров }
    UpdateScrollPos;
    UpdateVisOriginSize;
    { перерисовываем таблицу }
    Invalidate;
  end;
end;

procedure TCustomGridView.VertScroll(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer);
begin
  CancelCellTips;
  if FocusOnScroll then UpdateFocus;
end;

procedure TCustomGridView.VertScrollChange(Sender: TObject);
begin
  UpdateVisOriginSize;
  UpdateEdit(Editing);
end;

procedure TCustomGridView.LMGetDlgCode(var Message: TLMGetDlgCode);
begin
  with Message do
  begin
    Result := DLGC_WANTARROWS;
    if not RowSelect then
    begin
      if gkTabs in CursorKeys then Result := Result or DLGC_WANTTAB;
      if AllowEdit then Result := Result or DLGC_WANTCHARS;
    end;
  end;
end;

procedure TCustomGridView.LMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if Rows.Count > 0 then
  begin
    InvalidateFocus;
    if (FEdit <> nil) and (Message.FocusedWnd <> FEdit.Handle) then HideCursor;
  end;
end;

procedure TCustomGridView.LMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if Rows.Count > 0 then
  begin
    InvalidateFocus;
    if (FEdit = nil) or ((Message.FocusedWnd <> FEdit.Handle) or (not FEdit.FDefocusing)) then ShowCursor;
  end;
end;

procedure TCustomGridView.LMLButtonDown(var Message);
begin
  inherited;
  if FEdit <> nil then FEdit.FClickTime := GetTickCount64;
end;

procedure TCustomGridView.LMChar(var Msg: TLMChar);
var
  Shift: TShiftState;
begin
  { показываем строку ввода, если можно }
  if AllowEdit and (Char(Msg.CharCode) in [^H, #32..#255]) then
  begin
    Shift := KeyDataToShiftState(Msg.KeyData);
    if Shift * [ssCtrl, ssAlt] = [] then
    begin
      ShowEditChar(Char(Msg.CharCode));
      Exit;
    end;
  end;
  { иначе обработка по умолчанию }
  inherited;
end;

procedure TCustomGridView.LMHScroll(var Message: TLMHScroll);
begin
  if Message.ScrollBar = 0 then
    FHorzScrollBar.ScrollMessage(Message)
  else
    inherited;
end;

procedure TCustomGridView.LMVScroll(var Message: TLMVScroll);
begin
  if Message.ScrollBar = 0 then
    FVertScrollBar.ScrollMessage(Message)
  else
    inherited;
end;

procedure TCustomGridView.LMNCHitTest(var Message: TLMNCHitTest);
begin
  inherited;
  FHitTest := ScreenToClient(SmallPointToPoint(Message.Pos));
end;

procedure TCustomGridView.LMEraseBkgnd(var Message: TLMessage);
begin
  Message.Result := 1;
end;

procedure TCustomGridView.LMTimer(var Message);
begin
  KillTimer(Handle, 1);
  Editing := True;
end;

procedure TCustomGridView.LMUser(var Message);
begin
  UpdateEdit(AlwaysEdit);
end;

procedure TCustomGridView.CMCancelMode(var Message: TLMessage);
begin
  if FEdit <> nil then FEdit.WndProc(Message);
  inherited;
end;

procedure TCustomGridView.CMEnabledChanged(var Message);
begin
  inherited;
  Invalidate;
end;

procedure TCustomGridView.CMFontChanged(var Message);
begin
  { запоминаем шрифт }
  Canvas.Font := Font;
  { подправляем шрифт у заголовка и фиксированных, высоту строк }
  UpdateFonts;
  UpdateRows;
  { обработка по умолчанию }
  inherited;
end;

procedure TCustomGridView.CMColorChanged(var Message);
begin
  { запоминаем цвет }
  Brush.Color := Color;
  { подправляем цвет у заголовка и фиксированных }
  UpdateColors;
  { обработка по умолчанию }
  inherited;
end;

procedure TCustomGridView.CMShowHintChanged(var Message);
begin
  ShowCellTips := ShowCellTips and ShowHint;
end;

procedure TCustomGridView.CMHintShow(var Message: TLMessage);
var
  AllowTips: Boolean;
  R, TR: TRect;
  W: Integer;
begin
  with Message, {%H-}PHintInfo(LParam)^ do
  begin
    if not ShowCellTips then
    begin
      inherited;
      Exit;
    end;
    { если не попали в таблицу - выход }
    if not PtInRect(GetGridRect, CursorPos) then
    begin
      Result := 1;
      Exit;
    end;
    { ищем ячейку, на которую указывает курсор }
    FTipsCell := GetCellAt(CursorPos.X, CursorPos.Y);
    { если не попали - подсказки нет, выход }
    if IsCellEmpty(FTipsCell) then
    begin
      Result := 1;
      Exit;
    end;
    { а не идет ли редактирование этой ячейки }
    { hint is not allowed when editing cell text }
    if IsCellEditing(FTipsCell) then
    begin
      Result := 1;
      Exit;
    end;
    { а нужны ли подсказки для ячейки }
    { hint can be disabled by OnCellTips event }
    CellTips(FTipsCell, AllowTips);
    if not AllowTips then
    begin
      Result := 1;
      Exit;
    end;
    { hint is shown only if the cell text gets out of cell bounds }
    { получаем прямоугольник ячейки (без картинки) }
    R := GetCellHintRect(FTipsCell);
    { получаем прямоугольник текста ячейки }
    TR := GetCellTextBounds(FTipsCell);
    { смещаем его в соотвествии с выравниванием }
    W := TR.Right - TR.Left;
    case Columns[FTipsCell.Col].Alignment of
      taCenter:
        begin
          TR.Left := R.Left - (W - (R.Right - R.Left)) div 2;
          TR.Right := TR.Left + W;
        end;
      taRightJustify:
        begin
          TR.Right := R.Right;
          TR.Left := TR.Right - W;
        end;
    else
      TR.Left := R.Left;
      TR.Right := TR.Left + W;
    end;
    { учитываем видимую часть таблицы }
    IntersectRect(R, R, ClientRect);
    if ShowHeader then SubtractRect(R, R, GetHeaderRect);
    if FTipsCell.Col >= Fixed.Count then SubtractRect(R, R, GetFixedRect);
    { а вылезает ли текст за ячейку (слева, справа или по высоте) }
    if (TR.Left >= R.Left) and (TR.Right <= R.Right) and
      (TR.Bottom - TR.Top <= R.Bottom - R.Top) then
    begin
      Result := 1;
      Exit;
    end;
    { получаем текст подсказки }
    FTipsText := GetTipsText(FTipsCell);
    { получаем прямоугольник подсказки }
    R := GetTipsRect(FTipsCell, FTipsText);
    { настраиваем положение и текст подсказки }
    HintPos := ClientToScreen(R.TopLeft);
    HintStr := FTipsText;
    { настраиваем прямоугольник реакции мышки }
    R := GetCellRect(FTipsCell);
    if FTipsCell.Col < Fixed.Count then
    begin
      R.Left := MaxIntValue([R.Left, GetFixedRect.Left]);
      R.Right := MinIntValue([R.Right, GetFixedRect.Right]);
    end
    else
    begin
      R.Left := MaxIntValue([R.Left, GetFixedRect.Right]);
      R.Right := MinIntValue([R.Right, GetGridRect.Right]);
    end;
    InflateRect(R, 1, 1);
    CursorRect := R;
    { тип окна подсказки }
    HintWindowClass := GetTipsWindowClass;
    HintData := Self;
    { хинт можно показать }
    Result := 0;
  end;
end;

procedure TCustomGridView.CMMouseLeave(var Message);
begin
  inherited;
  if FHotSection <> nil then
  begin
    FHotSection := nil;
    InvalidateSection(FHotColumn, FHotLevel);
  end;
end;

procedure TCustomGridView.CMWinIniChange(var Message);
begin
  inherited;
  { i don't remember why, but the inplace editor needs to be updated
    after changing Windows settings }
  UpdateEditContents(True);
end;

function TCustomGridView.AcquireFocus: Boolean;
begin
  Result := True;
  { if the focus is now on the input line, then grid is in focus too }
  if (FEdit <> nil) and FEdit.Focused then Exit;
  { можно ли устанавливать фокус }
  if not (csDesigning in ComponentState) and CanFocus then
  begin
    UpdateFocus;
    Result := IsActiveControl;
  end;
end;

procedure TCustomGridView.CancelCellTips;
var
  //P: TPoint;
  HintControl: TControl;

  function GetHintControl(Control: TControl): TControl;
  begin
    Result := Control;
    while (Result <> nil) and not Result.ShowHint do Result := Result.Parent;
    if (Result <> nil) and (csDesigning in Result.ComponentState) then Result := nil;
  end;

begin
  if ShowCellTips then
  begin
    //Windows.GetCursorPos(P);
    //HintControl := GetHintControl(FindDragTarget(P, False));
    HintControl := GetHintControl(FindDragTarget(Mouse.CursorPos, False));
    if HintControl = Self then Application.CancelHint;
  end;
end;

procedure TCustomGridView.CellClick(Cell: TGridCell; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnCellClick) then FOnCellClick(Self, Cell, Shift, X, Y);
end;

procedure TCustomGridView.CellTips(Cell: TGridCell; out AllowTips: Boolean);
begin
  AllowTips := True;
  if Assigned(FOnCellTips) then FOnCellTips(self, Cell, AllowTips);
end;

procedure TCustomGridView.Change(var Cell: TGridCell; var Selected: Boolean);
begin
  if Assigned(FOnChange) then FOnChange(Self, Cell, Selected);
end;

procedure TCustomGridView.ChangeColumns;
begin
  if Assigned(FOnChangeColumns) then FOnChangeColumns(Self);
end;

procedure TCustomGridView.ChangeEditing;
begin
  if Assigned(FOnChangeEditing) then FOnChangeEditing(Self);
end;

procedure TCustomGridView.ChangeEditMode;
begin
  if Assigned(FOnChangeEditMode) then FOnChangeEditMode(Self);
end;

procedure TCustomGridView.ChangeFixed;
begin
  if Assigned(FOnChangeFixed) then FOnChangeFixed(Self);
end;

procedure TCustomGridView.ChangeRows;
begin
  if Assigned(FOnChangeRows) then FOnChangeRows(Self);
end;

procedure TCustomGridView.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  inherited ChangeScale(M, D);
  if M <> D then
  begin
    with Columns do
    begin
      BeginUpdate;
      try
        for I := 0 to Count-1 do
        begin
          Columns[I].FMaxWidth := MulDiv(Columns[I].FMaxWidth, M, D);
          Columns[I].FMinWidth := MulDiv(Columns[I].FMinWidth, M, D);
          Columns[I].DefWidth := MulDiv(Columns[I].DefWidth, M, D);
        end;
      finally
        EndUpdate;
      end;
    end;
    with Rows do
      Height := MulDiv(Height, M, D);
    with Header do
    begin
      SectionHeight := MulDiv(SectionHeight, M, D);
      if not GridFont then Font.Size := MulDiv(Font.Size, M, D);
    end;
    with Fixed do
      if not GridFont then Font.Size := MulDiv(Font.Size, M, D);
  end;
end;

procedure TCustomGridView.Changing(var Cell: TGridCell; var Selected: Boolean);
begin
  if Assigned(FOnChanging) then FOnChanging(Self, Cell, Selected);
end;

procedure TCustomGridView.CheckClick(Cell: TGridCell);
begin
  if Assigned(FOnCheckClick) then
  begin
    FOnCheckClick(Self, Cell);
    { usually the state of the check box (and the cell text) changes
      in the event, so it needs to be redrawn }
    InvalidateCell(Cell);
  end;
end;

procedure TCustomGridView.ColumnResize(Column: Integer; var Width: Integer);
begin
  if Assigned(FOnColumnResize) then FOnColumnResize(Self, Column, Width);
end;

procedure TCustomGridView.ColumnResizing(Column: Integer; var Width: Integer);
begin
  if Assigned(FOnColumnResizing) then FOnColumnResizing(Self, Column, Width);
end;

procedure TCustomGridView.ColumnSizeToFit(Column: Integer; var Width: Integer);
begin
  if Assigned(FOnColumnSizeToFit) then FOnColumnSizeToFit(Self, Column, Width);
end;

function TCustomGridView.CreateColumn(Columns: TGridColumns): TCustomGridColumn;
begin
  Result := GetColumnClass.Create(Columns);
end;

function TCustomGridView.CreateColumns: TGridColumns;
begin
  Result := TGridColumns.Create(Self);
end;

function TCustomGridView.CreateEdit(EditClass: TGridEditClass): TCustomGridEdit;
begin
  { проверяем класс }
  if EditClass = nil then EditClass := TGridEdit;
  { создаем строку }
  Result := EditClass.Create(Self);
end;

function TCustomGridView.CreateFixed: TCustomGridFixed;
begin
  Result := TGridFixed.Create(Self);
end;

function TCustomGridView.CreateHeader: TCustomGridHeader;                 
begin
  Result := TGridHeader.Create(Self);
end;

function TCustomGridView.CreateHeaderSection(Sections: TGridHeaderSections): TGridHeaderSection;
begin
  Result := TGridHeaderSection.Create(Sections);
end;

procedure TCustomGridView.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  FlatBorders: array[Boolean] of DWORD = (WS_EX_CLIENTEDGE, WS_EX_STATICEDGE);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or FlatBorders[FFlatBorder];
    end;
  end;
end;

function TCustomGridView.CreateRows: TCustomGridRows;
begin
  Result := TGridRows.Create(Self);
end;

function TCustomGridView.CreateScrollBar(Kind: TScrollBarKind): TGridScrollBar;
begin
  Result := TGridScrollBar.Create(Self, Kind);
end;

procedure TCustomGridView.CreateWnd;
begin
  inherited;
  { window scroll bars must be updated after create window }
  FHorzScrollBar.Update;
  FVertScrollBar.Update;
end;

procedure TCustomGridView.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  FContextPopupCol := GetColumnAtX(MousePos.X);
  FContextPopupRow := GetRowAtY(MousePos.Y);
  inherited DoContextPopup(MousePos, Handled);
  if not Handled then DoHeaderPopup(MousePos, Handled);
end;

procedure TCustomGridView.DoExit;
begin
  ResetClickPos;
  { устанавливаем текст и гасим строку редактирования }
  if CancelOnExit then Editing := False;
  { обработка по умолчанию }
  inherited DoExit;
end;

procedure TCustomGridView.DoHeaderPopup(MousePos: TPoint; var Handled: Boolean);
var
  Menu: TPopupMenu;
  ClickEvent: TNotifyEvent;
  I: Integer;
  Column: TCustomGridColumn;
  Item: TMenuItem;
  //Monitor: TMonitor;
  //Rect: TRect;
  Pt: TPoint;
begin
  if ShowHeader and PtInRect(GetHeaderRect, MousePos) and
    (DefaultHeaderMenu or (Header.PopupMenu <> nil)) then
  begin
    if DefaultHeaderMenu then
    begin
      ClickEvent := HeaderMenuClick;
      if Header.PopupMenu <> nil then
      begin
        Menu := Header.PopupMenu;
        for I := Menu.Items.Count-1 downto 0 do
        begin
          Item := Menu.Items[I];
          if @Item.OnClick = @ClickEvent then Item.Free;
        end;
        if Menu.Items.Count <> 0 then
        begin
          Item := NewItem(cLineCaption, 0, True, True, ClickEvent, 0, '');
          Menu.Items.Add(Item);
        end;
      end
      else
      begin
        if FHeaderPopupMenu = nil then
        begin
          FHeaderPopupMenu := TPopupMenu.Create(Self);
          //FHeaderPopupMenu.AutoLineReduction := maManual;
        end;
        Menu := FHeaderPopupMenu;
        Menu.Items.Clear;
      end;
      { for each column add menu item }
      for I := 0 to Columns.Count-1 do
      begin
        Column := Columns[I];
        Item := NewItem(Column.Caption2, 0, Column.Visible, Column.DefaultPopup, ClickEvent, 0, '');
        Item.Tag := I;
        Menu.Items.Add(Item);
      end;
      { for OnHeaderDetailsClick event handler add "Details..." item }
      if Assigned(FOnHeaderDetailsClick) then
      begin
        if Menu.Items.Count <> 0 then
        begin
          Item := NewItem(cLineCaption, 0, True, True, ClickEvent, 0, '');
          Menu.Items.Add(Item);
        end;
        Item := NewItem('&Details...', 0, False, Columns.Count <> 0, ClickEvent, 0, '');
        Item.Tag := -1;
        Menu.Items.Add(Item);
      end;
      { if there are many columns, and the screen resolution is small,
        then the menu will not fully fit on the screen, so we will divide
        the menu into several columns for which we need to calculate,
        how many menu items fit on the current monitor }
      if Menu.Items.Count <> 0 then
      begin
        //Monitor := Screen.MonitorFromPoint(ClientToScreen(MousePos));
        //if Monitor <> nil then Rect := Monitor.WorkareaRect
        //else Rect := Screen.WorkareaRect;
        //H := GetSystemMetrics(SM_CYMENUSIZE);
        //if H = 0 then N := 1024 else N := (Rect.Bottom - Rect.Top) div H;
        //for I := 0 to Menu.Items.Count-1 do
        //begin
        //  Item := Menu.Items[I];
        //  if (I > 1) and (I mod N = 0) then Item.Break := mbBarBreak
        //  else Item.Break := mbNone;
        //end;
      end
      else
        Menu := nil;
    end
    else
      Menu := Header.PopupMenu;
    if Menu <> nil then
    begin
      //SendCancelMode(Self);
      Menu.PopupComponent := Self;
      Pt := ClientToScreen(MousePos);
      Menu.Popup(Pt.X, Pt.Y);
      Handled := True;
    end;
  end;
end;

function TCustomGridView.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if (not Result) and (gkMouseWheel in CursorKeys) then
  begin
    if not (ssShift in Shift) then
      SetGridCursor(GetCursorCell(CellFocused, goDown), True, True)
    else if not RowSelect then
      SetGridCursor(GetCursorCell(CellFocused, goRight), True, True)
    else
      with HorzScrollBar do Position := Position + LineStep * LineSize;
    Result := True;
  end;
end;

function TCustomGridView.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if (not Result) and (gkMouseWheel in CursorKeys) then
  begin
    if not (ssShift in Shift) then
      SetGridCursor(GetCursorCell(CellFocused, goUp), True, True)
    else if not RowSelect then
      SetGridCursor(GetCursorCell(CellFocused, goLeft), True, True)
    else
      with HorzScrollBar do Position := Position - LineStep * LineSize;
    Result := True;
  end;
end;

procedure TCustomGridView.DoStartDrag(var DragObject: TDragObject);
begin
  KillTimer(Handle, 1);
  inherited;
end;

procedure TCustomGridView.DoTextNotFound(const FindText: string);
var
  S, Caption: string;
  Form: TCustomForm;
begin
  if Assigned(FOnTextNotFound) then
    FOnTextNotFound(Self, FindText)
  else
  begin
    S := Format('Can not find "%s".', [FindText]);
    Form := GetParentForm(Self);
    if Form <> nil then Caption := Form.Caption
    else Caption := Application.Title;
    Application.MessageBox(PChar(S), PChar(Caption), MB_ICONINFORMATION);
  end;
end;

procedure TCustomGridView.EditButtonPress(Cell: TGridCell);
begin
  if Assigned(FOnEditButtonPress) then FOnEditButtonPress(Self, Cell);
end;

procedure TCustomGridView.EditCanceled(Cell: TGridCell);
begin
  if Assigned(FOnEditCanceled) then FOnEditCanceled(Self, Cell);
end;

function TCustomGridView.EditCanModify(Cell: TGridCell): Boolean;
begin
  Result := not IsCellReadOnly(Cell);
  if Assigned(FOnEditCanModify) then FOnEditCanModify(Self, Cell, Result);
end;

function TCustomGridView.EditCanAcceptKey(Cell: TGridCell; Key: Char): Boolean;
begin
  Result := IsCellValid(Cell);
  if Assigned(FOnEditAcceptKey) then FOnEditAcceptKey(Self, Cell, Key, Result);
end;

function TCustomGridView.EditCanShow(Cell: TGridCell): Boolean;
begin
  { проверяем режим дизайна и загрузки }
  if [csReading, csLoading, csDesigning, csDestroying] * ComponentState <> [] then
    Exit( False );
  { а есть ли ячейки }
  if (Columns.Count - Fixed.Count = 0) or (Rows.Count = 0) then
    Exit( False );
  { результат }
  Result := HandleAllocated and AllowEdit and (AlwaysEdit or IsActiveControl);
  { разрешение колонки }
  if (Cell.Col >= Fixed.Count) and (Cell.Col < Columns.Count) then
    Result := Result and Columns[Cell.Col].AllowEdit;
  { событие пользователя }
  if Result and Assigned(FOnEditCanShow) then
    FOnEditCanShow(Self, Cell, Result); 
end;

function TCustomGridView.EditCanUndo(Cell: TGridCell): Boolean;
begin
  Result := EditCanModify(Cell);
end;

procedure TCustomGridView.EditChange(Cell: TGridCell);
begin
  if Assigned(FOnEditChange) then FOnEditChange(Self, Cell);
end;

procedure TCustomGridView.EditCloseUp(Cell: TGridCell; Items: TStrings;
  ItemIndex: Integer; var ItemText: string; var Accept: Boolean);
begin
  if Assigned(FOnEditCloseUp) then
    FOnEditCloseUp(Self, Cell, ItemIndex, Accept);
  if Assigned(FOnEditCloseUpEx) then
    FOnEditCloseUpEx(Self, Cell, Items, ItemIndex, ItemText, Accept);
end;

procedure TCustomGridView.EditSelectNext(Cell: TGridCell; var Value: string);
begin
  if Assigned(FOnEditSelectNext) then FOnEditSelectNext(Self, Cell, Value);
end;

procedure TCustomGridView.GetCellColors(Cell: TGridCell; Canvas: TCanvas);
begin
  if Cell.Col < Fixed.Count then
  begin
    Canvas.Font := Fixed.Font;
    Canvas.Brush.Color := Fixed.Color;
    { if the button face color is used for a fixed cell, then with themes
      enabled it should be slightly discolored to match the highlighted
      row with the HighlightFocusRow turned on }
    if (Fixed.Color = clBtnFace) and ThemeServices.ThemesEnabled then
      Canvas.Brush.Color := GetLightenColor(Canvas.Brush.Color, 8);
    { highlight every other row }
    if Fixed.GridColor and ThemeServices.ThemesEnabled and
      HighlightEvenRows and IsEvenRow(Cell) then
        Canvas.Brush.Color := GetLightenColor(Canvas.Brush.Color, -8);
    { highlight row with focused cell }
    if Fixed.GridColor and HighlightFocusRow then
      if (Cell.Row = CellFocused.Row) and ((Cell.Col <> CellFocused.Col) or not Editing) then
        if ThemeServices.ThemesEnabled then
          Canvas.Brush.Color := GetLightenColor(clBtnFace, 8)
        else
          Canvas.Brush.Color := clBtnFace;
    { set gray text color for read-only cell }
    if GrayReadOnly and IsCellReadOnly(Cell) then
      Canvas.Font.Color := clGrayText;
  end
  else
  begin
    Canvas.Brush.Color := Self.Color;
    Canvas.Font := Self.Font;
    { set gray text color for disabled and read-only cells }
    if not Enabled then
      Canvas.Font.Color := clGrayText
    else if GrayReadOnly and IsCellReadOnly(Cell) then
      Canvas.Font.Color := clGrayText;
    { focused cell }
    if Enabled and IsCellHighlighted(Cell) and (not IsCellEditing(Cell)) then
    begin
      if Focused or EditFocused then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end
      else if not HideSelection then
      begin
        Canvas.Brush.Color := clBtnFace;
        Canvas.Font.Color := Font.Color;
      end;
    end
    else
    begin
      { highlight every other row without windows theme }
      if ThemeServices.ThemesEnabled then
        if HighlightEvenRows and IsEvenRow(Cell) then
          Canvas.Brush.Color := GetLightenColor(Canvas.Brush.Color, -8);
      { highlight row with focused cell }
      if HighlightFocusRow then
        if (Cell.Row = CellFocused.Row) and ((Cell.Col <> CellFocused.Col) or not Editing) then
          if ThemeServices.ThemesEnabled then
            Canvas.Brush.Color := GetLightenColor(clBtnFace, 8)
          else
            Canvas.Brush.Color := clBtnFace;
    end;
  end;
  if Assigned(FOnGetCellColors) then FOnGetCellColors(Self, Cell, Canvas);
end;

function TCustomGridView.GetCellImage(Cell: TGridCell; out OverlayIndex: Integer): Integer;
begin
  OverlayIndex := -1;
  Result := -1;
  if not Assigned(Images) then Exit;
  { only the first column has default image }
  if Cell.Col = GetFirstImageColumn then Result := ImageIndexDef;
  if Assigned(FOnGetCellImage) then FOnGetCellImage(Self, Cell, Result);
  if Assigned(FOnGetCellImageEx) then FOnGetCellImageEx(Self, Cell, Result, OverlayIndex);
end;

function TCustomGridView.GetCellImageIndent(Cell: TGridCell): TPoint;
begin
  Result.X := ImageLeftIndent;
  Result.Y := ImageTopIndent;
  { учитываем 3D эффект }
  if GridLines and (Fixed.Count > 0) and (not Fixed.Flat) and
    (not ThemeServices.ThemesEnabled) then Inc(Result.Y, 1);
  { событие пользователя }
  if Assigned(FOnGetCellImageIndent) then FOnGetCellImageIndent(Self, Cell, Result);
end;

function TCustomGridView.GetCellImageRect(Cell: TGridCell): TRect;
var
  II: TPoint;
  R: TRect;
begin
  { а есть ли картинка }
  if not IsCellHasImage(Cell) then
    Exit( Classes.Rect(0, 0, 0, 0) );
  { получаем прямоугольник ячейки }
  R := GetCellRect(Cell);
  { учитываем флажок }
  if IsCellHasCheck(Cell) then
    Inc(R.Left, CheckWidth + GetCheckIndent(Cell).X);
  { прямоугольник картинки }
  with Result do
  begin
    II := GetCellImageIndent(Cell);
    Left := R.Left + II.X;
    Right := Min(Left + Images.Width, R.Right);
    Top := R.Top + II.Y;
    Bottom := R.Top + Images.Height;
  end;
end;

function TCustomGridView.GetCellHintRect(Cell: TGridCell): TRect;
begin
  Result := GetEditRect(Cell);
  if Assigned(FOnGetCellHintRect) then FOnGetCellHintRect(Self, Cell, Result);
end;

function TCustomGridView.GetCellText(Cell: TGridCell): string;
begin
  Result := '';
  if Assigned(FOnGetCellText) then FOnGetCellText(Self, Cell, Result);
end;

function TCustomGridView.GetCellTextBounds(Cell: TGridCell): TRect;
var
  R: TRect;
  TI: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
  T: string;
begin
  { проверяем колонку ячейки }
  if (Cell.Col < 0) or (Cell.Col > Columns.Count - 1) then
    Exit( Classes.Rect(0, 0, 0, 0) );
  { определяем цвета }
  if (Cell.Row >= 0) and (Cell.Row < Rows.Count) then
  begin
    GetCellColors(Cell, Canvas);
    TI := GetCellTextIndent(Cell);
    T := GetCellText(Cell);
  end;
  { параметры отрисовки }
  R := Classes.Rect(0, 0, 0, 0);
  if Columns[Cell.Col].WordWrap then
  begin
    R := GetEditRect(Cell);
    OffsetRect(R, -R.Left, -R.Top);
    R.Bottom := R.Top;
  end;
  A := Columns[Cell.Col].Alignment;
  WR := Columns[Cell.Col].WantReturns;
  WW := Columns[Cell.Col].WordWrap;
  { вычисляем прямоугольник текста }
  Result := GetTextRect(Canvas, R, TI.X, A, WR, WW, T);
  { устанавливаем левый верхний угол в (0, 0) }
  OffsetRect(Result, -Result.Left, -Result.Top);
end;

function TCustomGridView.GetCellTextIndent(Cell: TGridCell): TPoint;
begin
  { значение по умолчанию }
  Result.X := TextLeftIndent;
  Result.Y := TextTopIndent;
  { учитываем картинки и 3D эффект }
  if IsCellHasCheck(Cell) or IsCellHasImage(Cell) then Result.X := 2;
  if GridLines and (Fixed.Count > 0) and (not Fixed.Flat) and
    (not ThemeServices.ThemesEnabled) then Inc(Result.Y, 1);
  { событие пользователя }
  if Assigned(FOnGetCellTextIndent) then FOnGetCellTextIndent(Self, Cell, Result);
end;

function TCustomGridView.GetCheckAlignment(Cell: TGridCell): TAlignment;
begin
  Result := taLeftJustify;
  if CheckBoxes and (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].CheckAlignment;
    if Assigned(FOnGetCheckAlignment) then FOnGetCheckAlignment(Self, Cell, Result);
  end;
end;

procedure TCustomGridView.GetCheckImage(Cell: TGridCell; CheckImage: Graphics.TBitmap);
begin
  if Assigned(FOnGetCheckImage) then FOnGetCheckImage(Self, Cell, CheckImage);
end;

function TCustomGridView.GetCheckIndent(Cell: TGridCell): TPoint;
begin
  Result.X := CheckLeftIndent;
  Result.Y := CheckTopIndent;
  { учитываем 3D эффект }
  if GridLines and (Fixed.Count > 0) and (not Fixed.Flat) and
    (not ThemeServices.ThemesEnabled) then Inc(Result.Y, 1);
  { учитываем выравнивание флажка }
  if GetCheckAlignment(Cell) = taCenter then Result.X := (Columns[Cell.Col].Width - CheckWidth) div 2 - 1;
  { событие пользователя }
  if Assigned(FOnGetCheckIndent) then FOnGetCheckIndent(Self, Cell, Result);
end;

function TCustomGridView.GetCheckKind(Cell: TGridCell): TGridCheckKind;
begin
  Result := gcNone;
  if CheckBoxes and (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].CheckKind;
    if Assigned(FOnGetCheckKind) then FOnGetCheckKind(Self, Cell, Result);
  end;
end;

function TCustomGridView.GetCheckRect(Cell: TGridCell): TRect;
var
  IC: TPoint;
  R: TRect;
begin
  { а есть ли флажок }
  if not IsCellHasCheck(Cell) then
    Exit( Classes.Rect(0, 0, 0, 0) );
  { получаем прямоугольник ячейки }
  R := GetCellRect(Cell);
  { прямоугольник флажка }
  with Result do
  begin
    IC := GetCheckIndent(Cell);
    Left := R.Left + IC.X;
    Right := Min(Left + CheckWidth, R.Right);
    Top := R.Top + IC.Y;
    Bottom := R.Top + CheckHeight;
  end;
end;

function TCustomGridView.GetCheckState(Cell: TGridCell): TCheckBoxState;
var
  Dummy: Boolean;
begin
  Result := GetCheckStateEx(Cell, Dummy);
end;

function TCustomGridView.GetCheckStateEx(Cell: TGridCell; out CheckEnabled: Boolean): TCheckBoxState;
begin
  CheckEnabled := True;
  Result := cbUnchecked;
  if Assigned(FOnGetCheckStateEx) then
    FOnGetCheckStateEx(Self, Cell, Result, CheckEnabled)
  else if Assigned(FOnGetCheckState) then
    FOnGetCheckState(Self, Cell, Result);
end;

function TCustomGridView.GetClientOrigin: TPoint;
begin
  if Parent = nil then
    Result := Classes.Point(0, 0) // GetClientRect.TopLeft
  else
    Result := inherited GetClientOrigin;
end;

function TCustomGridView.GetClientRect: TRect;
begin
  if (Parent = nil) or (not HandleAllocated) then
    Result := Bounds(0, 0, Width, Height)
  else
    Result := inherited GetClientRect;
end;

function TCustomGridView.GetColumnClass: TGridColumnClass;
begin
  Result := TGridColumn;
end;

function TCustomGridView.GetCursorCell(Cell: TGridCell; Offset: TGridCursorOffset): TGridCell;

  function DoMoveLeft(O: Integer): TGridCell;
  var
    I: Integer;
    C: TGridCell;
  begin
    { новая активная колонка }
    I := MaxIntValue([Cell.Col - O, Fixed.Count]);
    { перебираем колонки до фиксированных, пока не устанвится активная }
    while I >= Fixed.Count do
    begin
      C := GridCell(I, Cell.Row);
      { пытаемся установить курсор }
      if IsCellAcceptCursor(C) then
        Exit( C );
      { предыдущая колонка }
      Dec(I);
    end;
    { результат }
    Result := Cell;
  end;

  function DoMoveRight(O: Integer): TGridCell;
  var
    I: Integer;
    C: TGridCell;
  begin
    { новая активная колонка }
    I := MinIntValue([Cell.Col + O, Columns.Count-1]);
    { перебираем колонки до последней, пока не устанвится активная }
    while I <= Columns.Count-1 do
    begin
      C := GridCell(I, Cell.Row);
      { пытаемся установить курсор }
      if IsCellAcceptCursor(C) then
        Exit( C );
      { следующая колонка }
      Inc(I);
    end;
    { результат }
    Result := Cell;
  end;

  function DoMoveUp(O: Integer): TGridCell;
  var
    J: Integer;
    C: TGridCell;
  begin
    { новая активная строка }
    J := MaxIntValue([Cell.Row - O, 0]);
    { перебираем строки до первой, пока не устанвится активная }
    while J >= 0 do
    begin
      C := GridCell(Cell.Col, J);
      { пытаемся установить курсор }
      if IsCellAcceptCursor(C) then
        Exit( C );
      { предыдущая строка }
      Dec(J);
    end;
    { результат }
    Result := Cell;
  end;

  function DoMoveDown(O: Integer): TGridCell;
  var
    J: Integer;
    C: TGridCell;
  begin
    { новая активная строка }
    J := MinIntValue([Cell.Row + O, Rows.Count-1]);
    { перебираем строки до последней, пока не устанвится активная }
    while J <= Rows.Count-1 do
    begin
      C := GridCell(Cell.Col, J);
      { пытаемся установить курсор }
      if IsCellAcceptCursor(C) then
        Exit( C );
      { следующая строка }
      Inc(J);
    end;
    { результат }
    Result := Cell;
  end;

  function DoMoveHome: TGridCell;
  var
    C: TGridCell;
  begin
    C := Cell;
    try
      Cell.Col := Fixed.Count;
      Result := DoMoveRight(0);
    finally
      Cell := C;
    end;
  end;

  function DoMoveEnd: TGridCell;
  var
    C: TGridCell;
  begin
    C := Cell;
    try
      Cell.Col := Columns.Count - 1;
      Result := DoMoveLeft(0);
    finally
      Cell := C;
    end;
  end;

  function DoMoveGridHome: TGridCell;
  var
    I, J: Integer;
    C: TGridCell;
  begin
    { новая активная колонка }
    I := Fixed.Count;
    { перебираем колонки до текущей, пока не устанвится активная }
    while I <= Cell.Col do
    begin
      { новая активная строка }
      J := 0;
      { перебираем строки до текущей, пока не устанвится активная }
      while J <= Cell.Row do
      begin
        C := GridCell(I, J);
        { пытаемся установить курсор }
        if IsCellAcceptCursor(C) then
          Exit( C );
        { следующая строка }
        Inc(J);
      end;
      { следующая колонка }
      Inc(I);
    end;
    { результат }
    Result := Cell;
  end;

  function DoMoveGridEnd: TGridCell;
  var
    I, J: Integer;
    C: TGridCell;
  begin
    { новая активная колонка }
    I := Columns.Count - 1;
    { перебираем колонки до текущей, пока не устанвится активная }
    while I >= Cell.Col do
    begin
      J := Rows.Count - 1;
      { перебираем строки до текущей, пока не устанвится активная }
      while J >= Cell.Row do
      begin
        C := GridCell(I, J);
        { пытаемся установить курсор }
        if IsCellAcceptCursor(C) then
          Exit( C );
        { предыдущая строка }
        Dec(J);
      end;
      { предыдущая колонка }
      Dec(I);
    end;
    { результат }
    Result := Cell;
  end;

  function DoMoveGridTop: TGridCell;
  var
    J: Integer;
    C: TGridCell;
  begin
    J := 0;
    while J <= Cell.Row do
    begin
      C := GridCell(Cell.Col, J);
      if IsCellAcceptCursor(C) then
        Exit( C );
      Inc(J);
    end;
    Result := Cell;
  end;

  function DoMoveGridBottom: TGridCell;
  var
    J: Integer;
    C: TGridCell;
  begin
    J := Rows.Count - 1;
    while J >= Cell.Row do
    begin
      C := GridCell(Cell.Col, J);
      if IsCellAcceptCursor(C) then
        Exit( C );
      Dec(J);
    end;
    Result := Cell;
  end;

  function DoSelect: TGridCell;

    function DoSelectLeft: TGridCell;
    var
      I: Integer;
      C: TGridCell;
    begin
      I := MaxIntValue([Cell.Col, Fixed.Count]);
      { перебираем колонки до текущей, пока не устанвится активная }
      while I <= CellFocused.Col do
      begin
        C := GridCell(I, Cell.Row);
        { пытаемся установить курсор }
        if IsCellAcceptCursor(C) then
          Exit( C );
        { следующая колонка }
        Inc(I);
      end;
      { ячейка не найдена }
      Result := Cell;
    end;

    function DoSelectRight: TGridCell;
    var
      I: Integer;
      C: TGridCell;
    begin
      I := MinIntValue([Cell.Col, Columns.Count-1]);
      { перебираем колонки до текущей, пока не устанвится активная }
      while I >= CellFocused.Col do
      begin
        C := GridCell(I, Cell.Row);
        { пытаемся установить курсор }
        if IsCellAcceptCursor(C) then
          Exit( C );
        { предыдущая колонка }
        Dec(I);
      end;
      { ячейка не найдена }
      Result := Cell;
    end;

    function DoSelectUp: TGridCell;
    var
      J: Integer;
      C: TGridCell;
    begin
      J := MaxIntValue([Cell.Row, 0]);
      { перебираем строки до текущей, пока не устанвится активная }
      while J <= CellFocused.Row do
      begin
        C := GridCell(Cell.Col, J);
        { пытаемся установить курсор }
        if IsCellAcceptCursor(C) then
          Exit( C );
        { следующая строка }
        Inc(J);
      end;
      { ячейка не найдена }
      Result := Cell;
    end;

    function DoSelectDown: TGridCell;
    var
      J: Integer;
      C: TGridCell;
    begin
      J := MinIntValue([Cell.Row, Rows.Count-1]);
      { перебираем строки до текущей, пока не устанвится активная }
      while J >= CellFocused.Row do
      begin
        C := GridCell(Cell.Col, J);
        { пытаемся установить курсор }
        if IsCellAcceptCursor(C) then
          Exit( C );
        { предыдущая строка }
        Dec(J);
      end;
      { ячейка не найдена }
      Result := Cell;
    end;

  begin
    { а доступна ли указанная ячейка }
    if IsCellAcceptCursor(Cell) then
      Exit( Cell );
    { если выделение слева от курсора - ищем слева }
    if Cell.Col < CellFocused.Col then
    begin
      Result := DoSelectLeft;
      if IsCellAcceptCursor(Result) then
        Exit;
    end;
    { если выделение справа от курсора - ищем справа }
    if Cell.Col > CellFocused.Col then
    begin
      Result := DoSelectRight;
      if IsCellAcceptCursor(Result) then
        Exit;
    end;
    { если выделение над курсором - ищем сверху }
    if Cell.Row < CellFocused.Row then
    begin
      Result := DoSelectUp;
      if IsCellAcceptCursor(Result) then
        Exit;
    end;
    { выделение под курсором - ищем снизу }
    if Cell.Row > CellFocused.Row then
    begin
      Result := DoSelectDown;
      if IsCellAcceptCursor(Result) then
        Exit;
    end;
    { ничего не изменилось }
    Result := CellFocused;
  end;

  function DoFirst: TGridCell;
  var
    C: TGridCell;
    I, J: Integer;
  begin
    J := 0;
    { перебираем строки до текущей, пока не устанвится активная }
    while J <= Rows.Count-1 do
    begin
      I := Fixed.Count;
      { перебираем колонки до последней, пока не устанвится активная }
      while I <= Columns.Count-1 do
      begin
        C := GridCell(I, J);
        { пытаемся установить курсор }
        if IsCellAcceptCursor(C) then
          Exit( C );
        { следующая  колонка }
        Inc(I);
      end;
      { следующая строка }
      Inc(J);
    end;
    { результат по умолчанию }
    Result := CellFocused;
  end;

  function DoNext: TGridCell;
  var
    C: TGridCell;
    I, J: Integer;
  begin
    I := Cell.Col + 1;
    J := Cell.Row;
    { перебираем строки до последней, пока не устанвится активная }
    while J <= Rows.Count-1 do
    begin
      { перебираем колонки до последней, пока не устанвится активная }
      while I <= Columns.Count-1 do
      begin
        C := GridCell(I, J);
        { пытаемся установить курсор, учитываем построчное выделение }
        if IsCellAcceptCursor(C) and ((not RowSelect) or (C.Row <> Cell.Row)) then
          Exit( C );
        { следующая  колонка }
        Inc(I);
      end;
      { следующая строка с первой колонки }
      I := Fixed.Count;
      Inc(J);
    end;
    { результат по умолчанию }
    Result := CellFocused;
  end;

  function DoPrev: TGridCell;
  var
    C: TGridCell;
    I, J: Integer;
  begin
    I := Cell.Col - 1;
    J := Cell.Row;
    { перебираем строки до первой, пока не устанвится активная }
    while J >= 0 do
    begin
      { перебираем колонки до последней, пока не устанвится активная }
      while I >= Fixed.Count do
      begin
        C := GridCell(I, J);
        { пытаемся установить курсор, учитываем построчное выделение }
        if IsCellAcceptCursor(C) and ((not RowSelect) or (C.Row <> Cell.Row)) then
          Exit( C );
        { предыдущая колонка }
        Dec(I);
      end;
      { предыдущая строка с последней колонки }
      I := Columns.Count - 1;
      Dec(J);
    end;
    { результат по умолчанию }
    Result := CellFocused;
  end;

begin
  case Offset of
    goLeft: Result := DoMoveLeft(1); // смещение на колонку влево
    goRight: Result := DoMoveRight(1); // смещение вправо на одну колонку
    goUp: Result := DoMoveUp(1); // смещение вверх на одну колонку     
    goDown: Result := DoMoveDown(1); // смещение вниз на одну колонку      
    goPageUp: Result := DoMoveUp(VisSize.Row - 1); // смещение на страницу вверх      
    goPageDown: Result := DoMoveDown(VisSize.Row - 1); // смещение на страницу вниз      
    goHome: Result := DoMoveHome; // в начало строки      
    goEnd: Result := DoMoveEnd; // в конец строки      
    goGridHome: Result := DoMoveGridHome; // в начало таблицы      
    goGridEnd: Result := DoMoveGridEnd; // в конец таблицы      
    goGridTop: Result := DoMoveGridTop;
    goGridBottom: Result := DoMoveGridBottom;
    goSelect: Result := DoSelect; // проверка ячейки      
    goFirst: Result := DoFirst; // выбрать первую возможную ячейку      
    goNext: Result := DoNext; // выбрать следующую ячейку      
    goPrev: Result := DoPrev; // выбрать предыдущую возможную ячейку      
  //else    
  //  Result := Cell; // остальное игнорируем
  end;
end;

function TCustomGridView.GetEditClass: TGridEditClass;
begin
  Result := TGridEdit;
end;

procedure TCustomGridView.GetEditList(Cell: TGridCell; Items: TStrings);
begin
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    with Columns[Cell.Col] do
      if (EditStyle = gePickList) and (FPickList <> nil) then Items.Assign(FPickList);
    if Assigned(FOnGetEditList) then FOnGetEditList(Self, Cell, Items);
  end;
end;

procedure TCustomGridView.GetEditListBounds(Cell: TGridCell; var Rect: TRect);
begin
  if Assigned(FOnGetEditListBounds) then FOnGetEditListBounds(Self, Cell, Rect);
end;

function TCustomGridView.GetEditListIndex(Cell: TGridCell; Items: TStrings;
  const ItemText: string): Integer;
begin
  Result := -1;
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Items.IndexOf(ItemText);
    if Assigned(FOnGetEditListIndex) then
      FOnGetEditListIndex(Self, Cell, Items, ItemText, Result);
  end;
end;

function TCustomGridView.GetEditMask(Cell: TGridCell): string;
begin
  Result := '';
  { проверяем колонку }
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].EditMask;
    if Assigned(FOnGetEditMask) then FOnGetEditMask(Self, Cell, Result);
  end;
end;

function TCustomGridView.GetEditStyle(Cell: TGridCell): TGridEditStyle;
begin
  Result := geSimple;
  { проверяем колонку }
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].EditStyle;
    if Assigned(FOnGetEditStyle) then FOnGetEditStyle(Self, Cell, Result);
  end;
end;

function TCustomGridView.GetEditText(Cell: TGridCell): string;
begin
  Result := GetCellText(Cell);
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, Cell, Result);
end;

function TCustomGridView.GetFixedDividerColor: TColor;
begin
  Result := GetFixedGridColor;
end;

function TCustomGridView.GetFixedGridColor: TColor;
begin
  if Fixed.GridColor then Result := GetGridLineColor(Color)
{$IFDEF WINDOWS}
  else if CheckWin32Version(6, 0) then Result := cl3DLight
{$ENDIF}
  else Result := clActiveBorder;
end;

procedure TCustomGridView.GetHeaderColors(Section: TGridHeaderSection; Canvas: TCanvas);
begin
  { стандартные цвета }
  Canvas.Brush.Color := Header.Color;
  Canvas.Font := Header.Font;
  if not Enabled then Canvas.Font.Color := clGrayText;
  { событие пользователя }                                           
  if Assigned(FOnGetHeaderColors) then FOnGetHeaderColors(Self, Section, Canvas);
end;

function TCustomGridView.GetGridHint: string;
begin
  Result := FGridHint;
  if Assigned(FOnGetGridHint) then FOnGetGridHint(Self, Result); 
end;

function TCustomGridView.GetGridLineColor(BkColor: TColor): TColor;
begin
  if ThemeServices.ThemesEnabled then
    Result := GetLightenColor(Self.Color, -24)
  else
  begin
    Result := FGridColor;
    if ColorToRGB(Result) = ColorToRGB(BkColor) then Result := GetLightenColor(Result, -64);
  end;
  if Assigned(FOnGetGridColor) then FOnGetGridColor(Self, Result);
end;

function TCustomGridView.GetHeaderImage(Section: TGridHeaderSection): Integer;
begin
  { а есть ли картинки }
  if not Assigned(Header.Images) then
    Exit( -1 );
  { по умолчанию номер картинки - номер колонки }
  Result := Section.ColumnIndex;
  { событие пользователя }
  if Assigned(FOnGetHeaderImage) then FOnGetHeaderImage(Self, Section, Result);
end;

function TCustomGridView.GetSortArrowSize: TSize;
begin
  if ThemeServices.ThemesEnabled {and CheckWin32Version(6, 0)} then
  begin
    Result.cx := 13;
    Result.cy := 5;
  end
  else
  begin
    Result.cx := 8;
    Result.cy := 5;
  end;
end;

function TCustomGridView.GetSortDirection(Section: TGridHeaderSection): TGridSortDirection;
begin
  Result := gsNone;
  if Assigned(FOnGetSortDirection) then FOnGetSortDirection(Self, Section, Result);
end;

procedure TCustomGridView.GetSortImage(Section: TGridHeaderSection; SortImage: Graphics.TBitmap);
begin
  if Assigned(FOnGetSortImage) then FOnGetSortImage(Self, Section, SortImage);
end;

function TCustomGridView.GetTextRect(Canvas: TCanvas; Rect: TRect; LeftIndent: Integer;
  Alignment: TAlignment; WantReturns, WordWrap: Boolean; const Text: string): TRect;
var
  R: TRect;
  F, W, H: Integer;
begin
  { проверяем, как выводится текст: с помощью DrawTextEx или TextOut }
  { there are different ways to draw text: using DrawTextEx for multiline
    text and TextOut for single line text (see PaintText) }
  if WantReturns or WordWrap or EndEllipsis then
  begin
    { атрибуты текста }
    F := DT_NOPREFIX;
    { горизонтальное выравнивание }
    case Alignment of
      taLeftJustify: F := F or DT_LEFT;
      taCenter: F := F or DT_CENTER;
      taRightJustify: F := F or DT_RIGHT;
    end;
    { вертикальное выравнивание }
    if not (WantReturns or WordWrap) then
    begin
      { автоматическое выравнивание }
      F := F or DT_SINGLELINE or DT_VCENTER;
      { многоточие на конце не учитываем }
    end;
    { перенос слов }
    if WordWrap then F := F or DT_WORDBREAK;
    { вычисляем размеры текста }
    R := Rect;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), R, F or DT_CALCRECT);
    W := MaxIntValue([Rect.Right - Rect.Left, R.Right - R.Left + LeftIndent + TextRightIndent]);
    H := MaxIntValue([Rect.Bottom - Rect.Top, R.Bottom - R.Top]);
  end
  else
  begin
    { высота и ширина текста }
    W := MaxIntValue([Rect.Right - Rect.Left, Canvas.TextWidth(Text) + LeftIndent + TextRightIndent]);
    H := MaxIntValue([Rect.Bottom - Rect.Top, Canvas.TextHeight(Text)]);
  end;
  { формируем прямоугольник }
  case Alignment of
    taCenter:
      begin
        R.Left := Rect.Left - (W - (Rect.Right - Rect.Left)) div 2;
        R.Right := R.Left + W;
      end;
    taRightJustify:
      begin
        R.Right := Rect.Right;
        R.Left := R.Right - W;
      end;
  else
    R.Left := Rect.Left;
    R.Right := R.Left + W;
  end;
  R.Top := Rect.Top;
  R.Bottom := R.Top + H;
  { результат }
  Result := R;
end;

function TCustomGridView.GetTipsRect(Cell: TGridCell; const TipsText: string): TRect;
var
  R: TRect;
  TI: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
begin
  { проверяем ячейку }
  if not IsCellValid(Cell) then
    Exit( Classes.Rect(0, 0, 0, 0) );
  { вычисляем прямоугольник }
  with GetTipsWindowClass.Create(Self) do
  try
    GetCellColors(Cell, Canvas);
    { параметры отрисовки }
    R := GetEditRect(Cell);
    TI := GetCellTextIndent(Cell);
    A := Columns[Cell.Col].Alignment;
    WR := Pos(#13, TipsText) <> 0; // Columns[Cell.Col].WantReturns;
    WW := Columns[Cell.Col].WordWrap;
    { считаем прямоугольник }
    R := GetTextRect(Canvas, R, TI.X, A, WR, WW, TipsText);
  finally
    Free;
  end;
  if R.Bottom - R.Top > Rows.Height then
    { NOTE !! для текста, больше, чем высота строки - поправка !! }
    { correction if text height is greater than the height of the row }
    Inc(R.Bottom, TextTopIndent * 2);
  InflateRect(R, 1, 1); // <- border
  Result := R;
  if Assigned(FOnGetTipsRect) then FOnGetTipsRect(Self, Cell, Result);
end;

function TCustomGridView.GetTipsText(Cell: TGridCell): string;
begin
  Result := GetCellText(Cell);
  if Assigned(FOnGetTipsText) then FOnGetTipsText(Self, Cell, Result); 
end;

function TCustomGridView.GetTipsWindowClass: TGridTipsWindowClass;
begin
  Result := TGridTipsWindow;
end;

procedure TCustomGridView.HeaderClick(Section: TGridHeaderSection);
begin
  if Assigned(FOnHeaderClick) then FOnHeaderClick(Self, Section);
end;

procedure TCustomGridView.HeaderClicking(Section: TGridHeaderSection; var AllowClick: Boolean);
begin
  { по умолчанию можно нажимать только нижние секции }
  AllowClick := ColumnClick and Section.AllowClick and (Section.Sections.Count = 0);
  { событие пользователя }
  if Assigned(FOnHeaderClicking) then FOnHeaderClicking(Self, Section, AllowClick);
end;

procedure TCustomGridView.HeaderMenuClick(Sender: TObject);
var
  C: Integer;
begin
  if Sender is TComponent then
  begin
    C := TComponent(Sender).Tag;
    if (C >= 0) and (C < Columns.Count) then
      Columns[C].Visible := not Columns[C].Visible
    else if Assigned(FOnHeaderDetailsClick) then
      FOnHeaderDetailsClick(Self);
  end;
end;

procedure TCustomGridView.HideCursor;
begin
  if IsFocusAllowed then InvalidateFocus else HideEdit;
end;

procedure TCustomGridView.HideEdit;
begin
  if FEdit <> nil then
  begin
    FEditCell := GridCell(-1, -1);
    FEdit.Hide;
  end;
end;

procedure TCustomGridView.HideFocus;
begin
  if IsFocusAllowed then PaintFocus;
end;

procedure TCustomGridView.KeyDown(var Key: Word; Shift: TShiftState);
const
  HomeOffsets: array[Boolean] of TGridCursorOffset = (goHome, goGridHome);
  EndOffsets: array[Boolean] of TGridCursorOffset = (goEnd, goGridEnd);
  TabOffsets: array[Boolean] of TGridCursorOffset = (goNext, goPrev);
var
  Cell: TGridCell;
begin
  { событие - пользователю }
  inherited KeyDown(Key, Shift);
  { разбираем стрелки }
  if gkArrows in CursorKeys then
    case Key of
      VK_LEFT: // курсор влево
        begin
          SetGridCursor(GetCursorCell(CellFocused, goLeft), True, True);
          { если выделение построчное - скроллируем таблицу влево }
          if RowSelect then 
            with HorzScrollBar do SetPosition(Position - LineStep);
        end;
      VK_RIGHT: // курсор вправо
        begin
          SetGridCursor(GetCursorCell(CellFocused, goRight), True, True);
          { если выделение построчное - скроллируем таблицу вправо }
          if RowSelect then 
            with HorzScrollBar do SetPosition(Position + LineStep);
        end;
      VK_UP: // курсор вверх
        begin
          { если фокуса нет, то смещаем всю таблицу }
          if not AllowSelect then Cell := VisOrigin else Cell := CellFocused;
          { меняем выделенный }
          SetGridCursor(GetCursorCell(Cell, goUp), True, True);
        end;
      VK_DOWN: // курсор вниз
        begin
          { если фокуса нет, то смещаем всю таблицу }
          if not AllowSelect then
          begin
            Cell := GridCell(VisOrigin.Col, VisOrigin.Row + VisSize.Row - 1);
            if not IsCellVisible(Cell, False) then Dec(Cell.Row);
          end
          else
            Cell := CellFocused;
          { меняем выделенный }
          SetGridCursor(GetCursorCell(Cell, goDown), True, True)
        end;
      VK_PRIOR: // курсор на страницу вверх
        SetGridCursor(GetCursorCell(CellFocused, goPageUp), True, True);
      VK_NEXT: // курсор на страницу вниз
        SetGridCursor(GetCursorCell(CellFocused, goPageDown), True, True);
      VK_HOME: //курсор в начало строки или таблицы
        begin
          Cell := GetCursorCell(CellFocused, HomeOffsets[ssCtrl in Shift]);
          SetGridCursor(Cell, True, True);
        end;
      VK_END: // курсор в конец строки или таблицы
        begin
          Cell := GetCursorCell(CellFocused, EndOffsets[ssCtrl in Shift]);
          SetGridCursor(Cell, True, True);
        end;
    end;
  { курсор на следующую или предыдущую ячейку при нажатии TAB }
  if (gkTabs in CursorKeys) and (Key = VK_TAB) then
    SetGridCursor(GetCursorCell(CellFocused, TabOffsets[ssShift in Shift]), True, True);
  case Key of
    VK_SPACE:
      { нажат пробел - кликаем флажок }
      { if row selection is enabled then click check box of the first column }
      if CheckBoxes and (not EditCanShow(CellFocused) or (ssCtrl in Shift)) then
      begin
        Cell := CellFocused;
        if RowSelect then Cell.Col := Fixed.Count;
        if GetCheckKind(Cell) <> gcNone then
        begin
          SetGridCursor(Cell, True, True);
          CheckClick(Cell);
        end;
      end;
    VK_F2:
      Editing := True;
    VK_ADD:
      if Shift >= [ssCtrl, ssShift] then
      begin
        SizeAllColumnsToFit;
        Key := 0;
      end;
  end;
end;

procedure TCustomGridView.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  { ENTER: stop or start editing }
  if (Key = #13) and CellSelected then
  begin
    Key := #0;
    if Editing then
    begin
      { вставляем текст, гасим строку ввода }
      ApplyEdit; 
      { курсор на следующую ячейку }
      if gkReturn in CursorKeys then
        SetGridCursor(GetCursorCell(CellFocused, goNext), True, True);
    end
    else if not AlwaysEdit then
    begin
      { если строки ввода нет - показываем ее }
      SetGridCursor(CellFocused, True, True); {?}
      Editing := True;
    end;
  end;
  { нажат ESC - закрываем строку ввода }
  if Key = #27 then
  begin
    Key := #0;
    { проверяем редактирование }
    if Editing then
      { гасим строку или восстанавливаем значение }
      if not AlwaysEdit then
        CancelEdit
      else
        ResetEdit;
  end;
end;

procedure TCustomGridView.Loaded;
begin
  inherited Loaded;
  UpdateFixed;
  UpdateHeader;
  UpdateRows;
  UpdateColors;
  UpdateFonts;
  UpdateCursor;
  { ищем первую ячейку фокуса }
  FCellSelected := AlwaysSelected;
  { show inplace editor asynchronously to avoid access violation if grid data
    is created in the FormCreate event handler }
  if AlwaysSelected then PostMessage(Handle, LM_USER, 0, 0);
end;

procedure TCustomGridView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  S: TGridHeaderSection;
  C, P: TGridCell;
  AllowClicking: Boolean;
begin
  KillTimer(Handle, 1);
  { устанавливаем фокус на себя }
  if not AcquireFocus then
  begin
    MouseCapture := False;
    Exit;
  end;
  { проверяем нажатие на заголовок }
  if Button = mbLeft then
    { если заголовок виден и попали на него }
    if ShowHeader and PtInRect(GetHeaderRect, Classes.Point(X, Y)) then
    begin
      { попали ли мышкой на край секции заголовка }
      S := GetResizeSectionAt(X, Y);
      if S <> nil then
      begin
        if ssDouble in Shift then
          SizeColumnToFit(S.ResizeColumnIndex)
        else
          StartColResizeX(S, X);
      end
      { щелчок на секции пока только одинарный, потом, когда понадобится,
        можно сделать событие OnHeaderDblCkick }
      else if not (ssDouble in Shift) then
      begin
        { попали ли мышкой на заголовок }
        S := GetSectionAt(X, Y);
        if S <> nil then
        begin
          { на секцию нельзя нажимать если секция плоская или если так
            решит пользователь }
          if not Header.Flat then
          begin
            AllowClicking := True;
            { user can prevent the header from clicking by using the
              OnHeaderClick event }
            HeaderClicking(S, AllowClicking);
          end
          else
            AllowClicking := False;
          if AllowClicking then
            if Header.Flat then HeaderClick(S)
            else StartHeaderClick(S, X, Y);
        end;
      end;
      { щелкнули на заголовок - не пускаем дальше }
      Exit;
    end;
  { select new cell by mouse }
  { проверяем новую выделенную ячейку }
  if (Button = mbLeft) or ((Button = mbRight) and RightClickSelect) then
    { если можно выделять мышкой и попали на ячейки }
    if (gkMouse in CursorKeys) and (PtInRect(GetGridRect, Classes.Point(X, Y))) then
    begin
      C := GetCellAt(X, Y);
      { сбрасываем ячейку последнего щелчка (из-за возможности Exit) }
      { reset position of pending editing }
      P := FClickPos;
      ResetClickPos;
      { смортим куда попали }
      { select cell or clear selection }
      if IsCellEmpty(C) then
      begin
        { никуда - гасим выделение курсора }
        Editing := False;
        SetGridCursor(CellFocused, False, False);
      end
      else
      begin
        { в ячейку - выделяем ее }
        SetGridCursor(C, True, True);
        CellClick(C, Shift, X, Y);
        { проверяем попадание на флажок }
        { check clicking on the check box }
        if PtInRect(GetCheckRect(C), Classes.Point(X, Y)) then
        begin
          CheckClick(C);
          Exit;
        end;
        { проверяем начало редактирования (толко левой кнопкой) }
        if (Button = mbLeft) and IsCellEqual(C, CellFocused) and AllowEdit then
          { редактирование по двойному или повторному щелчку на одной и
            той же ячейке }
          { editing by repeated single click on the same cell is controlled
            with a timer, so as not to confuse the double click and single
            click }
          if (Shift * [ssCtrl, ssShift, ssDouble] = []) and IsCellEqual(C, P) then
            FEditPending := True;
      end;
      { запоминаем позицию последнего щелчка }
      FClickPos := C;
    end;
  { правая клавиша }
  { abort actions }
  if Button = mbRight then
  begin
    { если идет изменение размера - прекратить }
    if FColResizing then
    begin
      { прекращаем изменение }
      StopColResize(True);
      { не пускаем дальше }
      Exit;
    end;
    { если идет нажатие на заголовок - прекратить }
    if FHeaderClicking then
    begin
      { прекращаем изменение }
      StopHeaderClick(True);
      { не пускаем дальше }
      Exit;
    end;
  end;
  { обработчик по умолчанию }
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomGridView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  C: TGridCell;
  S: TGridHeaderSection;
  AllowClicking: Boolean;
begin
  // перенесено из обработчика LM_SETCURSOR (там глючило)
  if not (csDesigning in ComponentState) then
  begin
    { идет ли изменение размера колонки }
    if FColResizing or
    { проверяем попадание на резделительную линию заголовка }
      (ShowHeader and
       PtInRect(GetHeaderRect, Classes.Point(X, Y)) and
       (GetResizeSectionAt(X, Y) <> nil)) then
      Cursor := crHSplit
    else
      Cursor := crArrow;
  end;
  { highlight "hot" header section }
  if ThemeServices.ThemesEnabled and ShowHeader and (not Header.Flat) then
  begin
    S := nil;
    if FColResizing then S := FColResizeSection
    else if PtInRect(GetHeaderRect, Classes.Point(X, Y)) then
    begin
      S := GetResizeSectionAt(X, Y);
      if S = nil then S := GetSectionAt(X, Y);
    end;
    if S <> nil then
    begin
      AllowClicking := True;
      HeaderClicking(S, AllowClicking);
      if not AllowClicking then S := nil;
    end;
    if FHotSection <> S then
    begin
      { the user can delete section at runtime, and the FHotSection pointer
        will become invalid, so you need to use a column and a section level
        instead of FHotSection to redraw }
      if FHotSection <> nil then
        InvalidateSection(FHotColumn, FHotLevel);
      FHotSection := S;
      if FHotSection <> nil then
      begin
        FHotColumn := FHotSection.ColumnIndex;
        FHotLevel := FHotSection.Level;
        InvalidateSection(FHotColumn, FHotLevel);
      end;
    end;
  end;
  if FColResizing then
  begin
    StepColResizeX(X);
    Exit;
  end;
  if FHeaderClicking then
  begin
    StepHeaderClick(X, Y);
    Exit;
  end;
  if (ssLeft in Shift) or ((ssRight in Shift) and RightClickSelect) then
    if gkMouseMove in CursorKeys then
    begin
      C := GetCellAt(X, Y);
      if (not IsCellEmpty(C)) and (not IsCellEqual(C, CellFocused)) then
      begin
        SetGridCursor(C, True, True);
        if IsCellEqual(C, CellFocused) and AlwaysEdit then
        begin
          Editing := True;
          if Editing then Exit;
        end;
      end;
    end;
  { abort pending editing on mouse move }
  if FEditPending then
  begin
    FEditPending := False;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomGridView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  { идет ли изменение размера колонки }
  if FColResizing then
  begin
    { заканчиваем изменение }
    StopColResize(False);
    { не пускаем дальше }
    Exit;
  end;
  { идет ли нажатие на заголовок }
  if FHeaderClicking then
  begin
    { заканчиваем нажатие }
    StopHeaderClick(False);
    { не пускаем дальше }
    Exit;
  end;
  { delayed start editing }
  if FEditPending and IsCellEqual(FClickPos, CellFocused) then
  begin
    FEditPending := False;
    if SetTimer(Handle, 1, GetDoubleClickTime, nil) = 0 then Editing := True;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomGridView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then FImages := nil;
    if Header <> nil then
    begin
      if AComponent = Header.FImages then Header.FImages := nil;
      if AComponent = Header.FPopupMenu then Header.FPopupMenu := nil;
    end;
    if AComponent = FEdit then
    begin
      FEdit := nil;
      FEditCell := GridCell(-1, -1);
      FEditing := False;
    end;
    if AComponent = FHeaderPopupMenu then FHeaderPopupMenu := nil;
    if AComponent = FFindDialog then FFindDialog := nil;
  end;
end;

procedure TCustomGridView.Paint;
var
  DefDraw: Boolean;
  R: TRect;
  S: string;
begin
  { отрисовка пользователя }
  DefDraw := True;
  try
    if Assigned(FOnDraw) then FOnDraw(Self, DefDraw);
  except
    Application.HandleException(Self);
  end;
  { нужна ли отрисовка по умолчанию }
  if not DefDraw then
    Exit;
  { заголовок }
  if ShowHeader and RectVisible(Canvas.Handle, GetHeaderRect) then
  begin
    { фиксированная часть }
    PaintHeaders(True);
    { отсекаем прямоугольник фиксированного заголовка }
    R := GetHeaderRect;
    R.Right := GetFixedRect.Right;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    { обычная часть }
    PaintHeaders(False);
    { отсекаем прямоугольник заголовка }
    R := GetHeaderRect;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
  end;
  { поле справа и снизу }
  PaintFreeField;
  { фиксированные ячейки }
  if (Fixed.Count > 0) and RectVisible(Canvas.Handle, GetFixedRect) then
  begin
    { ячейки }
    PaintFixed;
    if GridLines then PaintFixedGridLines;
    { отсекаем прямоугольник фиксированных }
    R := GetFixedRect;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
  end;
  { обычные ячейки }
  if (VisSize.Col > 0) and (VisSize.Row > 0) then
  begin
    { ячейки }
//  if Editing then
//  begin
//    { отсекаем прямоугольник строки редактирования }
//    R := GetEditRect(EditCell);
//    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
//  end;
    PaintCells;
    { прямоугольник фокуса }
    if IsFocusAllowed then PaintFocus;
  end;
  { сетка }
  { grid lines over the cells }
  if GridLines then PaintGridLines;
  { hint for empty grid }
  if IsGridHintVisible then
  begin
    R := GetGridRect;
    R.Left := GetColumnLeftRight(Fixed.Count).Left;
    S := GetGridHint;
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := GridHintColor;
    Canvas.TextOut(R.Left + 3, R.Top + 4, S);
  end;
  { линия изменения ширины столбца }
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then 
    PaintResizeLine;
end;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  end;
end;

procedure TCustomGridView.Paint3DFrame(Rect: TRect; SideFlags: Longint);
begin
  with Canvas do
  begin
    DrawEdge(Handle, Rect, BDR_RAISEDOUTER, SideFlags and (not BF_TOPLEFT));
    if SideFlags and BF_BOTTOM <> 0 then Dec(Rect.Bottom);
    if SideFlags and BF_RIGHT <> 0 then Dec(Rect.Right);
    DrawEdge(Handle, Rect, BDR_RAISEDINNER, SideFlags and (not BF_BOTTOMRIGHT));
    if SideFlags and BF_TOP <> 0 then Inc(Rect.Top);
    if SideFlags and BF_LEFT <> 0 then Inc(Rect.Left);
    DrawEdge(Handle, Rect, BDR_RAISEDINNER, SideFlags and (not BF_TOPLEFT));
    if SideFlags and BF_BOTTOM <> 0 then Dec(Rect.Bottom);
    if SideFlags and BF_RIGHT <> 0 then Dec(Rect.Right);
    DrawEdge(Handle, Rect, BDR_RAISEDOUTER, SideFlags and (not BF_BOTTOMRIGHT));
  end;
end;

procedure TCustomGridView.PaintCell(Cell: TGridCell; Rect: TRect);
var
  DefDraw: Boolean;
begin
  { устанавливаем цвета и шрифт ячейки }
  GetCellColors(Cell, Canvas);
  { отрисовка пользователя }
  DefDraw := True;
  try
    if Assigned(FOnDrawCell) then FOnDrawCell(Self, Cell, Rect, DefDraw);
  except
    Application.HandleException(Self);
  end;
  { нужна ли отрисовка по умолчанию }
  if DefDraw then DefaultDrawCell(Cell, Rect);
end;

procedure TCustomGridView.PaintCells;
var
  I, J: Integer;
  L, T, W: Integer;
  R: TRect;
  C: TGridCell;
begin
  { левая и верхняя краницы видимых ячеек }
  L := GetColumnLeftRight(VisOrigin.Col).Left;
  T := GetRowTopBottom(VisOrigin.Row).Top;
  { инициализируем верхнюю границу }
  R.Bottom := T;
  { перебираем строки }
  for J := 0 to FVisSize.Row - 1 do
  begin
    { смещаем прямоугольник по вертикали }
    R.Top := R.Bottom;
    R.Bottom := R.Bottom + Rows.Height;
    { инициализируем левую границу }
    R.Right := L;
    { пребираем колонки }
    for I := 0 to FVisSize.Col - 1 do
    begin
      { ячейка и ее ширина }
      C := GridCell(VisOrigin.Col + I, VisOrigin.Row + J);
      W := Columns[C.Col].Width;
      { рисуем только видимые ячейки }
      { hidden columns have zero width }
      if W > 0 then
      begin
        { смещаем прямоугольник по горизонтали }
        R.Left := R.Right;
        R.Right := R.Right + W;
        { рисуем ячейку }
        if RectVisible(Canvas.Handle, R) then PaintCell(C, R);
      end;
    end;
  end;
end;

procedure TCustomGridView.PaintCheck(Rect: TRect; CheckKind: TGridCheckKind;
  CheckState: TCheckBoxState; CheckEnabled: Boolean);
var
  CR: TRect;
  DrawState: UINT;
  Detail: TThemedButton;
begin
  with Canvas do
  begin
    CR := Rect;
    InflateRect(CR, -1, -1); Inc(CR.Left); Inc(CR.Top);
    if not ThemeServices.ThemesEnabled then
    begin
      if CheckKind = gcCheckBox then
      begin
        DrawState := DFCS_BUTTONCHECK;
        case CheckState of
          cbChecked: DrawState := DrawState or DFCS_CHECKED;
          cbGrayed:  DrawState := DrawState or DFCS_CHECKED or DFCS_BUTTON3STATE;
        else
          //
        end;
      end
      else
      begin
        DrawState := DFCS_BUTTONRADIO;
        if CheckState <> cbUnchecked then DrawState := DrawState or DFCS_CHECKED;
      end;
      if not CheckEnabled then DrawState := DrawState or DFCS_INACTIVE;
      if CheckStyle <> cs3D then DrawState := DrawState or DFCS_FLAT;
      DrawFrameControl(Handle, CR, DFC_BUTTON, DrawState);
    end
    else
    begin
      if CheckKind = gcCheckBox then
        if CheckEnabled then
          case CheckState of
            cbChecked: Detail := tbCheckBoxCheckedNormal;
            cbGrayed: Detail := tbCheckBoxMixedNormal;
          else
            Detail := tbCheckBoxUncheckedNormal;
          end
        else
          case CheckState of
            cbChecked: Detail := tbCheckBoxCheckedDisabled;
            cbGrayed: Detail := tbCheckBoxMixedDisabled;
          else
            Detail := tbCheckBoxUncheckedDisabled;
          end
      else
        if CheckEnabled then
          case CheckState of
            cbChecked: Detail := tbRadioButtonCheckedNormal;
            cbGrayed: Detail := tbRadioButtonCheckedNormal;
          else
            Detail := tbRadioButtonUncheckedNormal;
          end
        else
          case CheckState of
            cbChecked: Detail := tbRadioButtonCheckedDisabled;
            cbGrayed: Detail := tbRadioButtonCheckedDisabled;
          else
            Detail := tbRadioButtonUncheckedDisabled;
          end;
      with ThemeServices do
        DrawElement(Handle, GetElementDetails(Detail), CR);
    end
  end
end;

procedure TCustomGridView.PaintDotGridLines(Points: Pointer; Count: Integer);
var
  PtsArr: PPoint absolute Points;
  i: Integer;
  R: TRect;
begin
  PreparePatternBitmap(Canvas, GetGridLineColor(Color), False);
  try
    { рисуем линии }
    i := 0;
    while i+1 < Count do
    begin
      { координаты линии }
      R.TopLeft := PtsArr[i];
      Inc(i);
      R.BottomRight := PtsArr[i];
      Inc(i);
      { заливка не будет рисоваться, если ширина или высота прямоугольника
        нулевая }
      if (R.Left = R.Right) and (R.Top <> R.Bottom) then Inc(R.Right)
      else if (R.Left <> R.Right) and (R.Top = R.Bottom) then Inc(R.Bottom);
      { рисуем линию }
      Canvas.FillRect(R);
    end;
  finally
    PreparePatternBitmap(Canvas, GetGridLineColor(Color), True);
  end;
end;

procedure TCustomGridView.PaintFixed;
var
  I, J, W, Y: Integer;
  R: TRect;
  C: TGridCell;
begin
  { верхняя граница строк }
  R.Bottom := GetRowTopBottom(VisOrigin.Row).Top;
  { перебираем строки }
  for J := 0 to FVisSize.Row - 1 do
  begin
    R.Top := R.Bottom;
    R.Bottom := R.Bottom + Rows.Height;
    R.Right := GetGridRect.Left;
    { перебираем колонки }
    for I := 0 to Fixed.Count-1 do
    begin
      C := GridCell(I, VisOrigin.Row + J);
      W := Columns[C.Col].Width;
      if W > 0 then
      begin
        R.Left := R.Right;
        R.Right := R.Right + W;
        if RectVisible(Canvas.Handle, R) then PaintCell(C, R);
      end;
    end;
  end;
  { полоска справа }
  { vertical separator line on the right }
  if (Fixed.Flat or ThemeServices.ThemesEnabled) and
    (Fixed.ShowDivider or (gsFullVertLine in GridStyle)) then
  begin
    R := GetFixedRect;
    { if the colors of fixed cells and regular cells are the same - draw a
      separator from one line, otherwise draw a double line }
    if Fixed.GridColor or ThemeServices.ThemesEnabled then
    begin
      if not (gsDotLines in GridStyle) then
      begin
        Canvas.Pen.Color := GetFixedDividerColor;
        Canvas.Pen.Width := FGridLineWidth;
        Canvas.MoveTo(R.Right - 1, R.Bottom - 1);
        Canvas.LineTo(R.Right - 1, R.Top - 1);
      end
      else
      begin
        R.Left := R.Right - 1;
        PaintDotGridLines(@R, 2);
      end;
    end
    else
    begin
      Y := GetRowTopBottom(VisOrigin.Row + VisSize.Row).Top;
      with Canvas do
      begin
        Pen.Color := clBtnShadow;
        Pen.Width := 1;
        MoveTo(R.Right - 2, R.Top - 1);
        LineTo(R.Right - 2, Y);
        Pen.Color := clBtnHighlight;
        MoveTo(R.Right - 1, Y - 1);
        LineTo(R.Right - 1, R.Top - 1);
        Pen.Color := GetGridLineColor(Color);
        MoveTo(R.Right - 2, Y);
        LineTo(R.Right - 2, R.Bottom);
      end;
    end;
  end;
  { линия изменения ширины колонки }
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

procedure TCustomGridView.PaintFixedGridLines;
var
  Points: array of TPoint = [];
  PointCount: Integer;
  StrokeList: array of DWORD = [];
  StrokeCount: Integer;
  I, L, R, T, B, X, Y, C, W: Integer;
  Rect: TRect;

  procedure ShiftGridPoints(DX, DY: Integer);
  var
    I: Integer;
  begin
    I := 0;
    while I < (Fixed.Count - 1) * Ord(gsVertLine in GridStyle) * 2 do
    begin
      Points[I].X := Points[I].X + DX;
      Inc(I);
    end;
    while I < PointCount do
    begin
      Points[I].Y := Points[I].Y + DY;
      Inc(I);
    end;
  end;

  procedure Paint3DCells(Rect: TRect);
  var
    I: Integer;
    R: TRect;
  begin
    R := Rect;
    R.Bottom := R.Top;
    while R.Bottom < Rect.Bottom do
    begin
      R.Top := R.Bottom;
      R.Bottom := R.Bottom + Rows.Height;
      R.Right := GetFixedRect.Left;
      for I := 0 to Fixed.Count-1 do
      begin
        W := Columns[I].Width;
        if W > 0 then
        begin
          R.Left := R.Right;
          R.Right := R.Right + W;
          if RectVisible(Canvas.Handle, R) then Paint3DFrame(R, BF_RECT);
        end;
      end;
    end;
  end;

  procedure PaintHorz3DLines(Rect: TRect);
  var
    R: TRect;
  begin
    R := Rect;
    R.Bottom := R.Top;
    repeat
      R.Top := R.Bottom;
      R.Bottom := R.Bottom + Rows.Height;
      if RectVisible(Canvas.Handle, R) then Paint3DFrame(R, BF_RECT);
    until R.Bottom >= Rect.Bottom;
  end;

  procedure PaintVert3DLines(Rect: TRect; DrawBottomLine: Boolean);
  const
    Flags: array[Boolean] of Longint = (BF_TOPLEFT or BF_RIGHT, BF_RECT);
  var
    I: Integer;
    R: TRect;
  begin
    R := Rect;
    R.Right := R.Left;
    for I := 0 to Fixed.Count-1 do
    begin
      W := Columns[I].Width;
      if W > 0 then
      begin
        R.Left := R.Right;
        R.Right := R.Right + W;
        if RectVisible(Canvas.Handle, R) then Paint3DFrame(R, Flags[DrawBottomLine]);
      end;
    end
  end;

  procedure PaintBottom3DMargin(Rect: TRect);
  begin
    if RectVisible(Canvas.Handle, Rect) then
      Paint3DFrame(Rect, BF_LEFT or BF_TOP or BF_RIGHT);
  end;

begin
  if ThemeServices.ThemesEnabled or Fixed.Flat then
  begin
    { the number of grid lines is equal to the number of visible rows plus
      one line to the right of each fixed column }
    StrokeCount := 0;
    if gsHorzLine in GridStyle then
    begin
      if gsListViewLike in GridStyle then StrokeCount := GetGridHeight div Rows.Height
      else StrokeCount := VisSize.Row;
    end;
    if gsVertLine in GridStyle then
      StrokeCount := StrokeCount + Fixed.Count;
    if StrokeCount > 0 then
    begin
      { malloc two points on each line }
      SetLength(Points, StrokeCount * 2);
      SetLength(StrokeList, StrokeCount);
      for I := 0 to StrokeCount-1 do StrokeList[I] := 2;
      { fill the points of vertical lines }
      Rect := GetFixedRect;
      PointCount := 0;
      if gsVertLine in GridStyle then
      begin
        T := Rect.Top;
        B := Rect.Bottom;
        if [gsFullVertLine, gsListViewLike] * GridStyle = [] then
          B := GetRowTopBottom(VisOrigin.Row + VisSize.Row).Top;
        X := Rect.Left;
        for I := 0 to Fixed.Count - 2 do
        begin
          X := X + Columns[I].Width;
          Points[PointCount].X := X - 2;
          Points[PointCount].Y := T;
          Inc(PointCount);
          Points[PointCount].X := X - 2;
          Points[PointCount].Y := B;
          Inc(PointCount);
        end;
      end;
      { fill the points of horisontal lines }
      if gsHorzLine in GridStyle then
      begin
        L := Rect.Left;
        R := Rect.Right;
        Y := GetRowTopBottom(VisOrigin.Row).Top;
        C := FVisSize.Row;
        if gsListViewLike in GridStyle then C := GetGridHeight div Rows.Height;
        for I := 0 to C - 1 do
        begin
          Y := Y + Rows.Height;
          Points[PointCount].X := L;
          Points[PointCount].Y := Y - 1;
          Inc(PointCount);
          Points[PointCount].X := R - 1;
          Points[PointCount].Y := Y - 1;
          Inc(PointCount);
        end;
      end;
      { if the color of the fixed cells does not differ from the color of
        the grid, then the grid lines are the same, with the themes enabled
        always draw single lines, with the themes turned off on a gray
        background we draw double lines }
      if Fixed.GridColor or ThemeServices.ThemesEnabled then
      begin
        { shift the lines (they are calculated for the double line) }
        ShiftGridPoints(1, 0);
        if not (gsDotLines in GridStyle) then
        begin
          Canvas.Pen.Color := GetFixedGridColor;
          Canvas.Pen.Width := FGridLineWidth;
          PolyPolyLine(Canvas.Handle, Points, StrokeList);
        end
        else
          PaintDotGridLines(Pointer(Points), PointCount);
      end
      else
      begin
        Canvas.Pen.Color := clBtnShadow;
        Canvas.Pen.Width := 1;
        PolyPolyLine(Canvas.Handle, Points, StrokeList);
        ShiftGridPoints(1, 1);
        Canvas.Pen.Color := clBtnHighlight;
        PolyPolyLine(Canvas.Handle, Points, StrokeList);
      end;
    end;
  end
  else if (gsHorzLine in GridStyle) and (gsVertLine in GridStyle) then
  begin
    { all cells are 3D }
    Rect := GetFixedRect;
    if not (gsListViewLike in GridStyle) then
      Rect.Bottom := Rect.Top + FVisSize.Row * Rows.Height;
    Paint3DCells(Rect);
  end
  else if (gsHorzLine in GridStyle) and (not (gsVertLine in GridStyle)) then
  begin
    { only horizontal 3D lines }
    Rect := GetFixedRect;
    if not (gsListViewLike in GridStyle) then
      Rect.Bottom := Rect.Top + FVisSize.Row * Rows.Height;
    PaintHorz3DLines(Rect);
    if not (gsListViewLike in GridStyle) then
    begin
      Rect.Top := Rect.Bottom;
      Rect.Bottom := GetFixedRect.Bottom;
      PaintBottom3DMargin(Rect);
    end;
  end
  else if (not (gsHorzLine in GridStyle)) and (gsVertLine in GridStyle) then
  begin
    { only vertical 3D lines }
    Rect := GetFixedRect;
    PaintVert3DLines(Rect, False);
  end
  else
  begin
    { no 3D lines }
    Rect := GetFixedRect;
    PaintBottom3DMargin(Rect);
  end;
end;

procedure TCustomGridView.PaintFocus;
var
  R, R2: TRect;
begin
  if ShowFocusRect and Focused and (VisSize.Row > 0) and (not Editing) and
    (UpdateLock = 0) then
  begin
    { clip the header and fixed cells (these are all around the cells to
      allow for the indicator from TDBGridView) }
    R := ClientRect;
    R2 := GetGridRect;
    R2.Left := R2.Left + GetFixedWidth;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R2.Top);
    ExcludeClipRect(Canvas.Handle, R.Left, R2.Top, R2.Left, R2.Bottom);
    ExcludeClipRect(Canvas.Handle, R.Left, R2.Bottom, R.Right, R.Bottom);
    ExcludeClipRect(Canvas.Handle, R2.Right, R2.Top, R.Right, R2.Bottom);
    { focus rectangle should not include grid lines }
    R := GetFocusRect;
    if GridLines then
    begin
      if gsVertLine in GridStyle then Dec(R.Right, FGridLineWidth);
      if gsHorzLine in GridStyle then Dec(R.Bottom, FGridLineWidth);
    end;
    { focus by default is displayed as a dotted rectangle }
    with Canvas do
    begin
      SetTextColor(Handle, ColorToRGB(clWhite));
      SetBkColor(Handle, ColorToRGB(clBlack));
      SetBkMode(Handle, OPAQUE);
      SetRop2(Handle, R2_COPYPEN);
      DrawFocusRect(R);
    end;
  end;
end;

procedure TCustomGridView.PaintFreeField;
var
  X, Y: Integer;
  R: TRect;
begin
  { поле справа от таблицы }
  X := GetColumnLeftRight(VisOrigin.Col + VisSize.Col).Left;
  R := GetGridRect;
  if X < R.Right then
  begin
    R.Left := X;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(R);
  end;
  { поле снизу от таблицы }
  Y := GetRowTopBottom(VisOrigin.Row + VisSize.Row).Top;
  R := GetGridRect;
  if Y < R.Bottom then
  begin
    R.Left := GetFixedRect.Right;
    R.Top := Y;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(R);
    { поле под фиксированными }
    R.Right := R.Left;
    R.Left := GetFixedRect.Left;
    Inc(R.Bottom, 2);
    if (gsListViewLike in GridStyle) then Canvas.Brush.Color := Fixed.Color;
    Canvas.FillRect(R);
  end;
  { линия изменения ширины столбца }
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

procedure TCustomGridView.PaintGridLines;
var
  Points: array of TPoint = [];
  PointCount: Integer;
  StrokeList: array of DWORD = [];
  StrokeCount: Integer;
  I: Integer;
  L, R, T, B, X, Y, C: Integer;
  Rect: TRect;
begin
  { the number of grid lines is equal to the number of visible rows plus
    one line to the right of each fixed column }
  StrokeCount := 0;
  if gsHorzLine in GridStyle then
  begin
    if gsListViewLike in GridStyle then StrokeCount := GetGridHeight div Rows.Height
    else StrokeCount := VisSize.Row;
  end;
  if gsVertLine in GridStyle then
    StrokeCount := StrokeCount + VisSize.Col;
  if StrokeCount > 0 then
  begin
    { malloc two points on each line }
    SetLength(Points, StrokeCount * 2);
    SetLength(StrokeList, StrokeCount);
    for I := 0 to StrokeCount-1 do StrokeList[I] := 2;
    { fill the points of vertical lines }
    Rect := GetGridRect;
    PointCount := 0;
    if gsVertLine in GridStyle then
    begin
      T := Rect.Top;
      B := Rect.Bottom;
      if [gsFullVertLine, gsListViewLike] * GridStyle = [] then
        B := GetRowTopBottom(VisOrigin.Row + VisSize.Row).Top;
      X := GetColumnLeftRight(VisOrigin.Col).Left;
      for I := 0 to VisSize.Col - 1 do
      begin
        X := X + Columns[VisOrigin.Col + I].Width;
        Points[PointCount].X := X - 1;
        Points[PointCount].Y := T;
        Inc(PointCount);
        Points[PointCount].X := X - 1;
        Points[PointCount].Y := B;
        Inc(PointCount);
      end;
    end;
    { fill the points of horisontal lines }
    if gsHorzLine in GridStyle then
    begin
      L := Rect.Left + GetFixedWidth;
      R := Rect.Right;
      if [gsFullHorzLine, gsListViewLike] * GridStyle = [] then
        R := GetColumnLeftRight(VisOrigin.Col + VisSize.Col).Left;
      Y := GetRowTopBottom(VisOrigin.Row).Top;
      C := VisSize.Row;
      if gsListViewLike in GridStyle then C := GetGridHeight div Rows.Height;
      for I := 0 to C - 1 do
      begin
        Y := Y + Rows.Height;
        Points[PointCount].X := L;
        Points[PointCount].Y := Y - 1;
        Inc(PointCount);
        Points[PointCount].X := R;
        Points[PointCount].Y := Y - 1;
        Inc(PointCount);
      end;
    end;
    if not (gsDotLines in GridStyle) then
    begin
      Canvas.Pen.Color := GetGridLineColor(Color);
      Canvas.Pen.Width := FGridLineWidth;
      PolyPolyLine(Canvas.Handle, Points, StrokeList);
    end
    else
      PaintDotGridLines(Pointer(Points), PointCount);
  end;
end;

procedure TCustomGridView.PaintHeader(Section: TGridHeaderSection; Rect: TRect);
var
  DefDraw: Boolean;
begin
  { устанавливаем цвет и шрифт секции }
  GetHeaderColors(Section, Canvas);
  { отрисовка пользователя }
  DefDraw := True;
  try
    if Assigned(FOnDrawHeader) then FOnDrawHeader(Self, Section, Rect, DefDraw);
  except
    Application.HandleException(Self);
  end;
  { нужна ли отрисовка по умолчанию }
  if DefDraw then DefaultDrawHeader(Section, Rect);
end;

procedure TCustomGridView.PaintHeaderBackground(Rect: TRect; Color: TColor; PaintState: TGridPaintStates);
// NOTE !! отрисовка в винде через ThemeServices глючит !!
{$IFDEF UNIX}
var
  Detail: TThemedHeader;
  Details: TThemedElementDetails;
{$ENDIF}
begin
  with Canvas do
  {$IFDEF UNIX}
    if not ThemeServices.ThemesEnabled then
  {$ENDIF}
    begin
      Brush.Color := Color;
      FillRect(Rect);
      if psFlat in PaintState then
      begin
        Pen.Width := 1;
        Pen.Color := clBtnShadow;
        MoveTo(Rect.Left, Rect.Bottom - 2);
        LineTo(Rect.Right - 1, Rect.Bottom - 2);
        Pen.Color := clBtnHighlight;
        MoveTo(Rect.Left, Rect.Bottom - 1);
        LineTo(Rect.Right - 1, Rect.Bottom - 1);
        Pen.Color := clBtnShadow;
        MoveTo(Rect.Right - 2, Rect.Top);
        LineTo(Rect.Right - 2, Rect.Bottom - 1);
        Pen.Color := clBtnHighlight;
        MoveTo(Rect.Right - 1, Rect.Top);
        LineTo(Rect.Right - 1, Rect.Bottom);
      end
      else if psPressed in PaintState then
        DrawEdge(Handle, Rect, BDR_SUNKENOUTER, BF_RECT or BF_FLAT)
      else
        Paint3DFrame(Rect, BF_RECT);
    end
  {$IFDEF UNIX}
    else
    begin
      //if (psSorted in PaintState) {and CheckWin32Version(6, 0)} then
      //begin
      //  Details := ThemeServices.GetElementDetails(thHeaderItemNormal);
      //  if psPressed in PaintState then Details.State := HIS_SORTEDPRESSED
      //  else if psHot in PaintState then Details.State := HIS_SORTEDHOT
      //  else Details.State := HIS_SORTEDNORMAL;
      //end
      //else
      begin
        if psPressed in PaintState then Detail := thHeaderItemPressed
        else if psHot in PaintState then Detail := thHeaderItemHot
        else if psDontCare in PaintState then Detail := thHeaderDontCare
        else Detail := thHeaderItemNormal;
        Details := ThemeServices.GetElementDetails(Detail);
      end;
      ThemeServices.DrawElement(Handle, Details, Rect);
    end;
  {$ENDIF}
end;

procedure TCustomGridView.PaintHeaders(DrawFixed: Boolean);
const
  PaintState: array[Boolean] of TGridPaintStates = ([psDontCare], [psFlat, psDontCare]);
var
  R: TRect;
begin
  { подзаголовки }
  PaintHeaderSections(Header.Sections, DrawFixed);
  { оставшееся место справа }
  R := GetHeaderRect;
  R.Left := GetGridRect.Left + Header.Width + GetGridOrigin.X;
  if R.Left < R.Right then
  begin
    Dec(R.Left, Ord(ThemeServices.ThemesEnabled)); // <- double-line artifact with themes enabled
    Inc(R.Right, 2);
    PaintHeaderBackground(R, Header.Color, PaintState[Header.Flat]);
  end;
  { серая полоска снизу }
  if Header.Flat and (not ThemeServices.ThemesEnabled) then
  begin
    { подправляем края прямоугольника }
    if DrawFixed then
    begin
      R.Left := GetFixedRect.Left;
      R.Right := GetFixedRect.Right;
    end
    else
    begin
      R.Left := GetFixedRect.Right;
      R.Right := GetGridRect.Right;
    end;
    { рисуем }
    with Canvas do
      { если цвета фиксированных и  таблицы совпадают - рисует обычную полоску }
      if Header.GridColor then
      begin
        Pen.Color := GetGridLineColor(Color);
        Pen.Width := FGridLineWidth;
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Right, R.Bottom - 1);
      end
      else
      { иначе рисуем двойную полоску }
      begin
        Pen.Color := clBtnShadow;
        Pen.Width := 1;
        MoveTo(R.Left, R.Bottom - 2);
        LineTo(R.Right, R.Bottom - 2);
        Pen.Color := clBtnHighlight;
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Right, R.Bottom - 1);
      end;
  end;
  { линия изменения ширины колонки }
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

procedure TCustomGridView.PaintHeaderSections(Sections: TGridHeaderSections; DrawFixed: Boolean);
var
  I: Integer;
  S: TGridHeaderSection;
  R, SR: TRect;
begin
  for I := 0 to Sections.Count-1 do
  begin
    S := Sections[I];
    { рисуем только секции указанной "фиксированности" }
    { the header sections are drawn in two stages: the sections of fixed cells
      and the section of regular cells }
    if DrawFixed = S.FixedColumn then
    begin
      R := S.BoundsRect;
      { не рисуем, секции ненулевой ширины }
      if R.Right > R.Left then
      begin
        { вычисляем прямоугольник секции }
        SR := R;
        if S.Sections.Count > 0 then SR.Bottom := GetHeaderRect.Bottom;
        { рисуем только те секции и поззаголовки, которые надо перерисовывать }
        if RectVisible(Canvas.Handle, SR) then
        begin
          PaintHeader(S, R);
          PaintHeaderSections(S.Sections, DrawFixed);
        end;
      end;
    end
    else
      { некторые общие заголовки могут иметь одновременно фиксированные и
        нефиксированные подзаголовки (хотя это и неправильно), поэтому
        попробуем отрисовать их тоже }
      PaintHeaderSections(S.Sections, DrawFixed);
  end;
end;

procedure TCustomGridView.PaintResizeLine;
var
  OldPen: TPen;
begin
  OldPen := TPen.Create;
  try
    with Canvas do
    begin
      OldPen.Assign(Pen);
      try
        Pen.Color := clWhite;
        Pen.Style := psSolid;
        Pen.Mode := pmXor;
        Pen.Width := 1;
        with FColResizeRect do
        begin
          MoveTo(FColResizePos, Top);
          LineTo(FColResizePos, Bottom);
        end;
      finally
        Pen := OldPen;
      end;
    end;
  finally
    OldPen.Free;
  end;
end;

procedure TCustomGridView.PaintText(Canvas: TCanvas; Rect: TRect; LeftIndent: Integer;
  Alignment: TAlignment; WantReturns, WordWrap: Boolean; const Text: string);
var
  ts: TTextStyle;
  DX: Integer;
begin
  { draw text using DrawTextEx for multiline text, draw text using TextOut
    for single line text }
  // переписано для использования Canvas.TextRect
  ts := Canvas.TextStyle;
  ts.Opaque := False;
  ts.Layout := tlCenter;
  ts.Alignment := Alignment;
  ts.Wordbreak := WordWrap;
  if WantReturns or WordWrap or EndEllipsis then
  begin
    ts.ShowPrefix := False;
    ts.SingleLine := False;
    ts.EndEllipsis := False;
    if not (WantReturns or WordWrap) then
    begin
      ts.SingleLine := True;
      { многоточие на конце }
      if Alignment = taLeftJustify then ts.EndEllipsis := True
    end;
    { отступ справа }
    Dec(Rect.Right, TextRightIndent);
    { выводим текст }
    SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.TextRect(Rect, Rect.Left + LeftIndent, Rect.Top {+ TopIndent}, Text, ts);
  end
  else
  begin
    { смещение по горизонтали }
    case Alignment of
      taCenter:       DX := LeftIndent + (Rect.Right - Rect.Left) div 2;
      taRightJustify: DX := (Rect.Right - Rect.Left) - TextRightIndent;
    else
      DX := LeftIndent;
    end;
    ts.Clipping := True;
    { стандартный вывод текста }
    SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.TextRect(Rect, Rect.Left + DX, Rect.Top {+ TopIndent}, Text, ts);
  end;
end;

procedure TCustomGridView.PreparePatternBitmap(Canvas: TCanvas; FillColor: TColor; Remove: Boolean);
begin
  if Remove then
  begin
    if Canvas.Brush.Bitmap = nil then
      Exit;
    Canvas.Brush.Bitmap := nil;
  end
  else
  begin
    if Canvas.Brush.Bitmap = FPatternBitmap then Exit;
    { Линии точечками рисуются с использованием точечной заливки. Формат
      заливки устанавливается сразу на весь Canvas относительно верхнего
      левого угла компонента. Поэтому, даже заливая линию, сдвинутую на 1
      пиксел от предыдущей, мы все равно получим чередующуюся заливку.
      При горизонтальном смещение таблицы на 1 пиксел старая сетка смещается,
      и чтобы новая сетка рисовалась также чередующейся со старой, необходимо
      сместить и формат заливки. }
    if HorzScrollBar.Position mod 2 = 0 then
    begin
      FPatternBitmap.Canvas.Pixels[0, 0] := Color;
      FPatternBitmap.Canvas.Pixels[1, 1] := Color;
      FPatternBitmap.Canvas.Pixels[0, 1] := FillColor;
      FPatternBitmap.Canvas.Pixels[1, 0] := FillColor;
    end
    else
    begin
      FPatternBitmap.Canvas.Pixels[0, 0] := FillColor;
      FPatternBitmap.Canvas.Pixels[1, 1] := FillColor;
      FPatternBitmap.Canvas.Pixels[0, 1] := Color;
      FPatternBitmap.Canvas.Pixels[1, 0] := Color;
    end;
    { устанавливаем заливку }
    Canvas.Brush.Bitmap := FPatternBitmap;
  end;
  { обновляем полотно }
  Canvas.Refresh;
end;

procedure TCustomGridView.ResetClickPos;
begin
  FClickPos := GridCell(-1, -1);
end;

procedure TCustomGridView.Resize;
begin
  if UpdateLock = 0 then UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateEdit(Editing);
  inherited Resize;
end;

procedure TCustomGridView.SetEditText(Cell: TGridCell; var Value: string);
begin
  if Assigned(FOnSetEditText) then FOnSetEditText(Self, Cell, Value);
end;

procedure TCustomGridView.ShowCursor;
begin
  if IsFocusAllowed then InvalidateFocus else ShowEdit;
end;

procedure TCustomGridView.ShowEdit;
begin
  UpdateEdit(True);
end;

procedure TCustomGridView.ShowEditChar(C: Char);
begin
  { показываем строку ввода }
  Editing := True;
  { вставляем символ }
  if (Edit <> nil) and Editing then
    PostMessage(Edit.Handle, LM_CHAR, Word(C), 0);
end;

procedure TCustomGridView.ShowFocus;
begin
  if IsFocusAllowed then PaintFocus;
end;

procedure TCustomGridView.SizeAllColumnsToFit;
var
  I: Integer;
  Column: TCustomGridColumn;
begin
  if ColumnsResize then
    for I := 0 to Columns.Count-1 do
    begin
      Column := Columns[I];
      if Column.Visible and not Column.FixedSize then SizeColumnToFit(I);
    end;
end;

procedure TCustomGridView.SizeColumnToFit(ColumnIndex: Integer);
var
  W: Integer;
begin
  if (ColumnIndex >= 0) and (ColumnIndex < Columns.Count) then
  begin
    W := Min(Columns[ColumnIndex].MaxWidth, GetColumnMaxWidth(ColumnIndex));
    ColumnSizeToFit(ColumnIndex, W);
    ColumnResize(ColumnIndex, W);
    FColResizing := True;
    try
      Columns[ColumnIndex].Width := W;
    finally
      FColResizing := False;
    end;
  end;
end;

procedure TCustomGridView.StartColResizeX(Section: TGridHeaderSection; X: Integer);
begin
  FColResizeSection := Section;
  FColResizeIndex := Section.ResizeColumnIndex;
  FColResizeLevel := Section.Level;
  { вычисляем граничный прямоугольник для изменения размера }
  with FColResizeSection do
  begin
    { горизонтальные границы }
    if FColResizeIndex <= Columns.Count - 1 then
    begin
      FColResizeRect := GetColumnRect(FColResizeIndex);
      FColResizeRect.Bottom := GetGridRect.Bottom;
      FColResizeMinWidth := Columns[FColResizeIndex].MinWidth;
      FColResizeMaxWidth := Columns[FColResizeIndex].MaxWidth;
    end
    else
    begin
      FColResizeRect := BoundsRect;
      FColResizeRect.Bottom := GetGridRect.Bottom;
      FColResizeMinWidth := 0;
      FColResizeMaxWidth := 10000;
    end;
    { вертикальне границы }
    FColResizeRect.Top := Level * Header.SectionHeight;
    FColResizeRect.Bottom := Height;
  end;
  { положение линии размера }
  FColResizePos := FColResizeRect.Right;
  FColResizeOffset := FColResizePos - X;
  { можно изменять размер колонки }
  FColResizeCount := 0;
  FColResizing := True;
  { захватываем мышку }
  MouseCapture := True;
end;

procedure TCustomGridView.StepColResizeX(X: Integer);
var
  W: Integer;
  R: TRect;
  S: TGridHeaderSection;
begin
  { а идёт ли изменение размера колонки }
  if FColResizing then
  begin
    { текущее положение линии }
    X := X + FColResizeOffset;
    { текущая ширина }
    W := X - FColResizeRect.Left;
    { подправляем ширину в соотвествии с границами }
    if W < FColResizeMinWidth then W := FColResizeMinWidth;
    if W > FColResizeMaxWidth then W := FColResizeMaxWidth;
    ColumnResizing(FColResizeIndex, W);
    { опять подправляем шщирину }
    if W < FColResizeMinWidth then W := FColResizeMinWidth;
    if W > FColResizeMaxWidth then W := FColResizeMaxWidth;
    { новое положение линии }
    X := FColResizeRect.Left + W;
    { проводим линию }
    if FColResizePos <> X then
    begin
      { закрашиваем старую линию }
      if (FColResizeCount > 0) and not FColumnsFullDrag then PaintResizeLine;
      Inc(FColResizeCount);
      { новое положение линии }
      FColResizePos := X;
      { устанавливаем ширину }
      if FColumnsFullDrag and (FColResizeIndex < Columns.Count) then
      begin
        { перед изменение ширины столбца вычисляем и обновляем изменяющуюся
          часть ячеек таблицы }
        { calculate and update the changing part of the grid cells }
        R := Default(TRect);
        UnionRect(R, GetHeaderRect, GetGridRect);
        R.Left := GetColumnLeftRight(FColResizeIndex).Left;
        if FColResizeIndex >= Fixed.Count then
        begin
          { если нефиксированная колонка частично закрыта фиксированными,
            то эту закрытую часть перерисовывать не надо }
          R.Left := MaxIntValue([R.Left, GetFixedRect.Right]);
        end;
        if (W < Columns[FColResizeIndex].Width) and
           (R.Right >= GetColumnLeftRight(Columns.Count - 1).Right) and
           (HorzScrollBar.Max - HorzScrollBar.Min > HorzScrollBar.PageStep) then
        begin
          { граничный случай: если горизонтальный скроллер в самом
            правом положении, то уменьшение ширины колонки приводит к
            сдвигу вправо всех нефиксированных колонок, расположенных
            слева от текущей }
          R.Left := GetFixedRect.Right;
          R.Right := GetColumnLeftRight(FColResizeIndex + 1).Left;
        end;
        InvalidateRect(R);
        { если у колонки многоуровневый заголовок, то дополнительно
          обновляем самую верхнюю секцию }
        S := GetHeaderSection(FColResizeIndex, 0);
        if S <> nil then
        begin
          R := S.BoundsRect;
          R.Bottom := GetHeaderRect.Bottom;
          InvalidateRect(R);
        end;
        { запрещаем перерисовку всей таблицы, устанавливаем новую ширину
          столбца  }
        LockUpdate;
        try
          Columns[FColResizeIndex].Width := W;
        finally
          { теперь перерисовываем (лучше сразу - так меньше моргает) }
          { update grid instead of invalidate (blinks less) }
          UnlockUpdate(False);
          Update;
        end;
      end
      else
        { рисуем новую линию }
        { hide the line in the new position }
        PaintResizeLine;
    end
    else
    begin
      { рисуем линию первый раз }
      { show the resize line }
      if (FColResizeCount = 0) and not FColumnsFullDrag then PaintResizeLine;
      Inc(FColResizeCount);
    end;
  end;
end;

procedure TCustomGridView.StopColResize(Abort: Boolean);
var
  W: Integer;
begin
  if FColResizing then
  try
    { освобождаем мышку }
    MouseCapture := False;
    { было ли хотябы одно перемещение }
    { a simple click on the right side of the section does not change the size }
    if FColResizeCount > 0 then
    begin
      { закрашиваем линию }
      if not FColumnsFullDrag then PaintResizeLine;
      { а не прервано ли изменение }
      if Abort then Exit;
      { устанавливаем размер колонки }
      with FColResizeSection do
      begin
        { новая ширина }
        W := FColResizePos - FColResizeRect.Left;
        { подправляем ширину в соотвествии с границами }
        if W < FColResizeMinWidth then W := FColResizeMinWidth;
        if W > FColResizeMaxWidth then W := FColResizeMaxWidth;
        { событие пользователя }
        ColumnResize(FColResizeIndex, W);
        { опять подправляем шщирину }
        if W < FColResizeMinWidth then W := FColResizeMinWidth;
        if W > FColResizeMaxWidth then W := FColResizeMaxWidth;
        { устанавливаем ширину }
        if FColResizeIndex < Columns.Count then
          Columns[FColResizeIndex].Width := W;
      end;
    end;
  finally
    FColResizing := False;
  end;
end;

procedure TCustomGridView.StartHeaderClick(Section: TGridHeaderSection; X, Y: Integer);
begin
  { запоминаем параметры }
  FHeaderClickSection := Section;
  FHeaderClickRect := Section.BoundsRect;
  FHeaderClickState := False;
  FHeaderClicking := True;
  { захватываем мышку }
  MouseCapture := True;
  { нажимаем кнопку }
  StepHeaderClick(X, Y);
end;

procedure TCustomGridView.StepHeaderClick(X, Y: Integer);
var
  P: Boolean;
begin
  { а едет ли нажатие на секцию }
  if FHeaderClicking then
  begin
    { определяем признак нажатия }
    P := PtInRect(FHeaderClickRect, Classes.Point(X, Y));
    { изменилось ли что-нибудь }
    if FHeaderClickState <> P then
    begin
      FHeaderClickState := P;
      InvalidateRect(FHeaderClickRect);
    end;
  end;
end;

procedure TCustomGridView.StopHeaderClick(Abort: Boolean);
var
  P: Boolean;
begin
  { а едет ли нажатие на секцию }
  if FHeaderClicking then
  begin
    P := FHeaderClickState;
    { отжимаем кнопку }
    StepHeaderClick(-1, -1);
    { завершаем нажатие, отпускаем мышку }
    FHeaderClicking := False;
    MouseCapture := False;
    { вызываем событие }
    if (not Abort) and P then HeaderClick(FHeaderClickSection);
  end;
end;

procedure TCustomGridView.ApplyEdit;
begin
  Editing := False;
end;

procedure TCustomGridView.ApplyEditText;
var
  EditFocused: Boolean;
  EditText: string;
begin
  if (not ReadOnly) and (Edit <> nil) and (not IsCellReadOnly(EditCell)) then
  begin
    EditFocused := Editing;
    { text input can be canceled by throwing an exception in the
      OnSetEditText event }
    try
      EditText := Edit.Text;
      try
        SetEditText(EditCell, EditText);
      finally
        Edit.Text := EditText;
      end;
    except
      on E: Exception do
      begin
        MakeCellVisible(CellFocused, False);
        { if the input line is visible, then put the focus on it, otherwise
          it will be hidden after opening the error message box }
        if EditFocused then Edit.SetFocus;
        raise;
      end;
    end;
  end;
end;

procedure TCustomGridView.CancelEdit;
var
  Cell: TGridCell;
begin
  if Editing then
  begin
    { запоминаем ячейку редактирвоания, т.к. она сбросится после
      окончания редактирования }
    Cell := EditCell;
    { гасим строку или восстанавливаем значение }
    if not AlwaysEdit then
    begin
      HideEdit;
      ChangeEditing;
    end
    else
      UpdateEditContents(False);
    { ввод текста отменен }
    EditCanceled(Cell);
  end;
end;

procedure TCustomGridView.DefaultDrawCell(Cell: TGridCell; Rect: TRect);
var
  DefRect: TRect;
  CK: TGridCheckKind;
  CS: TCheckBoxState;
  CE: Boolean;
  CI, OI, X, Y, W, H: Integer;
  R: TRect;
  RS, RH, CH, IH, SH: Boolean;
  IC, II, IT: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
  T: string;
begin
  DefRect := Rect;
  CK := GetCheckKind(Cell);
  CI := GetCellImage(Cell, OI);
  RS := IsRowHighlighted(Cell.Row);
  RH := RS and (Cell.Col = Fixed.Count) and (Cell.Row = CellFocused.Row);
  CH := (not RS) and IsCellEqual(Cell, CellFocused);
  SH := (Canvas.Brush.Color = clHighlight) or (Canvas.Brush.Color = clBtnFace); {?}
  IH := (not ImageHighlight) and (RH or CH) and SH;
  IC := GetCheckIndent(Cell);
  II := GetCellImageIndent(Cell);
  { cell background }
  R := Rect;
  if IH then
  begin
    if CK <> gcNone then Inc(R.Left, IC.X + CheckWidth);
    if CI <> -1 then Inc(R.Left, II.X + Images.Width);
  end;
  Canvas.FillRect(R);
  { draw check box }
  if CK <> gcNone then
  begin
    if IH then Canvas.Brush.Color := Color;
    R := Rect;
    R.Right := Min(R.Left + CheckWidth + IC.X, Rect.Right);
    if R.Left < Rect.Right then
    begin
      with Canvas do
      begin
        X := R.Left + IC.X;
        Y := R.Top + IC.Y;
        W := CheckWidth;
        if X + W > R.Right then W := R.Right - X;
        H := CheckHeight;
        if Y + H > R.Bottom then H := R.Bottom - Y;
        if CK <> gcUserDefine then
        begin
          CS := GetCheckStateEx(Cell, CE);
          PaintCheck(Bounds(X, Y, W, H), CK, CS, CE);
        end
        else
        begin
          FCheckBuffer.Width := W;
          FCheckBuffer.Height := H;
          { user can define own check image using OnGetCheckImage event }
          GetCheckImage(Cell, FCheckBuffer);
          if not (FCheckBuffer.Empty or (FCheckBuffer.Width < 1) or
            (FCheckBuffer.Height < 1)) then Draw(X, Y, FCheckBuffer);
        end;
      end;
      { exclude check rect from text rect }
      Rect.Left := R.Right;
    end;
  end;
  { cell image }
  if (CI <> -1) and (Images <> nil) then
  begin
    R := Rect;
    R.Right := Min(R.Left + Images.Width + II.X, Rect.Right);
    if R.Left < Rect.Right then
    begin
      X := R.Left + II.X;
      Y := R.Top + II.Y;
      W := Images.Width;
      if X + W > R.Right then W := R.Right - X;
      H := Images.Height;
      if Y + H > R.Bottom then H := R.Bottom - Y;
      { exclude image rect from text rect }
      Rect.Left := R.Right;
    end;
  end;
  { cell text }
  if not (IsCellEqual(Cell, FEditCell) and (not IsFocusAllowed)) and
    (Rect.Left < Rect.Right) then
  begin
    Rect.Top := DefRect.Top;
    IT := GetCellTextIndent(Cell);
    A := Columns[Cell.Col].Alignment;
    WR := Columns[Cell.Col].WantReturns;
    WW := Columns[Cell.Col].WordWrap;
    T := GetCellText(Cell);
    PaintText(Canvas, Rect, IT.X, A, WR, WW, T);
  end;
end;

procedure TCustomGridView.DefaultDrawHeader(Section: TGridHeaderSection; Rect: TRect);
var
  PaintState: TGridPaintStates;
  IsPressed: Boolean;
  I, X, Y, W, H: Integer;
  T: string;
  TL: Integer;
  R: TRect;
  SD: TGridSortDirection;
  SS: TSize;
  SR: TRect;
  Points: array[0..2] of TPoint;
  ElementDetails: TThemedElementDetails;
  PS: TPenStyle;
  IT: TPoint;
begin
  IsPressed := IsHeaderPressed(Section);
  SD := gsNone;
  if Section.Sections.Count = 0 then SD := GetSortDirection(Section);
  { fill in the field to the right of the header, do not change the background
    color, as it could have been changed in the OnDrawHeader event }
  PaintState := [];
  if IsPressed then Include(PaintState, psPressed);
  if (not FHeaderClicking) and (Section = FHotSection) then Include(PaintState, psHot);
  if SD <> gsNone then Include(PaintState, psSorted);
  if Header.Flat then Include(PaintState, psFlat);
  if HighlightFocusCol and IsCellValidEx(CellFocused, True, False) and
    (Section.Sections.Count = 0) and (Section.ColumnIndex = CellFocused.Col) then
    Include(PaintState, psHot);
  PaintHeaderBackground(Rect, Canvas.Brush.Color, PaintState);
  { section pressing imitation }
  if IsPressed then OffsetRect(Rect, 1, 1);
  { some section have an image }
  I := GetHeaderImage(Section);
  if I <> -1 then
  begin
    R := Rect;
    R.Right := Min(R.Left + Header.Images.Width + 2, Rect.Right);
    if R.Left < Rect.Right then
    begin
      with Canvas do
      begin
        X := R.Left + 2;
        Y := R.Top + 1 + Ord(not Header.Flat);
        W := Header.Images.Width;
        if X + W > R.Right then W := R.Right - X;
        H := Header.Images.Height;
        if Y + H > R.Bottom then H := R.Bottom - Y;
      end;
      { image is always on the left of the section rectangle }
      Rect.Left := R.Right;
    end;
  end;
  if Rect.Left < Rect.Right then
  begin
    T := Section.DisplayText;
    TL := UTF8Length(T);
    { draw sort image before section text }
    if SD <> gsNone then
    begin
      SS := GetSortArrowSize;
      if ThemeServices.ThemesEnabled {and CheckWin32Version(6, 0)} then
      begin
        SR.Left := Rect.Left + (Rect.Right - Rect.Left - SS.cx) div 2;
        SR.Right := SR.Left + SS.cx;
        SR.Top := Rect.Top;
        SR.Bottom := SR.Top + SS.cy;
        { sort image is never "pressed" (like in Explorer) }
        if IsPressed then OffsetRect(SR, -1, -1);
        if SD = gsAscending then
          ElementDetails := ThemeServices.GetElementDetails(thHeaderSortArrowSortedUp)
        else
          ElementDetails := ThemeServices.GetElementDetails(thHeaderSortArrowSortedDown);
        ThemeServices.DrawElement(Canvas.Handle, ElementDetails, SR);
      end
      else
      begin
        SR := Bounds(0, 0, SS.cx, SS.cy);
        OffsetRect(SR, Rect.Right - 10 - SS.cx,
          Rect.Top + ((Rect.Bottom - Rect.Top) - SS.cy) div 2 + SortTopIndent);
        { sort image is never "pressed" (like in Explorer) }
        if IsPressed then OffsetRect(SR, -1, -1);
        if SD = gsAscending then
        begin
          OffsetRect(SR, 0, -1);
          Points[0] := Classes.Point(SR.Left, SR.Bottom);
          Points[1] := Classes.Point(SR.Left + SS.cx div 2, SR.Top);
          Points[2] := Classes.Point(SR.Right, SR.Bottom);
        end
        else
        begin
          OffsetRect(SR, 0, 1);
          Points[0] := Classes.Point(SR.Left + 1, SR.Top);
          Points[1] := Classes.Point(SR.Right, SR.Top);
          Points[2] := Classes.Point(SR.Left + SS.cx div 2, SR.Bottom - 1);
        end;
        PS := Canvas.Pen.Style;
        Canvas.Pen.Style := psClear;
        Canvas.Brush.Color := clGrayText;
        Canvas.Polygon(Points);
        Canvas.Pen.Style := PS;
        { sort image is always on the right when on disabled themes }
        Rect.Right := SR.Left - SortLeftIndent;
      end;
    end;
    if (TL > 0) and (Rect.Left < Rect.Right) then
    begin
      R := Rect;
      R.Top := R.Top + 2;
      { section text indent is the same as cells text indent }
      IT.X := TextLeftIndent;
      IT.Y := TextTopIndent;
      if I <> -1 then Inc(IT.X, 4);
      PaintText(Canvas, R, IT.X, Section.Alignment, False, Section.WordWrap, T);
    end;
  end;
end;

procedure TCustomGridView.DrawDragRect(Cell: TGridCell);
var
  R: TRect;
begin
  if IsCellVisible(Cell, True) then
  begin
    { прямоугольник ячейки }
    R := GetEditRect(Cell);
    { цвета }
    GetCellColors(CellFocused, Canvas);
    { рисуем }
    with Canvas do
    begin
      { отсекаем место под заголовок и фиксированные }
      { cannot drag on fixed cells }
      R := GetGridRect;
      IntersectClipRect(Handle, GetFixedRect.Right, R.Top, R.Right, R.Bottom);
      { фокус }
      DrawFocusRect(R);
    end;
  end;
end;

function TCustomGridView.FindText(const FindText: string; Options: TFindOptions): Boolean;

  function CompareCell(Col, Row: Integer): Boolean;
  var
    C: TGridCell;
    T: string;
  begin
    Result := False;
    { skip hidden columns }
    if Columns[Col].Width > 0 then
    begin
      C := GridCell(Col, Row);
      T := GetCellText(C);
      if CompareStrings(FindText, T, frWholeWord in Options, frMatchCase in Options) then
      begin
        SetGridCursor(C, True, True);
        Result := True;
      end;
    end;
  end;

var
  I, R: Integer;
begin
  if Rows.Count > 0 then
  begin
    Result := True;
    if frDown in Options then
    begin
      { search forward: iterate the cells down from left to right, starting
        with next cell relative to the current one }
      I := CellFocused.Col + 1;
      R := CellFocused.Row;
      while R <= Rows.Count-1 do
      begin
        while I <= Columns.Count-1 do
        begin
          if CompareCell(I, R) then Exit;
          Inc(I);
        end;
        Inc(R);
        I := 0;
      end;
    end
    else
    begin
      { search backward: iterate the cells up from right to left, starting
        with previous cell relative to the current one }
      I := CellFocused.Col - 1;
      R := CellFocused.Row;
      { special case: during backward search, a fixed cell should be skipped,
        since it cannot be selected, the cell to the right will be selected
        instead of it; the next reverse search will again detect this fixed
        cell, and the cell to the right will be selected again, and so on.
        outwardly, it will look as if the search is frozen in one cell }
      while (I >= 0) and (Columns[I].Width = 0) do Dec(I);
      if (I < Fixed.Count) and (R >= 0) then
      begin
        Dec(R);
        I := Columns.Count - 1;
      end;
      while R >= 0 do
      begin
        while I >= 0 do
        begin
          if CompareCell(I, R) then Exit;
          Dec(I);
        end;
        Dec(R);
        I := Columns.Count - 1;
      end;
    end;
    { text not found event }
    DoTextNotFound(FindText);
  end;
  Result := False;
end;

function TCustomGridView.GetCellAt(X, Y: Integer): TGridCell;
var
  C, R: Integer;
begin
  C := GetColumnAtX(X);
  R := GetRowAtY(Y);
  if (C <> -1) and (R <> -1) then
  begin
    Result.Col := C;
    Result.Row := R;
  end
  else
  begin
    Result.Col := -1;
    Result.Row := -1;
  end;
end;

function TCustomGridView.GetCellRect(Cell: TGridCell): TRect;
var
  CR, RR: TRect;
begin
  { простое пересечение прямоугольника колонки и строки не совсем правильное,
    т.к. левый край прямоугольника строки не может быть левее последней
    фиксированной колонки }
  CR := GetColumnLeftRight(Cell.Col);
  Result.Left := CR.Left;
  Result.Right := CR.Right;
  RR := GetRowTopBottom(Cell.Row);
  Result.Top := RR.Top;
  Result.Bottom := RR.Bottom;
end;

function TCustomGridView.GetCellsRect(Cell1, Cell2: TGridCell): TRect;
var
  CR{, RR}: TRect;
begin
  { проверяем границы }
  if (Cell2.Col < Cell1.Col) or (Cell2.Row < Cell1.Row) then
    Exit( Classes.Rect(0, 0, 0, 0) );
  { левая и правая границы }
  CR := GetColumnRect(Cell1.Col);
  if Cell2.Col > Cell1.Col then CR.Right := GetColumnRect(Cell2.Col).Right;
  // { верхняя и нижняя границы }
  // RR := GetRowRect(Cell1.Row);
  // if Cell2.Row > Cell1.Row then RR.Bottom := GetRowRect(Cell2.Row).Bottom;
  { результат }
  Result.Left := CR.Left;
  Result.Right := CR.Right;
  Result.Top := CR.Top; // <- RR.Top ???
  Result.Bottom := CR.Bottom; // <- RR.Bottom ???
end;

function TCustomGridView.GetColumnAtX(X: Integer): Integer;
var
  L, R: Integer;
begin
  Result := 0;
  { ищем среди фиксированных }
  L := GetGridRect.Left;
  while Result <= Fixed.Count-1 do
  begin
    R := L + Columns[Result].Width;
    if (R <> L) and (X >= L) and (X < R) then
      Exit;
    L := R;
    Inc(Result);
  end;
  { ищем среди обычных }
  L := L + GetGridOrigin.X;
  while Result <= Columns.Count-1 do
  begin
    R := L + Columns[Result].Width;
    if (R <> L) and (X >= L) and (X < R) then
      Exit;
    L := R;
    Inc(Result);
  end;
  Result := -1;
end;

function TCustomGridView.GetColumnLeftRight(Column: Integer): TRect;
begin
  { проверяем колонку }
  if Columns.Count = 0 then
  begin
    { колонок вообще нет }
    Result.Left := GetGridRect.Left;
    Result.Right := Result.Left;
  end
  else if Column < 0 then
  begin
    { колонка левее самой первой }
    Result := GetColumnLeftRight(0);
    Result.Right := Result.Left;
  end
  else if Column > Columns.Count - 1 then
  begin
    { колонка правее самой последей }
    Result := GetColumnLeftRight(Columns.Count - 1);
    Result.Left := Result.Right;
  end
  else
  begin
    { обычная колонка }
    Result.Left := GetGridRect.Left + GetColumnsWidth(0, Column - 1);
    if Column >= Fixed.Count then Inc(Result.Left, GetGridOrigin.X);
    Result.Right := Result.Left + Columns[Column].Width;
  end;
end;

function TCustomGridView.GetColumnMaxWidth(Column: Integer): Integer;
var
  I, W: Integer;
  C: TGridCell;
  R: TRect;
begin
  { проверяем колонку }
  if (Column < 0) or (Column > Columns.Count - 1) then
    Exit( 0 );
  { а есть ли видимые строки }
  if FVisSize.Row = 0 then
    Exit( Columns[Column].DefWidth );

  Result := 0;
  { вычисляем максимальную ширину по видимым строкам }
  for I := 0 to FVisSize.Row - 1 do
  begin
    { колонка и текст ячейки }
    C := GridCell(Column, VisOrigin.Row + I);
    { определяем прямоугольник текста }
    R := GetCellTextBounds(C);
    { определяем ширину }
    W := R.Right - R.Left;
    { место под флажок }
    if IsCellHasCheck(C) then Inc(W, CheckWidth + GetCheckIndent(C).X);
    { место под картинку }
    if IsCellHasImage(C) then Inc(W, Images.Width + GetCellImageIndent(C).X);
    { учитываем сетку }
    if GridLines and (gsVertLine in GridStyle) then Inc(W, FGridLineWidth);
    { запоминаем }
    if Result < W then Result := W;
  end;
end;

function TCustomGridView.GetColumnRect(Column: Integer): TRect;
begin
  Result := GetColumnLeftRight(Column);
  { верхний и нижний край строки определяется соответственно по верхнему
    краю первой строки и по нижнему краю последней строки }
  Result.Top := GetRowTopBottom(0).Top;
  Result.Bottom := GetRowTopBottom(Rows.Count - 1).Bottom;
end;

function TCustomGridView.GetColumnsRect(Column1, Column2: Integer): TRect;
var
  R1, R2: TRect;
begin
  R1 := GetColumnRect(Column1);
  R2 := GetColumnRect(Column2);
  Result:= Default(TRect);
  UnionRect(Result, R1, R2);
end;

function TCustomGridView.GetColumnsWidth(Column1, Column2: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  { подправляем индексы }
  Column1 := MaxIntValue([Column1, 0]);
  Column2 := MinIntValue([Column2, Columns.Count-1]);
  { считаем }
  for I := Column1 to Column2 do Inc(Result, Columns[I].Width);
end;

function TCustomGridView.GetEditRect(Cell: TGridCell): TRect;
begin
  Result := GetCellRect(Cell);
  { место под флажок  }
  if IsCellHasCheck(Cell) then Inc(Result.Left, CheckWidth + GetCheckIndent(Cell).X);
  { место под картинку }
  if IsCellHasImage(Cell) then Inc(Result.Left, Images.Width + GetCellImageIndent(Cell).X);
  { учитываем сетку }
  if GridLines then
  begin
    if gsVertLine in GridStyle then Dec(Result.Right, FGridLineWidth);
    if gsHorzLine in GridStyle then Dec(Result.Bottom, FGridLineWidth);
  end;
  { проверяем правый край }
  if Result.Left > Result.Right then Result.Left := Result.Right;
end;

function TCustomGridView.GetHeaderHeight: Integer;
begin
  Result := Header.Height;
end;

function TCustomGridView.GetHeaderRect: TRect;
begin
  Result := GetClientRect;
  Result.Bottom := Result.Top;
  if ShowHeader then Inc(Result.Bottom, GetHeaderHeight);
end;

function TCustomGridView.GetFindDialog: TGridFindDialog;
begin
  if FFindDialog = nil then
  begin
    FFindDialog := TGridFindDialog.Create(Self);
    FFindDialog.OnFind := HandlerFind;
  end;
  Result := FFindDialog;
end;

function TCustomGridView.GetFirstImageColumn: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Fixed.Count to Columns.Count-1 do
    if Columns[I].Visible then
      Exit( I );
end;

function TCustomGridView.GetFixedRect: TRect;
begin
  Result := GetGridRect;
  Result.Right := Result.Left + GetFixedWidth;
end;

function TCustomGridView.GetFixedWidth: Integer;
begin
  Result := GetColumnsWidth(0, Fixed.Count - 1);
end;

function TCustomGridView.GetFocusRect: TRect;
var
  C: TGridCell;
  L: Integer;
begin
  { получаем левую колонку и прямоугольник фокуса }
  if RowSelect then
  begin
    C := GridCell(Fixed.Count, CellFocused.Row);
    Result := GetRowRect(CellFocused.Row);
    Result.Left := GetColumnLeftRight(Fixed.Count).Left;
  end
  else
  begin
    C := CellFocused;
    Result := GetCellRect(CellFocused);
  end;
  { выделяется ли картинка ячейки }
  if not ImageHighlight then
  begin
    { neither the picture nor the check box are highlighted }
    { место под флажок }
    if IsCellHasCheck(C) then Inc(Result.Left, CheckWidth + GetCheckIndent(C).X);
    { место под картинку }
    if IsCellHasImage(C) then Inc(Result.Left, Images.Width + GetCellImageIndent(C).X);
  end;
  { проверяем правый край результата }
  L := GetColumnLeftRight(C.Col).Right;
  if Result.Left > L then Result.Left := L;
end;

function TCustomGridView.GetFontHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  Metrics := Default(TTextMetric);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

function TCustomGridView.GetFontWidth(Font: TFont; TextLength: Integer): Integer;
var
  DC: HDC;
  Canvas: TCanvas;
  TM: TTextMetric;
begin
  DC := GetDC(0);
  try
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;
      TM := Default(TTextMetric);
      GetTextMetrics(DC, TM);
      Result := TextLength * (Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 4;
    finally
      Canvas.Free;
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

function TCustomGridView.GetGridHeight: Integer;
begin
  with GetGridRect do Result := Bottom - Top;
end;

function TCustomGridView.GetGridOrigin: TPoint;
begin
  Result.X := - HorzScrollBar.Position;
  Result.Y := - VertScrollBar.Position * Rows.Height;
end;

function TCustomGridView.GetGridRect: TRect;
begin
  Result := ClientRect;
  Result.Top := GetHeaderRect.Bottom;
end;

function TCustomGridView.GetHeaderSection(ColumnIndex, Level: Integer): TGridHeaderSection;

  function DoGetSection(Sections: TGridHeaderSections): TGridHeaderSection;
  var
    I, L: Integer;
    S: TGridHeaderSection;
  begin
    for I := 0 to Sections.Count-1 do
    begin
      S := Sections[I];
      L := S.Level;
      { сравниваем колонку и уровень }
      if (S.ColumnIndex >= ColumnIndex) and
        (((Level = -1) and (S.Sections.Count = 0)) or (L = Level)) then
        { нашли }
        Exit( S );
      { рекурсия на все подзаголовки снизу }
      S := DoGetSection(S.Sections);
      { нашли или нет }
      if S <> nil then
        Exit( S );
    end;
    { секции нет }
    Result := nil;
  end;

begin
  Result := DoGetSection(Header.Sections);
end;

function TCustomGridView.GetResizeSectionAt(X, Y: Integer): TGridHeaderSection;

  function FindSection(Sections: TGridHeaderSections; out Section: TGridHeaderSection): Boolean;
  var
    I, C, DL, DR: Integer;
    R: TRect;
    S: TGridHeaderSection;
  begin
    for I := Sections.Count-1 downto 0 do
    begin
      { получаем ячейку и ее колонку }
      S := Sections[I];
      { ищем только для видимых колонок }
      if S.Visible then
      begin
        C := S.ResizeColumnIndex;
        { получаем прямоугольник области изменения размера }
        R := S.BoundsRect;
        with R do
        begin
          { определяем погрешность попадания }
          DL := 7;
          if R.Right - R.Left < 20 then DL := 3;
          if R.Right - R.Left < 10 then DL := 1;
          DR := 5;
          if C < Columns.Count - 1 then
          begin
            if Columns[C + 1].DefWidth < 20 then DR := 3;
            if Columns[C + 1].DefWidth < 10 then DR := 1;
          end;
          { подправляем прямоугольник попадания }
          if R.Right > R.Left then Left := Right - DL;
          Right := Right + DR;
        end;
        { попала ли точка в него }
        if PtInRect(R, Classes.Point(X, Y)) then
        begin
          { проверяем колонку на фиксированный размер }
          { some columns cannot resize }
          if (C < Columns.Count) and (Columns[C].FixedSize or (not ColumnsResize)) then
          begin
            Section := nil;
            Result := False;
          end
          else
          begin
            Section := S;
            Result := True;
          end;
          { секцию нашли - выход }
          Exit;
        end;
        { ищем секцию в подзаголовках }
        if FindSection(S.Sections, Section) then
          Exit( True );
      end;
    end;
    { ничего не нашли }
    Section := nil;
    Result := False;
  end;

begin
  FindSection(Header.Sections, Result);
end;

function TCustomGridView.GetRowAtY(Y: Integer): Integer;
var
  Row: Integer;
  GRT, GOY: Integer;
begin
  Result := -1;
  GRT := GetGridRect.Top;
  GOY := GetGridOrigin.Y;
  if Y - GRT - GOY < 0 then // NOTE !! вот это ключевой момент !!
    Exit;
  if Rows.Height > 0 then
  begin
    Row := (Y - GRT - GOY) div Rows.Height;
    { проверяем ячейку }
    if (Row >= 0) and (Row < Rows.Count) then Result := Row;
  end;
end;

function TCustomGridView.GetRowRect(Row: Integer): TRect;
begin
  Result := GetRowTopBottom(Row);
  { левый и правый край строки определяется соответственно по левому
    краю первой нефиксированной колонки и по правому краю последней
    колонки }
  Result.Left := MinIntValue([GetGridRect.Left, GetColumnLeftRight(Fixed.Count).Left]);
  Result.Right := GetColumnLeftRight(Columns.Count - 1).Right;
end;

function TCustomGridView.GetRowsRect(Row1, Row2: Integer): TRect;
var
  R1, R2: TRect;
begin
  R1 := GetRowRect(Row1);
  R2 := GetRowRect(Row2);
  Result := Default(TRect);
  UnionRect(Result, R1, R2);
end;

function TCustomGridView.GetRowsHeight(Row1, Row2: Integer): Integer;
begin
  Result := 0;
  if Row2 >= Row1 then Result := (Row2 - Row1 + 1) * Rows.Height;
end;

function TCustomGridView.GetRowTopBottom(Row: Integer): TRect;
begin
  { верх и низ прямоугольника вычисляется по номеру и высоте строки }
  Result.Top := GetGridRect.Top + GetRowsHeight(0, Row - 1) + GetGridOrigin.Y;
  Result.Bottom := Result.Top + Rows.Height;
end;

function TCustomGridView.GetSectionAt(X, Y: Integer): TGridHeaderSection;

  function FindSection(Sections: TGridHeaderSections; out Section: TGridHeaderSection): Boolean;
  var
    I: Integer;
    S: TGridHeaderSection;
    R: TRect;
  begin
    for I := 0 to Sections.Count-1 do
    begin
      { получаем секцию }
      S := Sections[I];
      { проверяем ее }
      { ищем только для видимых колонок }
      if S.Visible then
      begin
        { получаем прямоугольник секции }
        R := S.BoundsRect;
        { попала ли точка в него }
        if PtInRect(R, Classes.Point(X, Y)) then
        begin
          { секцию нашли }
          Section := S;
          Exit( True );
        end;
      end;
      { ищем секцию в подзаголовках }
      if FindSection(S.Sections, Section) then
        Exit( True );
    end;
    { ничего не нашли }
    Section := nil;
    Result := False;
  end;

begin
  FindSection(Header.Sections, Result);
end;

procedure TCustomGridView.Invalidate;
begin
  if (Parent <> nil) and (FUpdateLock = 0) then inherited;
end;

procedure TCustomGridView.InvalidateCell(Cell: TGridCell);
var
  R: TRect;
begin
  R := GetCellRect(Cell);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateCheck(Cell: TGridCell);
var
  R: TRect;
begin
  R := GetCheckRect(Cell);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateColumn(Column: Integer);
var
  R: TRect;
begin
  R := GetColumnRect(Column);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateColumns(Column1, Column2: Integer);
var
  R: TRect;
begin
  R := GetColumnsRect(Column1, Column2);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateEdit;
begin
  if Editing then Edit.Invalidate;
end;

procedure TCustomGridView.InvalidateFixed;
begin
  InvalidateRect(GetFixedRect);
end;

procedure TCustomGridView.InvalidateFocus;
var
  Rect: TRect;
begin
  Rect := GetFocusRect;
  { cell image not included in focus rectangle }
  if IsRowHighlighted(CellFocused.Row) then
    UnionRect(Rect, Rect, GetCellRect(GridCell(Fixed.Count, CellFocused.Row)))
  else
    UnionRect(Rect, Rect, GetCellRect(CellFocused));
  { selected row not included in focus rectangle }
  if HighlightFocusRow then
  begin
    Rect.Left := 0;
    Rect.Right := Width;
  end;
  InvalidateRect(Rect);
  { invalidate header section too }
  if HighlightFocusCol and ShowHeader and ThemeServices.ThemesEnabled then
  begin
    Rect := GetColumnRect(CellFocused.Col);
    Rect.Top := 0;
    Rect.Bottom := GetHeaderHeight;
    InvalidateRect(Rect);
  end;
end;

procedure TCustomGridView.InvalidateGrid;
begin
  InvalidateEdit;
  InvalidateRect(GetGridRect);
end;

procedure TCustomGridView.InvalidateHeader;
begin
  if ShowHeader then InvalidateRect(GetHeaderRect);
end;

procedure TCustomGridView.InvalidateRect(Rect: TRect);
begin
  if (FUpdateLock = 0) and HandleAllocated and Visible then
    LCLIntf.InvalidateRect(Handle, @Rect, False);
end;

procedure TCustomGridView.InvalidateRow(Row: Integer);
var
  R: TRect;
begin
  R := GetRowRect(Row);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateRows(Row1, Row2: Integer);
var
  R: TRect;
begin
  R := GetRowsRect(Row1, Row2);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateSection(Section: TGridHeaderSection);
begin
  if ShowHEader and (Section <> nil) then InvalidateRect(Section.BoundsRect)
end;

procedure TCustomGridView.InvalidateSection(ColumnIndex, Level: Integer);
begin
  InvalidateSection(GetHeaderSection(ColumnIndex, Level));
end;

function TCustomGridView.IsActiveControl: Boolean;
var
  Form: TCustomForm;
  H: HWND;
begin
  { определяем активность по форме }
  Form := GetParentForm(Self);
  if (Form <> nil) and (Form.ActiveControl = Self) then
    Exit( True );
  { определяем по описателю }
  H := GetFocus;
  while IsWindow(H) do
  begin
    if H = WindowHandle then
      Exit( True );
    H := GetParent(H);
  end;
  { ничего не нашли }
  Result := False;
end;

function TCustomGridView.IsCellAcceptCursor(Cell: TGridCell): Boolean;
begin
  { а корректна ли ячейка }
  if not IsCellValid(Cell) then
    Exit( False );
  { результат по умолчанию }
  Result := (Cell.Col >= Fixed.Count) and Columns[Cell.Col].TabStop;
  { можно ли устанавливать курсор на ячейку }
  if Assigned(FOnCellAcceptCursor) then FOnCellAcceptCursor(Self, Cell, Result);
end;

function TCustomGridView.IsCellEditing(Cell: TGridCell): Boolean;
begin
  Result := IsCellEqual(EditCell, Cell) and Editing;
end;

function TCustomGridView.IsCellHighlighted(Cell: TGridCell): Boolean;
begin
  Result := (Cell.Col >= Fixed.Count) and
    ((CellSelected and IsCellFocused(Cell) and IsFocusAllowed) or IsRowHighlighted(Cell.Row));
end;

function TCustomGridView.IsCellHasCheck(Cell: TGridCell): Boolean;
begin
  Result := IsCellValid(Cell) and CheckBoxes and (GetCheckKind(Cell) <> gcNone);
end;

function TCustomGridView.IsCellHasImage(Cell: TGridCell): Boolean;
var
  Dummy: Integer;
begin
  Result := IsCellValid(Cell) and Assigned(Images) and (GetCellImage(Cell, Dummy) <> -1);
end;

function TCustomGridView.IsCellFocused(Cell: TGridCell): Boolean;
begin
  Result := ((Cell.Col = CellFocused.Col) or RowSelect) and
    (Cell.Row = CellFocused.Row) and (Cell.Col >= Fixed.Count);
end;

function TCustomGridView.IsCellReadOnly(Cell: TGridCell): Boolean;
begin
  Result := True;
  { а верна ли ячейка }
  if IsCellValid(Cell) then
  begin
    { ячейку можно редактировать, если таблица не в режиме ReadOnly,
      это не фиксированная колонка и сама колонка не ReadOnly }
    Result := ReadOnly or (Cell.Col < Fixed.Count) or Columns[Cell.Col].ReadOnly;
    if Assigned(FOnGetCellReadOnly) then FOnGetCellReadOnly(Self, Cell, Result);
  end;
end;

function TCustomGridView.IsCellValid(Cell: TGridCell): Boolean;
begin
  Result := IsCellValidEx(Cell, True, True);
end;

function TCustomGridView.IsCellValidEx(Cell: TGridCell; CheckPosition, CheckVisible: Boolean): Boolean;
var
  C, R, V: Boolean;
begin
  { определяем видимость, ширину колонки и корректность ячейки }
  with Cell do
  begin
    C := (Col >= 0) and (Col < Columns.Count);
    R := (Row >= 0) and (Row < Rows.Count);
    V := C and Columns[Col].Visible and (Columns[Col].Width > 0);
  end;
  { результат }
  Result := ((not CheckPosition) or (C and R)) and ((not CheckVisible) or V);
end;

function TCustomGridView.IsCellVisible(Cell: TGridCell; PartialOK: Boolean): Boolean;
var
  CR, GR, R: TRect;
begin
  { получаем границы ячейки и сетки }
  CR := GetCellRect(Cell);
  GR := GetGridRect;
  { если есть фиксированные и ячейка нефиксирована, то левая граница есть
    граница фиксированных, а не граница таблицы }
  if (Fixed.Count > 0) and (Cell.Col >= Fixed.Count) then
    GR.Left := GetFixedRect.Right;
  { пересечение }
  R := Default(TRect);
  Result := IntersectRect(R, CR, GR);
  { полная видимость }
  if not PartialOK then Result := EqualRect(R, CR);
end;

function TCustomGridView.IsColumnVisible(Column: Integer): Boolean;
var
  R: TRect;
begin
  R := Default(TRect);
  Result := IntersectRect(R, GetColumnRect(Column), GetGridRect);
end;

function TCustomGridView.IsEvenRow(Cell: TGridCell): Boolean;
begin
  Result := Cell.Row mod 2 <> 0;
end;

function TCustomGridView.IsFixedVisible: Boolean;
begin
  Result := (Columns.Count > 0) and (Fixed.Count > 0);
end;

function TCustomGridView.IsFocusAllowed: Boolean;
begin
  Result := (RowSelect or (not (Editing or AlwaysEdit))) and AllowSelect;
end;

function TCustomGridView.IsGridHintVisible: Boolean;
begin
  Result := FShowGridHint and (Rows.Count = 0);
end;

function TCustomGridView.IsHeaderHasImage(Section: TGridHeaderSection): Boolean;
begin
  Result := Assigned(Header.Images) and (GetHeaderImage(Section) <> -1);
end;

function TCustomGridView.IsHeaderPressed(Section: TGridHeaderSection): Boolean;
begin
  Result := ((Section = nil) or (Section = FHeaderClickSection)) and FHeaderClickState;
end;

function TCustomGridView.IsRowHighlighted(Row: Integer): Boolean;
begin
  Result := CellSelected and RowSelect and (Row = CellFocused.Row);
end;

function TCustomGridView.IsRowVisible(Row: Integer): Boolean;
var
  R: TRect;
begin
  R := Default(TRect);
  Result := IntersectRect(R, GetRowRect(Row), GetGridRect);
end;

procedure TCustomGridView.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TCustomGridView.MakeCellVisible(Cell: TGridCell; PartialOK: Boolean);
var
  DX, DY, X, Y: Integer;
  R: TRect;
begin
  if not IsCellVisible(Cell, PartialOK) then
  begin
    DX := 0;
    DY := 0;
    with GetGridRect do
    begin
      { смещение по горизонтали }
      // if not RowSelect then
      begin
        R := GetColumnRect(Cell.Col);
        X := Left + GetFixedWidth;
        if R.Right > Right then DX := Right - R.Right;
        if R.Left < X then DX := X - R.Left;
        if R.Right - R.Left > Right - X then DX := X - R.Left;
      end;
      { смещение по вертикали }
      if Rows.Height > 0 then
      begin
        R := GetRowRect(Cell.Row);
        if R.Bottom > Bottom then DY := Bottom - R.Bottom;
        if R.Top < Top then DY := Top - R.Top;
        if R.Bottom - R.Top > Bottom - Top then DY := Top - R.Top;
        Y := DY div Rows.Height;
        if (FVisSize.Row > 1) and (DY mod Rows.Height <> 0) then Dec(Y);
        DY := Y;
      end;
    end;
    { изменяем положение }
    with VertScrollBar do Position := Position - DY;
    with HorzScrollBar do Position := Position - DX;
  end;
end;

procedure TCustomGridView.SetGridCursor(Cell: TGridCell; IsSelected, IsVisible: Boolean);
var
  PartialOK: Boolean;
begin
  { проверяем выделение }
  UpdateSelection(Cell, IsSelected);
  { изменилось ли что нибудь }
  if (not IsCellEqual(FCellFocused, Cell)) or (FCellSelected <> IsSelected) then
  begin
    { ячейка меняется }
    Changing(Cell, IsSelected);
    { устанавливаем активную ячейку }
    if not IsCellEqual(FCellFocused, Cell) then
    begin
      { при изменении положения курсора гасим подсказку }
      CancelCellTips;
      { если идет редактирование - проверяем текст }
      Editing := False;
      { меняем ячейку }
      HideCursor;
      PartialOK := RowSelect or (FCellFocused.Col = Cell.Col);
      FCellFocused := Cell;
      FCellSelected := IsSelected;
      if IsVisible then MakeCellVisible(CellFocused, PartialOK);
      ShowCursor;
    end
    { устанавливаем выделение }
    else if FCellSelected <> IsSelected then
    begin
      { строка видна - фокус на нее, состояние не трогаем }
      if Editing then ShowEdit;
      { если строка погасла - меняем состояние курсора }
      if not Editing then
      begin
        HideCursor;
        FCellSelected := IsSelected;
        if IsVisible then MakeCellVisible(CellFocused, True);
        ShowCursor;
      end;
    end;
    { ячейка сменилась }
    Change(FCellFocused, FCellSelected);
  end
  else
    { ячейка не изменилась - подправляем видимость }
    if Visible then MakeCellVisible(CellFocused, False);
end;

procedure TCustomGridView.ResetEdit;
begin
  if (FEdit <> nil) and EditCanUndo(EditCell) then FEdit.Undo;
end;

procedure TCustomGridView.UnLockUpdate(Redraw: Boolean);
begin
  Dec(FUpdateLock);
  if (FUpdateLock = 0) and Redraw then Invalidate;
end;

procedure TCustomGridView.UpdateCursor;
var
  Cell: TGridCell;
  IsValidCell, Dummy: Boolean;
begin
  Cell := CellFocused;
  IsValidCell := IsCellValid(Cell) and IsCellAcceptCursor(Cell);
  { если текущая ячейка недоступна, то ищем доступную ячейку вокруг или
    первую попавшуюся, если таковой нет }
  if not IsValidCell then
  begin
    Dummy := False;
    UpdateSelection(Cell, Dummy);
    if IsCellEqual(Cell, CellFocused) then Cell := GetCursorCell(Cell, goFirst);
  end;
  { подправляем выделение ячейки }
  SetGridCursor(Cell, CellSelected, not IsValidCell);
end;

procedure TCustomGridView.UpdateColors;
begin
  Header.GridColorChanged(Color);
  Fixed.GridColorChanged(Color);
end;

procedure TCustomGridView.UpdateEdit(Activate: Boolean);

  procedure DoValidateEdit;
  var
    EditClass: TGridEditClass;
  begin
    { получаем класс строки редактирования }
    EditClass := GetEditClass;
    { создаем или меняем строку }
    if (FEdit = nil) or (FEdit.ClassType <> EditClass) then
    begin
      FEdit.Free;
      FEdit := CreateEdit(EditClass);
      FEdit.Parent := Self;
      FEdit.FGrid := Self;
    end;
  end;

  procedure DoUpdateEdit;
  begin
    FEditCell := FCellFocused;
    FEdit.Updating;
    FEdit.UpdateContents;
    FEdit.UpdateStyle;
    FEdit.Updated;
    FEdit.SelectAll;
  end;

begin
  { а разрешена ли строка ввода }
  if Activate and EditCanShow(FCellFocused) then
  begin
    { если строки ввода нет - создаем ее }
    if FEdit = nil then
    begin
      DoValidateEdit;
      DoUpdateEdit;
    end
    { если положение изменилось - гасим и обновляем строку }
    else if not IsCellEditing(FCellFocused) then
    begin
      Activate := Activate or Editing or AlwaysEdit;
      HideEdit;
      DoValidateEdit;
      DoUpdateEdit;
    end;
    { показываем строку }
    if Activate then
    begin
      CancelCellTips;
      FEdit.Show;
    end;
  end
  else
    { гасим строку }
    HideEdit;
end;

procedure TCustomGridView.UpdateEditContents(SaveText: Boolean);
var
  EditText: string;
begin
  if Editing then
  begin
    EditText := Edit.Text;
    { чтобы строка обновилась полностью, ее необходимо погасить }
    HideEdit;
    { обновляем и вновь показываем строку }
    UpdateEdit(True);
    { восстанавливаем текст }
    if SaveText then Edit.Text := EditText;
  end;
end;

procedure TCustomGridView.UpdateFixed;
begin
  Fixed.SetCount(Fixed.Count);
end;

procedure TCustomGridView.UpdateFocus;                           
begin
  if csDesigning in ComponentState then Exit;
  { если таблица уже активна, то ставим фокус на нее еще раз насильно,
    т.к. в противном случае могут быть глюки с MDI формами }
  if IsActiveControl then
  begin
    LCLIntf.SetFocus(Handle);
    if GetFocus = Handle then Perform(CM_UIACTIVATE, 0, 0);
  end
  else if IsWindowVisible(Handle) and TabStop and
    (CanFocus or (GetParentForm(Self) = nil)) then
  begin
    Show;
    SetFocus;
    if AlwaysEdit and (Edit <> nil) then UpdateEdit(True);
  end;
end;

procedure TCustomGridView.UpdateFonts;
begin
  Header.GridFontChanged(Font);
  Fixed.GridFontChanged(Font);
end;

procedure TCustomGridView.UpdateHeader;
begin
  with Header do
  begin
    SynchronizeSections;
    SetSectionHeight(SectionHeight);
  end;
end;

procedure TCustomGridView.UpdateRows;
begin
  Rows.SetHeight(Rows.Height);
end;

procedure TCustomGridView.UpdateScrollBars;

  procedure UpdateVertScrollBar;
  var
    R, P, L: Integer;
  begin
    if (Rows.Count > 0) and (Rows.Height > 0) then
    begin
      R := Rows.Count - 1;
      with GetGridRect do P := (Bottom - Top) div Rows.Height;
      L := 1;
    end
    else
    begin
      R := 0;
      P := 0;
      L := 0;
    end;
    with VertScrollBar do
    begin
      SetLineSize(Rows.Height);
      SetParams(0, R, P, L);
    end;
  end;

  procedure UpdateHorzScrollBar;
  var
    R, P, L: Integer;
  begin
    if Columns.Count > 0 then
    begin
      R := GetColumnsWidth(0, Columns.Count - 1) - GetFixedWidth;
      with GetGridRect do P := (Right - Left) - GetFixedWidth;
      L := 8;
    end
    else
    begin
      R := 0;
      P := 0;
      L := 0;
    end;
    with HorzScrollBar do
    begin
      SetLineSize(1);
      SetParams(0, R, P, L);
    end;
  end;

begin
  { when updating scrollers, the window will resize several times and each
    resizing will require redrawing, it needs to be blocked and redrawn
    once at the end }
  LockUpdate;
  try
    UpdateVertScrollBar;
    UpdateHorzScrollBar;
    UpdateVertScrollBar;
  finally
    UnLockUpdate(False);
  end;
end;

procedure TCustomGridView.UpdateScrollPos;
begin
  VertScrollBar.Position := FVisOrigin.Row;
  HorzScrollBar.Position := GetColumnsWidth(Fixed.Count, FVisOrigin.Col - 1);
end;

procedure TCustomGridView.UpdateSelection(var Cell: TGridCell; var Selected: Boolean);
begin
  { Проверка флага выделения }
  Selected := Selected or FAlwaysSelected;
  Selected := Selected and (Rows.Count > 0) and (Columns.Count > 0);
  { проверка ячейки на границы }
  with Cell do
  begin
    if Col < Fixed.Count then Col := Fixed.Count;
    if Col < 0 then Col := 0;
    if Col > Columns.Count - 1 then Col := Columns.Count - 1;
    if Row < 0 then Row := 0;
    if Row > Rows.Count - 1 then Row := Rows.Count - 1;
  end;
  { проверяем фокус }
  Cell := GetCursorCell(Cell, goSelect);
end;

procedure TCustomGridView.UpdateText;
begin
  ApplyEditText;
end;

procedure TCustomGridView.UpdateVisOriginSize;
var
  R: TRect;
  I, X, H: Integer;
begin
  if Columns.Count > 0 then
  begin
    { ищем первую попавшуюся видимую нефиксированную колонку }
    X := GetGridRect.Left + GetFixedWidth - HorzScrollBar.Position;
    R := GetFixedRect;
    I := Fixed.Count;
    while I < Columns.Count-1 do
    begin
      X := X + Columns[I].Width;
      if X > R.Right then Break;
      Inc(I);
    end;
    FVisOrigin.Col := I;
    { считаем количество видимых колонок }
    R := GetGridRect;
    while I < Columns.Count-1 do
    begin
      if X >= R.Right then Break;
      Inc(I);
      X := X + Columns[I].Width;
    end;
    FVisSize.Col := I - FVisOrigin.Col + 1;
  end
  else
  begin
    FVisOrigin.Col := 0;
    FVisSize.Col := 0;
  end;
  if (Rows.Count > 0) and (Rows.Height > 0) then
  begin
    { вертикальный движок определяем номер первой видимой строки }
    FVisOrigin.Row := VertScrollBar.Position;
    { считаем количество видимых (пусть даже и частично) строк }
    H := GetGridHeight;
    FVisSize.Row := H div Rows.Height + Ord(H mod Rows.Height > 0);
    if FVisSize.Row + FVisOrigin.Row  > Rows.Count then
      FVisSize.Row := Rows.Count - FVisOrigin.Row;
    { видимых строк не может быть меньше ноля }
    if FVisSize.Row < 0 then FVisSize.Row := 0; 
  end
  else
  begin
    FVisOrigin.Row := 0;
    FVisSize.Row := 0;
  end;
end;

end.
