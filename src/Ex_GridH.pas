{
  TGridView component (grid)
  (C) Roman M. Mochalov, 1997-2019
  (C) Iluha Companets  , 2002-2023
  License: MIT
}

unit Ex_GridH;

{$mode delphi}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Forms, Graphics, StdCtrls, ComCtrls,
  Ex_Grid;

type

{ THeaderEditorForm }

  THeaderEditorForm = class(TForm)
    SectionssGroup: TGroupBox;
    SectionsTree: TTreeView;
    AddButton: TButton;
    DeleteButton: TButton;
    PropertiesGroup: TGroupBox;
    IndexLabel: TLabel;
    IndexEdit: TEdit;
    CaptionLabel: TLabel;
    CaptionEdit: TEdit;
    AlignmentLabel: TLabel;
    AlignmentCombo: TComboBox;
    WordWrapCheck: TCheckBox;
    OKButton: TButton;
    CancelButton: TButton;
    ApplyButton: TButton;
    procedure EnableApply(Sender: TObject);
    procedure DisableApply(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SectionsTreeChange(Sender: TObject; Node: TTreeNode);
    procedure SectionsTreeChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure SectionsTreeEnter(Sender: TObject);
    procedure SectionsTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure IndexEditKeyPress(Sender: TObject; var Key: Char);
    procedure OKButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
  private
    FGrid: TCustomGridView;
    FSections: TGridHeaderSections;
    FExceptCount: Integer;
    FChangeCount: Integer;
    procedure AddSection;
    procedure DeleteAllSections;
    procedure DeleteSection;
    procedure GetSection;
    procedure GetParams;
    procedure MoveSectionDown;
    procedure MoveSectionLeft;
    procedure MoveSectionRight;
    procedure MoveSectionUp;
    procedure PutSection;
    procedure PutParams;
    procedure RefreshView;
    procedure SelectSection(Section: TGridHeaderSection);
  public
    function Execute(Grid: TCustomGridView): Boolean;
  end;

function EditGridHeader(Grid: TCustomGridView): Boolean;

implementation

{$R *.lfm}

function EditGridHeader(Grid: TCustomGridView): Boolean;
begin
  with THeaderEditorForm.Create(nil) do
  try
    { показываем диалог }
    Execute(Grid);
    { результат }
    Result := FChangeCount > 0;
  finally
    Free;
  end;
end;


{ THeadersEditorForm }

procedure THeaderEditorForm.AddSection;
var
  S: TGridHeaderSection;
  SS: TGridHeaderSections;
begin
  with SectionsTree do
    { есть ли выделенный узел }
    if Selected <> nil then
    begin
      { определяем секции, к которым следует добавлять }
      S := TGridHeaderSection(Selected.Data);
      if S <> nil then SS := S.Sections else SS := FSections;
      { добавляем секцию }
      SS.Add;
      { обновляем дерево заголовков }
      RefreshView;
      { фокус на строку с названием }
      CaptionEdit.SetFocus;
      { доступ к кнопке "Применить" }
      EnableApply(Self);
    end;
end;

procedure THeaderEditorForm.DeleteAllSections;
const
  Message = 'Удалить все секции текущего уровня?';
  Flags = MB_YESNO or MB_ICONQUESTION;
var
  S: TGridHeaderSection;
begin
  with SectionsTree do
    { есть ли выделенная секция }
    if (Selected <> nil) and (Selected.Data <> nil) then
    begin
      { получаем выделенную секцию }
      S := TGridHeaderSection(Selected.Data);
      { спрашиваем разрешение на удаление }
      if Application.MessageBox(Message, PChar(Caption), Flags) = IDYES then
      begin
        { удаляем секции }
        S.ParentSections.Clear;
        { обновляем дерево заголовков }
        RefreshView;
        { доступ к кнопке "Применить" }
        EnableApply(nil);
      end;
    end;
end;

procedure THeaderEditorForm.DeleteSection;
var
  S: TGridHeaderSection;
begin
  with SectionsTree do
    { есть ли выделенная секция }
    if (Selected <> nil) and (Selected.Data <> nil) then
    begin
      { получаем выделенную секцию }
      S := TGridHeaderSection(Selected.Data);
      { удаляем ее }
      S.Free;
      { обновляем дерево заголовков }
      RefreshView;
      { фокус на дерево }
      SetFocus;
      { доступ к кнопке "Применить" }
      EnableApply(nil);
    end;
end;

procedure THeaderEditorForm.GetSection;
var
  S: TGridHeaderSection;
begin
  with SectionsTree do
    { есть ли выделенная секция }
    if (Selected <> nil) and (Selected.Data <> nil) then
    begin
      { получаем выделенную секцию }
      S := TGridHeaderSection(Selected.Data);
      { считываем ее параметры }
      IndexEdit.Text := IntToStr(S.Index);
      CaptionEdit.Text := S.Caption;
      AlignmentCombo.ItemIndex := Ord(S.Alignment);
      WordWrapCheck.Checked := S.WordWrap;
    end;
end;

procedure THeaderEditorForm.GetParams;
begin
  { запоминаем список секций }
  FSections.Assign(FGrid.Header.Sections);
  { обновляем дерево заголовков }
  RefreshView;
end;

procedure THeaderEditorForm.MoveSectionDown;
var
  S: TGridHeaderSection;
begin
  with SectionsTree do
    { есть ли выделенная секция }
    if (Selected <> nil) and (Selected.Data <> nil) then
    begin
      { получаем выделенную секцию }
      S := TGridHeaderSection(Selected.Data);
      { можно ли двигать ее вниз }
      if S.Index < S.ParentSections.Count - 1 then
      begin
        { перемещаем вниз }
        S.Index := S.Index + 1;
        { обновляем дерево заголовков }
        RefreshView;
        { подправляем выделенный узел }
        SelectSection(S);
        { доступ к кнопке "Применить" }
        EnableApply(nil);
      end;
    end;
end;

procedure THeaderEditorForm.MoveSectionLeft;
var
  S, O: TGridHeaderSection;
begin
  with SectionsTree do
    { есть ли выделенная секция }
    if (Selected <> nil) and (Selected.Data <> nil) then
    begin
      { получаем выделенную секцию }
      S := TGridHeaderSection(Selected.Data);
      O := S.ParentSections.OwnerSection;
      { можно ли двигать ее влево }
      if (O <> nil) and (O.ParentSections <> nil) then
      begin
        { перемещаем влево }
        S.Collection := O.ParentSections;
        S.Index := O.Index;
        { обновляем дерево заголовков }
        RefreshView;
        { подправляем выделенный узел }
        SelectSection(S);
        { доступ к кнопке "Применить" }
        EnableApply(nil);
      end;
    end;
end;

procedure THeaderEditorForm.MoveSectionRight;
var
  S: TGridHeaderSection;
begin
  with SectionsTree do
    { есть ли выделенная секция }
    if (Selected <> nil) and (Selected.Data <> nil) then
    begin
      { получаем выделенную секцию }
      S := TGridHeaderSection(Selected.Data);
      { можно ли двигать ее влево }
      if S.Index < S.ParentSections.Count - 1 then
      begin
        { перемещаем влево }
        S.Collection := S.ParentSections[S.Index + 1].Sections;
        S.Index := 0;
        { обновляем дерево заголовков }
        RefreshView;
        { подправляем выделенный узел }
        SelectSection(S);
        { доступ к кнопке "Применить" }
        EnableApply(nil);
      end;
    end;
end;

procedure THeaderEditorForm.MoveSectionUp;
var
  S: TGridHeaderSection;
begin
  with SectionsTree do
    { есть ли выделенная секция }
    if (Selected <> nil) and (Selected.Data <> nil) then
    begin
      { получаем выделенную секцию }
      S := TGridHeaderSection(Selected.Data);
      { можно ли двигать ее вверх }
      if S.Index > 0 then
      begin
        { перемещаем вверх }
        S.Index := S.Index - 1;
        { обновляем дерево заголовков }
        RefreshView;
        { подправляем выделенный узел }
        SelectSection(S);
        { доступ к кнопке "Применить" }
        EnableApply(nil);
      end;
    end;
end;

procedure THeaderEditorForm.PutSection;
var
  S: TGridHeaderSection;
begin
  with SectionsTree do
    { есть ли выделенная секция }
    if (Selected <> nil) and (Selected.Data <> nil) then
    begin
      { получаем выделенную секцию }
      S := TGridHeaderSection(Selected.Data);
      { устанавливаем параметры }
      S.Index := StrToIntDef(IndexEdit.Text, S.Index);
      S.Caption := CaptionEdit.Text;
      S.Alignment := TAlignment(AlignmentCombo.ItemIndex);
      S.WordWrap := WordWrapCheck.Checked;
    end;
end;

procedure THeaderEditorForm.PutParams;
begin
  FGrid.Header.Sections := FSections;
end;

procedure THeaderEditorForm.RefreshView;

  procedure BeginRefresh;
  begin
    SectionsTree.OnChange := nil;
    SectionsTree.OnChanging := nil;
    IndexEdit.OnChange := nil;
    CaptionEdit.OnChange := nil;
    AlignmentCombo.OnChange := nil;
    WordWrapCheck.OnClick := nil;
  end;

  procedure EndRefresh;
  begin
    SectionsTree.OnChange := SectionsTreeChange;
    SectionsTree.OnChanging := SectionsTreeChanging;
    IndexEdit.OnChange := EnableApply;
    CaptionEdit.OnChange := EnableApply;
    AlignmentCombo.OnChange := EnableApply;
    WordWrapCheck.OnClick := EnableApply;
  end;

  procedure RefreshTree;

    function CalcColumnIndex(Section: TGridHeaderSection): Integer;
    var
      Left: TGridHeaderSection;
    begin
      if Section.Index > 0 then
      begin
        Left := Section.ParentSections[Section.Index - 1];
        Result := CalcColumnIndex(Left);
        if Left.Sections.Count > 0 then
          Inc(Result, Left.Sections.Count)
        else
          Inc(Result);
      end
      else if Section.Parent <> nil then
        Result := CalcColumnIndex(Section.Parent)
      else
        Result := 0;
    end;

    procedure ProcessNode(Node: TTreeNode; Sections: TGridHeaderSections);
    var
      I, J: Integer;
      S: string;
    begin
      { уравниваем количество дочерних узлов с количеством секций }
      if Node.Count > Sections.Count then
      begin
        I := Node.Count;
        while I > Sections.Count do
        begin
          Dec(I);
          Node[I].Delete;
        end;
      end;
      if Node.Count < Sections.Count then
      begin
        I := Node.Count;
        while I < Sections.Count do
        begin
          Inc(I);
          with SectionsTree do
            Selected := Items.AddChild(Node, '');
        end;
      end;
      { обновляем узлы }
      for I := 0 to Node.Count - 1 do
      begin
        S := Sections[I].Caption;
        { bottom level sections use columns captions if they do not have
          their own captions (see DisplayText property) but internal
          FSections is not linked with grid, so section cannot get column
          caption, and we must calculate it manually }
        if (S = '') and (Sections[I].Sections.Count = 0) then
        begin
          J := CalcColumnIndex(Sections[I]);
          if J < FGrid.Columns.Count then S := FGrid.Columns[J].Caption;

        end;
        Node[I].Text := S;
        Node[I].Data := Sections[I];
      end;
      { обновляем дочерние секции }
      for I := 0 to Node.Count - 1 do
        ProcessNode(Node[I], Sections[I].Sections);
    end;

  begin
    ProcessNode(SectionsTree.Items.GetFirstNode, FSections);
  end;

  procedure RefreshControls;
  begin
    with SectionsTree do
      { есть ли выделенная секция }
      if (Selected <> nil) and (Selected.Data <> nil) then
      begin
        { разрешаем изменение параметров }
        PropertiesGroup.Enabled := True;
        DeleteButton.Enabled := True;
        { обновляем компоненты параметров }
        GetSection;
      end
      else
      begin
        { очищаем компоненты параметров }
        WordWrapcheck.Checked := False;
        AlignmentCombo.ItemIndex := -1;
        CaptionEdit.Text := '';
        IndexEdit.Text := '';
        { запрещаем изменение параметров }
        DeleteButton.Enabled := False;
        PropertiesGroup.Enabled := False;
      end;
  end;

begin
  BeginRefresh;
  try
    RefreshTree;
    RefreshControls;
  finally
    Endrefresh;
  end;
end;

procedure THeaderEditorForm.SelectSection(Section: TGridHeaderSection);

  function FindNode(Node: TTreeNode): TTreeNode;
  begin
    while Node <> nil do
    begin
      { проверяем узел }
      if Node.Data = Section then
        Exit( Node );
      { ищем среди дочерних узлов }
      Result := FindNode(Node.GetFirstChild);
      if Result <> nil then
        Exit;
      { следующий узел }
      Node := Node.GetNextSibling;
    end;
    { узел не найден }
    Result := nil;
  end;

begin
  SectionsTree.Selected := FindNode(SectionsTree.Items.GetFirstNode);
end;

function THeaderEditorForm.Execute(Grid: TCustomGridView): Boolean;
begin
  { запоминаем ссылку на таблицу }
  FGrid := Grid;
  { считываем заголовок }
  GetParams;
  { показываем диалог }
  Result := ShowModal = mrOK;
end;

procedure THeaderEditorForm.EnableApply(Sender: TObject);
begin
  OKButton.Default := False;
  ApplyButton.Enabled := True;
  ApplyButton.Default := True;
end;

procedure THeaderEditorForm.DisableApply(Sender: TObject);
begin
  ApplyButton.Default := False;
  ApplyButton.Enabled := False;
  OKButton.Default := True;
end;

procedure THeaderEditorForm.FormCreate(Sender: TObject);
begin
  FSections := TGridHeaderSections.Create(nil, nil);
  SectionsTree.Items.Add(nil, 'Секции');
end;

procedure THeaderEditorForm.FormDestroy(Sender: TObject);
begin
  FSections.Free;
end;

procedure THeaderEditorForm.SectionsTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if not (csDestroying in ComponentState) then RefreshView;
end;

procedure THeaderEditorForm.SectionsTreeChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  if not (csDestroying in ComponentState) then
  begin
    try
      PutSection;
      RefreshView;
    except
      if FExceptCount = 0 then Application.HandleException(Self);
      Inc(FExceptCount);
    end;
    AllowChange := FExceptCount = 0;
  end;
end;

procedure THeaderEditorForm.SectionsTreeEnter(Sender: TObject);
begin
  FExceptCount := 0;
end;

procedure THeaderEditorForm.SectionsTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_INSERT:
        begin
          with AddButton do if Enabled then Click;
          Key := 0;
        end;
      VK_DELETE:
        begin
          with DeleteButton do if Enabled then Click;
          Key := 0;
        end;
    end;
  if Shift = [ssCtrl] then
    case Key of
      VK_DELETE:
        begin
          DeleteAllSections;
          Key := 0;
        end;
      VK_UP:
        begin
          PutSection;
          MoveSectionUp;
          Key := 0;
        end;
      VK_DOWN:
        begin
          PutSection;
          MoveSectionDown;
          Key := 0;
        end;
      VK_LEFT:
        begin
          PutSection;
          MoveSectionLeft;
          Key := 0;
        end;
      VK_RIGHT:
        begin
          PutSection;
          MoveSectionRight;
          Key := 0;
        end;
    end;
end;

procedure THeaderEditorForm.AddButtonClick(Sender: TObject);
begin
  PutSection;
  AddSection;
end;

procedure THeaderEditorForm.DeleteButtonClick(Sender: TObject);
begin
  PutSection;
  DeleteSection;
end;

procedure THeaderEditorForm.IndexEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Beep;
    Key := #0;
  end;
end;

procedure THeaderEditorForm.OKButtonClick(Sender: TObject);
begin
  ApplyButtonClick(ApplyButton);
  ModalResult := mrOK;
end;

procedure THeaderEditorForm.ApplyButtonClick(Sender: TObject);
begin
  { проверяем и вставляем параметры }
  PutSection;
  PutParams;
  { обновлем компоненты }
  GetParams;
  DisableApply(nil);
  { увеличиваем счетчик изменений }
  Inc(FChangeCount);
end;

end.
