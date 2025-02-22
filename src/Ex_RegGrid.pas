{
  TGridView component (grid)
  (C) Roman M. Mochalov, 1997-2019
  (C) Iluha Companets  , 2002-2025
  License: MIT
}

unit Ex_RegGrid;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Forms, Dialogs, TypInfo,
  LResources, ComponentEditors, PropEdits;

type

{ TGridEditor }

  TGridEditor = class(TDefaultEditor)
  private
    FCollection: TCollection;
    procedure FindCollectionEditor(const PropertyEditor: TPropertyEditor);
  protected
    procedure EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TGridColumnsProperty }

  TGridColumnsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TGridHeaderProperty }

  TGridHeaderProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TDBGridFieldNameProperty }

  TDBGridFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Ex_Grid, Ex_GridC, Ex_GridH, Ex_DBGrid;

{$R *.dcr}

{ TGridEditor }

procedure TGridEditor.FindCollectionEditor(const PropertyEditor: TPropertyEditor);
var
  P: PTypeInfo;
begin
  { если редактор свойства найден, то значение FCollection будет
    сброшено в nil }
  if FCollection <> nil then
  begin
    P := PropertyEditor.GetPropType;
    { проверяем тип и название свойства }
    if (P <> nil) and (P^.Kind = tkClass) and (CompareText(P^.Name, FCollection.ClassName) = 0) then
    begin
      PropertyEditor.Edit;
      FCollection := nil;
    end;
  end;
  //if FContinue then
  //  EditProperty(Prop, FContinue);
  // NOTE !! об освобождении редактора свойства необходимо заботится самим? !!
  //PropertyEditor.Free;
end;

procedure TGridEditor.EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean);
begin
  inherited EditProperty(PropertyEditor, Continue);
  { Двойной щелчок на компоненте в дизайнере Delphi приводит к вызову
    метода Edit у TDefaultEditor. TDefaultEditor перебирает внутри Edit
    все свойства компонента, ищет свойство-метод с именем OnCreate,
    OnChange или OnClick (или первый попавшийся, если указанных нет) и
    вызывает Edit соответствующего редактор свойства. Редактор свойтва в
    свою очередь ставит курсор на обработчик данного метода в тексте.
    Поэтому, если мы хотим, чтобы двойной щелчок на таблице автоматически
    ставил курсор на OnGetCellText, то нам необходимо самим найти редактор
    свойства этого свойства и "выполнить" его. }
  if CompareText(PropertyEditor.GetName, 'ONGETCELLTEXT') = 0 then
  begin
    PropertyEditor.Edit;
    { говорим стандартному обработчику, что исполнять найденный им
      редактор свойства не надо }
    Continue := False;
  end;
end;

procedure TGridEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: if EditGridColumns(TCustomGridView(Component)) then Designer.Modified;
    1: if EditGridHeader(TCustomGridView(Component)) then Designer.Modified;
  end;
end;

function TGridEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Columns Editor...';
    1: Result := 'Header Editor...';
  end;
end;

function TGridEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TGridColumnsProperty }

procedure TGridColumnsProperty.Edit;
begin
  if EditGridColumns(TGridColumns(GetObjectValue).Grid) then Modified;
end;

function TGridColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TGridHeaderProperty }

procedure TGridHeaderProperty.Edit;
begin
  if EditGridHeader(TGridHeaderSections(GetObjectValue).Header.Grid) then Modified;
end;

function TGridHeaderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TDBGridFieldNameProperty }

function TDBGridFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBGridFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  Grid: TCustomDBGridView;
  Values: TStringList;
  I: Integer;
begin
  Grid := TDBGridColumn(GetComponent(0)).Grid;
  if (Grid <> nil) and (Grid.DataLink.DataSet <> nil) then
  begin
    Values := TStringList.Create;
    try
      Grid.DataLink.DataSet.GetFieldNames(Values);
      for I := 0 to Values.Count - 1 do Proc(Values[I]);
    finally
      Values.Free;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('GridView', [TGridView, TDBGridView]);

  RegisterComponentEditor(TGridView, TGridEditor);
  RegisterPropertyEditor(TypeInfo(TGridHeaderSections), TGridHeader, 'Sections', TGridHeaderProperty);
  //RegisterPropertyEditor(TypeInfo(TGridColumns), TGridView, 'Columns', TGridColumnsProperty);

  RegisterComponentEditor(TDBGridView, TGridEditor);
  RegisterPropertyEditor(TypeInfo(string), TDBGridColumn, 'FieldName', TDBGridFieldNameProperty);
  //RegisterPropertyEditor(TypeInfo(TGridHeaderSections), TDBGridHeader, 'Sections', TGridHeaderProperty);
end;

end.
