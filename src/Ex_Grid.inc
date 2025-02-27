{%MainUnit Ex_Grid.pas}

uses
  LazUTF8, LazUtilities, LCLType, LCLIntf, LMessages, Types, Themes,
  SysUtils, DateUtils, Classes, Controls, Graphics, Forms,
  Dialogs, StdCtrls, Math, MaskEdit, ImgList, Menus,
  Ex_Utils;

const
  GRID_LINE_WIDTH = 1;

type
  TGridHeaderSections = class;
  TCustomGridHeader = class;
  TCustomGridColumn = class;
  TGridColumns = class;
  TCustomGridRows = class;
  TCustomGridFixed = class;
  TCustomGridView = class;

  TGridEditStyle = (geSimple, geEllipsis, gePickList, geDataList, geUserDefine);

  TGridCheckKind = (gcNone, gcCheckBox, gcRadioButton, gcUserDefine);
  TGridCheckStyle = (csFlat, cs3D, csWin95);

{ TGridHeaderSection }

  {
    Секция заголовка.

    Свойства:

    ColumnIndex -       Индекс соответствующей колонки. Для заголовка с
                        подзаголовками - это индекс, соотвествующий последнему
                        подзаголовку.
    BoundsRect -        Прямоугольник секции. Для заголовка с подзаголовками
                        не включает в себя границы подзаголовков.
    FirstColumnIndex -  Индекс самой левой колонки секции заголовка.
    FixedColumn -       Столбец заголовка или один из его подзоголовков
                        фиксирован.
    Header -            Ссылка на заголовок таблицы.
    Level -             Уровень заголовка. Самый верхний заголовок имеет
                        уровень 0, под ним - 1 и т.д.
    Parent -            Ссылка на верхнюю секцию.
    ParentSections -    Ссылка на список секций, которому принадлежит данная
                        секция.
    ResizeColumnIndex - Индекс колонки для изменения ширины при изменении
                        ширины секции мышкой. Для заголовка с подзаголовками
                        это индекс, соотвествующий последнему видимому
                        подзаголовку, для самой нижней секции это ColumnIndex.
    Visible -           Видимость секции. Секция видна, если видна хотябы одна
                        из подсекций или если видна соотвествующая колонка или
                        для секции нет колонки.

    Alignment -         Выравнивание текста заголовка по горизонтали.
    Caption -           Текст заголовка. Соответствует заголовку колонки.
    Sections -          Список подзаголовков (т.е. секций снизу).
    Width -             Ширина заголовка. Равна ширине соотвествующей колонки
                        или сумме ширин подзаголовков.
    WordWrap -          Перенос слов текста заголовка.
  }

  {
    Public Properties:

    BoundsRect -        Returns the section bounds.
    ColumnIndex -       The index of the corresponding column. For a multilevel
                        section, this is the index corresponding to the column
                        index of the rightmost subsection.
    FirstColumnIndex -  Returns the index of the leftmost column of a section.
    FixedColumn -       Specifies whether the section is sizeable.
    Header -            Indicates the header to which the section instance
                        belongs.
    Level -             Indicates section level. The top level section is level 0.
    Parent -            Refers the section one level up that contains the section.
                        The top level section has no parent section.
    ResizeColumnIndex - Returns the index of the column whose size is changed
                        when the width of the section is changed with the mouse.
                        For a multilevel section, this corresponds to the index
                        of the rightmost subsection.
    Visible -           Indicates the section is visible. A section is visible
                        if at least one of the subsections is visible or if the
                        corresponding column is visible.
    Width -             The width of the section. Equal to the sum of the width
                        of the subsections or the width of the corresponding
                        column.

    Published Properties:

    Alignment -         Determines how the text is aligned within the section.
    Caption -           Specified the title of the section. If not specified,
                        the column caption is used.
    Sections -          Lists the subsections.
    WordWrap -          Determines whether the section inserts soft carriage
                        returns so text wraps at the right margin of the section.
  }

  TGridHeaderSection = class(TCollectionItem)
  private
    FSections: TGridHeaderSections;
    FCaption: string;
    FAlignment: TAlignment;
    FWordWrap: Boolean;
    FBoundsRect: TRect;
    FColumnIndex: Integer;
    function IsSectionsStored: Boolean;
    function GetAllowClick: Boolean;
    function GetBoundsRect: TRect;
    function GetDisplayText: string;
    function GetFirstColumnIndex: Integer;
    function GetFixedColumn: Boolean;
    function GetHeader: TCustomGridHeader;
    function GetLevel: Integer;
    function GetParent: TGridHeaderSection;
    function GetParentSections: TGridHeaderSections;
    function GetResizeColumnIndex: Integer;
    function GetSections: TGridHeaderSections;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure ReadWidth(Reader: TReader);
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetSections(Value: TGridHeaderSections);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AllowClick: Boolean read GetAllowClick;
    property BoundsRect: TRect read GetBoundsRect;
    property ColumnIndex: Integer read FColumnIndex;
    property DisplayText: string read GetDisplayText;
    property FirstColumnIndex: Integer read GetFirstColumnIndex;
    property FixedColumn: Boolean read GetFixedColumn;
    property Header: TCustomGridHeader read GetHeader;
    property Level: Integer read GetLevel;
    property Parent: TGridHeaderSection read GetParent;
    property ParentSections: TGridHeaderSections read GetParentSections;
    property ResizeColumnIndex: Integer read GetResizeColumnIndex;
    property Visible: Boolean read GetVisible;
    property Width: Integer read GetWidth;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property Sections: TGridHeaderSections read GetSections write SetSections stored IsSectionsStored;
  end;

{ TGridHeaderSections }

  {
    Список секций заголовка.

    Процедуры:

    Add -         Добавить новую секцию в список.

    Свойства:

    Header -      Ссылка на заголовок таблицы.
    MaxColumn -   Максимальный индекс столбца.
    MaxLevel -    Максимальный уровень подзаголовков. Самый верхний
                  заголовок имеет уровень 0, под ним - 1 и т.д.
    Owner -       Ссылка на секцию - владельца.
    Sections -    Список подзаголовков.
  }

  {
    Public Methods:

    Add -          Creates a new TGridHeaderSection instance and adds it to the
                   Sections array.

    Public Properties:

    Header -       Indicates the header to which the sections instance belongs.
    MaxColumn -    Indicates the maximum column index of the subsections.
    MaxLevel -     Indicates the maximum level of the subsections. The top level
                   section is level 0.
    OwnerSection - Refers the section one level up that contains the section.
                   The top level section has no owner.
    Sections -     Lists the subsections.
  }

  TGridHeaderSections = class(TCollection)
  private
    FHeader: TCustomGridHeader;
    FOwnerSection: TGridHeaderSection;
    function GetMaxColumn: Integer;
    function GetMaxLevel: Integer;
    function GetSection(Index: Integer): TGridHeaderSection;
    procedure SetSection(Index: Integer; Value: TGridHeaderSection);
  protected
    function GetOwner: TPersistent; override;
    procedure Update({%H-}Item: TCollectionItem); override;
  public
    constructor Create(AHeader: TCustomGridHeader; AOwnerSection: TGridHeaderSection); virtual;
    function Add: TGridHeaderSection;
    property Header: TCustomGridHeader read FHeader;
    property MaxColumn: Integer read GetMaxColumn;
    property MaxLevel: Integer read GetMaxLevel;
    property OwnerSection: TGridHeaderSection read FOwnerSection;
    property Sections[Index: Integer]: TGridHeaderSection read GetSection write SetSection; default;
  end;

{ TCustomGridHeader }

  {
    Заголовок таблицы.

    Процедуры:

    SynchronizeSections - Уравнивает количество нижних секций с количеством
                          указанных столбцов.
    UpdateSections      - Обновление внутренних параметров секций (BoundsRect
                          и ColumnIndex). Данные параметры инициализируются
                          один раз при изменении заголовка для ускорения
                          работы с секциями.

    Свойства:

    Grid -              Ссылка на таблицу.
    Height -            Высота.
    Images -            Картинки секций заголовка.
    MaxColumn -         Максимальный индекс столбца.
    MaxLevel -          Максимальный уровень подзаголовков.
    Width -             Ширина

    AutoHeight -        Автоматически подбирать высоту секций.
    AutoSynchronize -   Автоматически синхронизировать секции заголовка с
                        колонками.
    Color -             Цвет фона.
    Flat -              Вид отображения секций заголовка. Если Flat = True,
                        то секции заголовока отображаются плоскими, в
                        противном слечае - виде кнопок.
    Font -              Шрифт.
    FullSynchronizing - Полностью синхронизировать секции заголовка с
                        колонками, включая текст заголовка и выравнивание
                        текста. В противном случае синхронизируется только
                        количество секций с количеством колонок.
    GridColor -         Брать ли в качестве цвета заголовка цвет таблицы.
                        ПРИМЕЧАНИЕ: если GridColor = True, то разделительные
                        линии секций заголовка рисуются одинарными линиями,
                        как сетка у обычных ячеек. В противном случае
                        разделительные линии рисуются двойными.
    GridFont -          Брать ли в качестве шрифта заголовка шрифт таблицы.
    SectionHeight -     Высота одной секции (подзаголовка). При установленном
                        свойстве AutoHeight выбирается с учетом высоты
                        картинок, 3D эффекта, значения свойства GridColor и
                        высоты шрифта.
    Sections -          Список подзаголовков.
    Synchronized -      Секции заголовка синхронизированы с колонками
                        таблицы.

    События:

    OnChange -          Событие на изменение параметров.
  }

  {
    Public Properties:

    Grid -          Indicates the grid control to which the header instance
                    belongs.
    MaxColumn -     Indicates the maximum column index of the subsections.
    MaxLevel -      Indicates the maximum level of the subsections. The top
                    level section is level 0.

    Published Properties:

    AutoHeight -    Determines whether the height of the header sections
                    automatically resizes to accommodate the text.
    Color -         Specifies the background color of the header.
    Images -        Provides a list of icon images to display for sections
                    of the header. To determinate image index of the section
                    use OnGetHeaderImage event.
    Flat -          Specifies whether the sections of the header are flat.
    Font -          Controls the attributes of text written on the header.
    GridColor -     Determines where a header looks for its color information.
    GridFont -      Determines where a header looks for its font information
    PopupMenu -     User pop-up menu of the header. For more information
                    see TGridView.DefaultHeaderMenu property.
    SectionHeight - Determines the height of the header sections.
    Sections -      Lists the header sections (column headings).
  }

  TCustomGridHeader = class(TPersistent)
  private
    FGrid: TCustomGridView;
    FSections: TGridHeaderSections;
    FSectionHeight: Integer;
    FAutoHeight: Boolean;
    FColor: TColor;
    FGridColor: Boolean;
    FFont: TFont;
    FGridFont: Boolean;
    FImages: TImageList;
    FImagesLink: TChangeLink;
    FFlat: Boolean;
    FPopupMenu: TPopupMenu;
    FOnChange: TNotifyEvent;
    procedure ImagesChange(Sender: TObject);
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsSectionHeightStored: Boolean;
    function IsSectionsStored: Boolean;
    procedure FontChange(Sender: TObject);
    function GetHeight: Integer;
    function GetMaxColumn: Integer;
    function GetMaxLevel: Integer;
    function GetWidth: Integer;
    procedure ReadFullSynchronizing(Reader: TReader);
    procedure SetAutoHeight(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetFlat(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetGridColor(Value: Boolean);
    procedure SetGridFont(Value: Boolean);
    procedure SetImages(Value: TImageList);
    procedure SetSections(Value: TGridHeaderSections);
    procedure SetSectionHeight(Value: Integer);
    procedure SetPopupMenu(const Value: TPopupMenu);
  protected
    procedure Change; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function GetOwner: TPersistent; override;
    procedure GridColorChanged(NewColor: TColor); virtual;
    procedure GridFontChanged(NewFont: TFont); virtual;
  public
    constructor Create(AGrid: TCustomGridView); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetNamePath: string; override;
    procedure SynchronizeSections;
    procedure UpdateSections; virtual;
    property Grid: TCustomGridView read FGrid;
    property Height: Integer read GetHeight;
    property MaxColumn: Integer read GetMaxColumn;
    property MaxLevel: Integer read GetMaxLevel;
    property Width: Integer read GetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property Color: TColor read FColor write SetColor stored IsColorStored default clBtnFace;
    property Images: TImageList read FImages write SetImages;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property GridColor: Boolean read FGridColor write SetGridColor default False;
    property GridFont: Boolean read FGridFont write SetGridFont default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Sections: TGridHeaderSections read FSections write SetSections stored IsSectionsStored;
    property SectionHeight: Integer read FSectionHeight write SetSectionHeight stored IsSectionHeightStored;
  end;

  TGridHeader = class(TCustomGridHeader)
  published
    property AutoHeight;
    property Color;
    property Images;
    property Flat;
    property Font;
    property GridColor;
    property GridFont;
    property PopupMenu;
    property Sections;
    property SectionHeight;
  end;

{ TCustomGridColumn }

  {
    Колонка таблицы.

    Свойства:

    Columns -     Ссылка на список колонок.

    AlignEdit -   Надо ли выравнивать текст в строке ввода в соответствии
                  с выравниванием текста колонки.
    Alignment -   Выравнивание текста колонки.
    AllowClick -  Можно или нет нажимать на заголовок колонки.
    AllowEdit -   Можно или нет показывать строку ввода на колонке.
    Caption -     Текст заголовка.
    CheckAlignment - Выравнивание флажка колонки.
    CheckKind -   Тип флажка колонки.
    EditMask -    Маска строки редактирования колонки.
    EditStyle -   Стиль строки ввода колонки.
    EditWordWrap - Разрешен ли автоматический перенос слов в строке ввода.
    FixedSize -   Ширина колонки постоянна (нельзя изменять мышкой в RunTime).
    MaxLength -   Максимальная длина редактируемого текста. Если значение
                  установлено в -1, то строка будет ReadOnly (в то время,
                  как сама ячейка - нет).
    MaxWidth -    Максимальная ширина колонки.
    MinWidth -    Минимальная ширина колонки.
    PickList -    Содержимое выпадающего списка.
    Tag -         Аналог Tag у TComponent.
    WantReturns - Может ли быть текст в ячейках содержать символы переноса
                  строки. Если WantReturns выставлено в False, то нажатие
                  клавиши ENTER в строке ввода будет игнорировано, симовлы
                  переноса будут отображаться как обычные символы.
    WordWrap -    Разрешен ли автоматический перенос слов текста ячейки.
    ReadOnly  -   Не редактируемая.
    TabStop -     Можно ли установить курсор на колонку.
    Title -       Ссылка на секцию заголовка колонки.
    Visible -     Видимость.
    Width -       Ширина колонки. Если колонка не видима, возвращает ноль.
    DefWidth -    Реальная ширина колонки. Не зависит от видимости столбца.
  }

  {
    Public Properties:

    Caption2 -       Return a fully qualified column header for a multi-level
                     header.
                     The result string consists of all top-level section headers,
                     separated by a dash.
    Columns -        Refers the list that contains the column.
    HeaderSection -  Refers to the header section of the column.

    Published Properties:

    AlignEdit -      Determines whether the text of the inplace editor will be
                     aligned same as the text of the column.
    Alignment -      Determines how the text is aligned within the cells.
    AllowClick -     Allows the column section to respond to mouse clicks at
                     runtime.
    AllowEdit -      Determines whether the inplace editor can be displayed
                     for a column.
    Caption -        Indicates the Title that represents the column header.
    CheckAlignment - Determines how the check box is aligned within the cell text.
    CheckKind -      Determines the style of the check box:
                       gcNone -        No check box.
                       gcCheckBox -    Like TCheckBox.
                       gcRadioButton - Like TRadioButton.
                       gcUserDefine -  User defined check image.
    DefWidth -       The real width of the column.
    EditMask -       Specifies the mask that represents what text is valid for
                     the inplace editor.
    EditStyle -      Determines the style of the inplace editor:
                       geSimple - Edit line.
                       geEllipsis - Edit line with ellipsis button.
                       gePickList - Edit line with drop-down list button.
                       geDataList - Edit line with lookup list button.
    EditWordWrap -   Determines whether the inplace editor inserts soft carriage
                     returns so text wraps at the right margin of the cell.
    FixedSize -      Specifies whether the column is sizeable.
    MaxLength -      Specifies the maximum number of characters the user can
                     enter into the inplace editor.
    MaxWidth -       Specifies the maximum width (in pixels) of the column.
    MinWidth -       Specifies the minimal width (in pixels) of the column.
    PickList -       Contains the lines of text in the inplace editor drop-down
                     list.
    ReadOnly  -      Determines whether data displayed in the column is editable.
    TabStop -        Determines if the user can tab to a column.
    Visible -        Specifies whether the column is visible in the grid.
    WantReturns -    Determines whether the user can insert return characters
                     into the text in the inplace editor.
    WordWrap -       Determines whether the grid inserts soft carriage returns
                     so text wraps at the right margin of the cell.
    Width -          The width of the column or 0 for invisible column.
  }

  TGridEditWordWrap = (ewAuto, ewEnabled, ewDisabled);

  TGridColumnClass = class of TCustomGridColumn;

  TCustomGridColumn = class(TCollectionItem)
  private
    FColumns: TGridColumns;
    FCaption: string;
    FWidth: Integer;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FFixedSize: Boolean;
    FMaxLength: Integer;
    FAlignment: TAlignment;
    FReadOnly: Boolean;
    FWantReturns: Boolean;
    FWordWrap: Boolean;
    FEditStyle: TGridEditStyle;
    FEditMask: string;
    FEditWordWrap: TGridEditWordWrap;
    FCheckKind: TGridCheckKind;
    FCheckAlignment: TAlignment;
    FTabStop: Boolean;
    FVisible: Boolean;
    FPickList: TStrings;
    FAllowClick: Boolean;
    FTag: Integer;
    FAlignEdit: Boolean;
    FAllowEdit: Boolean;
    FDefaultPopup: Boolean;
    function GetCaption2: string;
    function GetEditAlignment: TAlignment;
    function GetGrid: TCustomGridView;
    function GetHeaderSection: TGridHeaderSection;
    function GetPickList: TStrings;
    function GetPickListCount: Integer;
    function GetWidth: Integer;
    function IsPickListStored: Boolean;
    procedure ReadMultiline(Reader: TReader);
    procedure SetAlignEdit(Value: Boolean);
    procedure SetAllowEdit(Value: Boolean);
    procedure SetCheckAlignment(Value: TAlignment);
    procedure SetCheckKind(Value: TGridCheckKind);
    procedure SetEditWordWrap(Value: TGridEditWordWrap);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetPickList(Value: TStrings);
    procedure SetTabStop(Value: Boolean);
    procedure SetWantReturns(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  protected
    FWidthLock: Integer;
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
    procedure SetAlignment(Value: TAlignment); virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetEditMask(const Value: string); virtual;
    procedure SetEditStyle(Value: TGridEditStyle); virtual;
    procedure SetFixedSize(Value: Boolean); virtual;
    procedure SetMaxLength(Value: Integer); virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure SetWidth(Value: Integer); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AlignEdit: Boolean read FAlignEdit write SetAlignEdit default False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AllowClick: Boolean read FAllowClick write FAllowClick default True;
    property AllowEdit: Boolean read FAllowEdit write SetAllowEdit default True;
    property Caption: string read FCaption write SetCaption;
    property Caption2: string read GetCaption2;
    property CheckAlignment: TAlignment read FCheckAlignment write SetCheckAlignment default taLeftJustify;
    property CheckKind: TGridCheckKind read FCheckKind write SetCheckKind default gcNone;
    property Columns: TGridColumns read FColumns;
    property DefaultPopup: Boolean read FDefaultPopup write FDefaultPopup default True;
    property EditAlignment: TAlignment read GetEditAlignment;
    property EditMask: string read FEditMask write SetEditMask;
    property EditStyle: TGridEditStyle read FEditStyle write SetEditStyle default geSimple;
    property EditWordWrap: TGridEditWordWrap read FEditWordWrap write SetEditWordWrap default ewAuto;
    property FixedSize: Boolean read FFixedSize write SetFixedSize;
    property Grid: TCustomGridView read GetGrid;
    property HeaderSection: TGridHeaderSection read GetHeaderSection;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 10000;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property PickList: TStrings read GetPickList write SetPickList stored IsPickListStored;
    property PickListCount: Integer read GetPickListCount;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property TabStop: Boolean read FTabStop write SetTabStop default True;
    property Tag: Integer read FTag write FTag default 0;
    property Visible: Boolean read FVisible write SetVisible;
    property WantReturns: Boolean read FWantReturns write SetWantReturns default False;
    property Width: Integer read GetWidth write SetWidth stored False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property DefWidth: Integer read FWidth write SetWidth;
  end;

  TGridColumn = class(TCustomGridColumn)
  published
    property AlignEdit;
    property Alignment;
    property AllowClick;
    property AllowEdit;
    property Caption;
    property CheckAlignment;
    property CheckKind;
    property DefaultPopup;
    property EditMask;
    property EditStyle;
    property EditWordWrap;
    property FixedSize default False;
    property MaxLength;
    property MaxWidth;
    property MinWidth;
    property PickList;
    property ReadOnly;
    property TabStop;
    property Tag;
    property Visible default True;
    property WantReturns;
    property Width default 64;
    property WordWrap;
    property DefWidth default 64;
  end;

{ TGridColumns }

  {
    Список колонок таблицы.

    Процедуры:

    Add -     Добавить колонку.

    Свойства:

    Columns - Список колонок.
    Layout -  Строка состояния колонок. Содержит список чисел, разделенных
              запятой, определяющий видимость и ширину каждой колонки.
              Подходит для сохранения раскладки колонок в реестр или INI
              файл.
    Grid -    Ссылка на таблицу.
  }

  {
    Public Methods:

    Add -     Creates a new TGridColumn instance and adds it to the
              Columns array.

    Public Properties:

    Columns - Lists the columns in the collection.
    Layout -  Returns a string with a comma-separated list of column widths.
              Suitable for saving column layouts in the registry or INI file.
    Grid -    Indicates the grid control to which the columns instance
              belongs.
  }

  TGridColumns = class(TCollection)
  private
    FGrid: TCustomGridView;
    FOnChange: TNotifyEvent;
    function GetColumn(Index: Integer): TCustomGridColumn;
    function GetLayout: string;
    procedure SetColumn(Index: Integer; Value: TCustomGridColumn);
    procedure SetLayout(const Value: string);
  protected
    function GetOwner: TPersistent; override;
    procedure Update({%H-}Item: TCollectionItem); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AGrid: TCustomGridView); virtual;
    function Add: TCustomGridColumn;
    property Columns[Index: Integer]: TCustomGridColumn read GetColumn write SetColumn; default;
    property Grid: TCustomGridView read FGrid;
    property Layout: string read GetLayout write SetLayout;
  end;

{ TCustomGridRows }

  {
    Строки таблицы.

    Свойства:

    MaxCount -    Максимально допустимое количество строк в таблице. Зависит
                  от высоты строки.

    AutoHeight -  Автоматически подбирать высоту строк.
    Count -       Количество строк в таблице.
    Grid -        Ссылка на таблицу.
    Height -      Высота одной строки. При установленном свойстве AutoHeight
                  выбирается с учетом наличия флажков, высоты картинок,
                  высоты шрифта таблицы, высоты шрифта, 3D вида, значения
                  свойства Fixed.GridColor, вертикального смещения текста
                  ячейки, наличия сетки.
                  ПРИМЕЧАНИЕ: Если высота строки меньше, чем высота текста
                  ячейки, возможны глюки с отрисовкой кнопки строки ввода
                  таблицы. Соотвествие высоты строки и шрифта таблицы
                  следует отслеживать вручную или установить AutoHeight в
                  True.

    События:

    OnChange -        Событие на изменение параметров.
  }

  {
    Public Properties:

    Grid -       Indicates the grid control to which the rows instance belongs.
    MaxCount -   The maximum number of rows allowed in a grid. Depends on
                 row height.

    Published Properties:

    AutoHeight - Determines whether the height of the rows automatically
                 resizes to accommodate the grid font.
    Count -      Specifies the number of rows in the grid.
    Height -     Gives the height (in pixels) of all rows in the grid.
  }

  TCustomGridRows = class(TPersistent)
  private
    FGrid: TCustomGridView;
    FCount: Integer;
    FHeight: Integer;
    FAutoHeight: Boolean;
    FOnChange: TNotifyEvent;
    function GetMaxCount: Integer;
    function IsHeightStored: Boolean;
    procedure SetAutoHeight(Value: Boolean);
    procedure SetHeight(Value: Integer);
  protected
    procedure Change; virtual;
    procedure SetCount(Value: Integer); virtual;
  public
    constructor Create(AGrid: TCustomGridView); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property Count: Integer read FCount write SetCount default 0;
    property Grid: TCustomGridView read FGrid;
    property Height: Integer read FHeight write SetHeight stored IsHeightStored;
    property MaxCount: Integer read GetMaxCount;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGridRows = class(TCustomGridRows)
  published
    property AutoHeight;
    property Count;
    property Height;
  end;

{ TCustomGridFixed }

  {
    Параметры фиксированныех колонок таблицы.

    Count -     Количество фиксированных столбцов. Не может быть больше, чем
                количество столбцов таблицы минус 1.
    Color -     Цвет фиксированной части.
    Flat -      Вид отображения фиксированных ячеек. Если Flat = True, то
                ячейки отображаются плоскими, иначе - в виде кнопок.
    Font -      Шрифт текста ячеек.
    GridColor - Брать ли в качестве цвета фиксированных цвет таблицы.
                ПРИМЕЧАНИЕ: если GridColor = True, то сетка фиксированных
                ячеек рисуется одинарными линимя, как у обычных ячеек. В
                противном случае сетка - двойная разделительная линия, как
                у заголовка
    GridFont -  Брать ли в качестве шрифта фиксированных шрифт таблицы.
    ShowDivider - Рисовать разделительную линию между фиксированными и
                таблицей.

    События:

    OnChange -  Событие на изменение параметров.
  }

  {
    Public Properties:

    Grid -       Indicates the grid control to which the fixed instance belongs.

    Published Properties:

    Color -       Specifies the background color of the fixed columns.
    Count -       Specifies the number of columns on the left of the grid that
                  cannot be scrolled.
    Flat -        Specifies whether the fixed cells are flat.
    Font -        Controls the attributes of text written in the fixed columns.
    GridColor -   Determines where a fixed columns looks for its color information.
    GridFont -    Determines where a fixed columns looks for its font information.
    ShowDivider - Determines whether to draw a vertical dividing line between
                  fixed cells and scrollable cells.
  }

  TCustomGridFixed = class(TPersistent)
  private
    FGrid: TCustomGridView;
    FCount: Integer;
    FColor: TColor;
    FGridColor: Boolean;
    FFont: TFont;
    FGridFont: Boolean;
    FFlat: Boolean;
    FShowDivider: Boolean;
    FOnChange: TNotifyEvent;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    procedure FontChange(Sender: TObject);
    procedure SetColor(Value: TColor);
    procedure SetFlat(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetGridColor(Value: Boolean);
    procedure SetGridFont(Value: Boolean);
    procedure SetShowDivider(Value: Boolean);
  protected
    procedure Change; virtual;
    procedure GridColorChanged(NewColor: TColor); virtual;
    procedure GridFontChanged(NewFont: TFont); virtual;
    procedure SetCount(Value: Integer); virtual;
  public
    constructor Create(AGrid: TCustomGridView); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Color: TColor read FColor write SetColor stored IsColorStored default clBtnFace;
    property Count: Integer read FCount write SetCount default 0;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Grid: TCustomGridView read FGrid;
    property GridColor: Boolean read FGridColor write SetGridColor default False;
    property GridFont: Boolean read FGridFont write SetGridFont default True;
    property ShowDivider: Boolean read FShowDivider write SetShowDivider default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGridFixed = class(TCustomGridFixed)
  published
    property Color;
    property Count;
    property Flat;
    property Font;
    property GridColor;
    property GridFont;
    property ShowDivider;
  end;

{ TGridScrollBar }

  {
    Полоса прокрутки таблицы.

    Процедуры:

    Change -        Вызывается стразу после изменения позиции.
    Scroll -        Вызывается непосредственно перед изменением позиции для
                    корректировки нового положения.
    ScrollGrid -    Скроллировать изображение сетки. Вызывается при смене
                    позиции движка.
    ScrollMessage - Обработать сообщение Windows о нажатии на скроллер.
    SetParams -     Установить пределы.
    SetPosition -   Установить позицию.
    SetPositionEx - Установить позицию.

    Свойства:

    Grid -           Ссылка на таблицу.
    Kind -           Вид скроллера (горизонтальный или вертикальный).
    LineStep -       Малый шаг.
    LineSize -       Размер скроллируемой части окна при смещении на 1
                     позицию.
    Min -            Минимум.
    Max -            Максимум.
    PageStep -       Большой шаг.
    Position -       Текущая позиция.

    Tracking -       Признак синхронного изменения позиции при перемещении
                     мышкой центрального движка.
    Visible -        Видимость скроллера.

    События:

    OnChange -       Произошли изменения положения.
    OnChangeParams - Произошли изменения параметров (шаг, максимум).
    OnScroll -       Вызывается непосредственно перед изменением положения
                     скроллера.
  }

  {
    Public Properties:

    Grid -     Indicates the grid control to which the scroll bar
               instance belongs.
    Kind -     Specifies whether the scroll bar is horizontal or vertical.
    LineStep - Determines how much Position changes when the user clicks
               the arrow buttons on the scroll bar or presses the arrow
               keys on the keyboard.
    LineSize - Determines how far the grid moves when the user clicks one
               of the small end arrows on the scroll bar.
    Min -      Specifies the minimum position represented by the scroll bar.
    Max -      Specifies the maximum position represented by the scroll bar.
    PageStep - Determines how much Position changes when the user clicks
               the scroll bar on either side of the thumb tab or presses
               PgUp or PgDn.
    Position - Indicates the current position of the scroll bar.

    Published Properties:

    Tracking - Determines whether the grid moves before the thumb tab
               is released.
    Visible -  Determines whether the scroll bar appears on the grid.
  }

  TGridScrollEvent = procedure(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer) of object;

  TGridScrollBar = class(TPersistent)
  private
    FGrid: TCustomGridView;
    FKind: TScrollBarKind;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FPageStep: Integer;
    FLineStep: Integer;
    FLineSize: Integer;
    FTracking: Boolean;
    FVisible: Boolean;
    FUpdateLock: Integer;
    FOnScroll: TGridScrollEvent;
    FOnChange: TNotifyEvent;
    FOnChangeParams: TNotifyEvent;
    procedure SetLineSize(Value: Integer);
    procedure SetLineStep(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPageStep(Value: Integer);
    procedure SetVisible(Value: Boolean);
  protected
    FBarCode: Integer;
    procedure Change; virtual;
    procedure ChangeParams; virtual;
    procedure Scroll(ScrollCode: Integer; var ScrollPos: Integer); virtual;
    procedure ScrollMessage(var Message: TLMScroll); virtual;
    procedure SetParams(AMin, AMax, APageStep, ALineStep: Integer); virtual;
    procedure SetPosition(Value: Integer);
    procedure SetPositionEx(Value: Integer; ScrollCode: Integer); virtual;
    procedure Update; virtual;
  public
    constructor Create(AGrid: TCustomGridView; AKind: TScrollBarKind); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure LockUpdate;
    procedure UnLockUpdate;
    property Grid: TCustomGridView read FGrid;
    property Kind: TScrollBarKind read FKind;
    property LineStep: Integer read FLineStep write SetLineStep;
    property LineSize: Integer read FLineStep write SetLineSize;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property PageStep: Integer read FPageStep write SetPageStep;
    property Position: Integer read FPosition write SetPosition;
    property UpdateLock: Integer read FUpdateLock;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeParams: TNotifyEvent read FOnChangeParams write FOnChangeParams;
    property OnScroll: TGridScrollEvent read FOnScroll write FOnScroll;
  published
    property Tracking: Boolean read FTracking write FTracking default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

{ TGridListBox }

  {
    Выпадающий список строки редактирования.
  }

  TGridListBox = class(TCustomListBox)
  private
    FGrid: TCustomGridView;
    FSearchText: String;
    FSearchTime: UInt64;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Grid: TCustomGridView read FGrid;
  end;

{ TCustomGridEdit }

  {
    Сторка ввода таблицы. Может содержать кнопку с многоточием или кнопку с
    выпадающим списокм.

    Процедуры:

    GetDropList -      Получить выпадающий список строки редактирования в
                       соотвествии с типом строки. Функцию следует перекрыть
                       для получения создания списка другого класса,
                       отличного от TGridListBox. Например, наследник
                       TDBGridView вовзращает список TDBLookupListBox для
                       строки типа TDataList.
    PaintButton -      Рисовать кнопку.
    UpdateBounds -     Определение положения и размера строки с последующим
                       показом ее (вызывается непосредственно из метода
                       Show).
    UpdateColors -     Определение цвета ячейки и шрифта.
    UpdateContents -   Определение текста строки, максимальной длины строки
                       и возможности редактирования.
    UpdateList -       Настройка внешнего вида выпадащенго списка.
    UpdateListBounds - Определение положения и размера выпадающего списка.
    UpdateListItems -  Заполнение текущего выпадающего списка значениями.
    UpdateListValue -  Установка выбранного значения выпадающего списка.
                       Вызывается из метода CloseUp при выборе значения.
    UpdateStyle -      Определение типа строки.

    CloseUp -          Закрыть выпадающий список.
    DropDown -         Открыть выпадающий список (только для строки с типом
                       gePickList, geDataList).
    Press -            Нажатие на кнопку с многоточием (мышкой или нажат
                       Ctrl+Enter при WantReturns = False).
    SelectNext -       Следует выбрать следующее значение из списка (нажат
                       Ctrl+Enter при WantReturns = False).

    Свойства:

    ButtonRect -       Прямоугольник кнопки.
    ButtonWidth -      Ширина кнопки.
    DropDownCount -    Количество строк в выпадающем списке.
    DropList -         Текущий выпадающий список строки. Может не совпадать
                       с DropListBox дл янаследников (например, для Lookup
                       ячеек в TDBGridView).
    DropListBox -      Собственный выпадающий список строки.
    DropListVisible -  Видимость выпадающего списка.
    EditStyle -        Тип строки. Может принимать следующие значения:
                         geSimple -      простая строка редактирования.
                         geEllipsis -    строка с кнопкой ... (с многоточием)
                         gePickList -    строка с кнопкой выпадающего списка
                         geDataList -    зарезервированно для дальнейших
                                         DB версий.
                         geUserDefine -  строка с кнопкой пользователя.
    Grid -             Ссылка на кнопку.
    LineCount -        Количество строк в строке.
    WantReturns -      Может ли текст в строке содержать символы переноса.
    WordWrap -         Разрешен ли автоматический перенос слов.
  }

  {
    Protected Methods:

    ApplyListValue -   Calls the EditCloseUp grid event and sets selected item text
                       to the edit. Called internally when the drop-down list
                       closes up.
    GetDropList -      Creates the popup list instance.

    Public Methods:

    CloseUp -          Call CloseUp to programmatically close the popup list
                       of the edit. The Accept parameter determines whether to
                       modify the cell value with the value that corresponds to
                       the selected value in the drop-down list.
    DropDown -         Opens popup list so that the user can choose a lookup
                       value. Only for editor with style gePickList, geDataList.
    Press -            Use Press to handle button presses. Called when the user
                       hit Ctrl+Enter or presses a button with the mouse. By
                       default generates an OnEditButtonPress event.
    SelectNext -       Use SelectNext to select the next popup list value.
                       Called when user hit Ctrl+Enter or double click on editor
                       with closed popup list. By default generates an
                       OnEditSelectNext event.

    Public Properties:

    DropDownCount -    Specifies the number of lines displayed in the popup list.
    DropListVisible -  Determines whether the popup list is currently in the
                       open (dropped-down) position.
    EditStyle -        Indicates the current style of the editor:
                         geSimple -   Edit line.
                         geEllipsis - Edit line with ellipsis button.
                         gePickList - Edit line with drop-down list button.
                         geDataList - Edit line with lookup list button.
    Grid -             Indicates the grid control to which the editor instance
                       belongs.
    PickList -         Indicated the current popup list instance.
    WantReturns -      Determines whether the user can insert return characters
                       into the text in the editor.
    WordWrap -         Determines whether the editor inserts soft carriage
                       returns so text wraps at the right margin of the edit.
  }

  TGridEditClass = class of TCustomGridEdit;

  TCustomGridEdit = class(TCustomMaskEdit)
  private
    FGrid: TCustomGridView;
    FClickTime: UInt64;
    FEditStyle: TGridEditStyle;
    FWantReturns: Boolean;
    FWordWrap: Boolean;
    FDropDownCount: Integer;
    FDropListVisible: Boolean;
    FCloseUpCount: Integer;
    FPressCount: Integer;
    FPickListBox: TGridListBox;
    FActiveList: TWinControl;
    FButtonWidth: Integer;
    FButtonTracking: Boolean;
    FButtonPressed: Boolean;
    FButtonHot: Boolean;
    FDefocusing: Boolean;
    FAlignment: TAlignment;
    function GetButtonRect: TRect;
    function GetClosingUp: Boolean;
    function GetLineCount: Integer;
    function GetPressing: Boolean;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure SetAlignment(Value: TAlignment);
    procedure SetButtonWidth(Value: Integer);
    procedure SetDropListVisible(Value: Boolean);
    procedure SetEditStyle(Value: TGridEditStyle);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure LMGetDlgCode(var Message: TLMGetDlgCode); message LM_GETDLGCODE;
    procedure LMPaste(var Message); message LM_PASTE;
    procedure LMCut(var Message); message LM_CUT;
    procedure LMClear(var Message); message LM_CLEAR;
    procedure LMCancelMode(var Message); message LM_CANCELMODE;
    procedure LMKillFocus(var Message); message LM_KILLFOCUS;
    procedure LMWindowPosChanged(var Message); message LM_WINDOWPOSCHANGED;
    procedure LMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure LMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure LMLButtonDblClk(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure LMSetCursor(var Message); message LM_SETCURSOR;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE{%H-}; // NOTE: !! not implemented !!
    procedure CMEnabledChanged(var Message); message CM_ENABLEDCHANGED;
    procedure LMContextMenu(var Message: TLMessage); message LM_CONTEXTMENU;
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message); message CM_MOUSELEAVE;
    procedure ApplyListValue(Accept: Boolean); virtual;
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure VisibleChanged; override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function EditCanModify: Boolean; override;
    function GetDropList: TWinControl; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintButton(DC: HDC; Rect: TRect); virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure StartButtonTracking(X, Y: Integer);
    procedure StepButtonTracking(X, Y: Integer);
    procedure StopButtonTracking;
    procedure UpdateBounds(ScrollCaret: Boolean); virtual;
    procedure UpdateColors; virtual;
    procedure UpdateContents; virtual;
    procedure UpdateList; virtual;
    procedure UpdateListBounds; virtual;
    procedure UpdateListItems; virtual;
    procedure UpdateStyle; virtual;
    procedure WndProc(var Message: TLMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure Deselect;
    procedure DropDown;
    procedure Invalidate; override;
    procedure InvalidateButton;
    procedure Press;
    procedure SelectNext;
    procedure Undo; override;
    property ActiveList: TWinControl read FActiveList;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ButtonRect: TRect read GetButtonRect;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property ClosingUp: Boolean read GetClosingUp;
    property Color;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount;
    property DropListVisible: Boolean read FDropListVisible write SetDropListVisible;
    property EditStyle: TGridEditStyle read FEditStyle write SetEditStyle;
    property Font;
    property Grid: TCustomGridView read FGrid;
    property LineCount: Integer read GetLineCount;
    property MaxLength;
    property Pressing: Boolean read GetPressing;
    property WantReturns: Boolean read FWantReturns write FWantReturns;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
  end;

  TGridEdit = class(TCustomGridEdit);

{ TGridTipsWindow }

  TGridTipsWindowClass = class of TGridTipsWindow;

  TGridTipsWindow = class(THintWindow)
  private
    FGrid: TCustomGridView;
    procedure CMTextChanged(var {%H-}Message); message CM_TEXTCHANGED;
    procedure LMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override; 
  end;

{ TGridFindDialog }

  TGridFindDialog = class(TFindDialog)
  private
    FVisible: Boolean;
  public
    procedure DoClose; override;
    procedure DoShow; override;
    procedure ShowModal;
    property Visible: Boolean read FVisible;
  end;

{ TCustomGridView }

  {
    Таблица.

    Процедуры:

    AcquireFocus -         Установка фокуса на талицу или строку ввода.
                           Возвращает False, если по каим либо причинам фокус
                           не установлен.
    CancelCellTips -       Отключение видимой подсказки (CellTips) ячейки. Т.к.
                           подсказки выполнены на основе Hint, то для ее
                           отключения используется Application.CancelHint, но
                           только предварительно убедившись, что Hint сейчас
                           показывается действительно у данной таблицы.
    CellClick -            Нажатие машкой на ячейке.
    Change -               Ячейка выдлена.
    Changing -             Ячейка выделяется.
    CheckClick -           Нажатие машкой на флажке ячейки.
    ColumnAutoSize -       Иширина колонка изменяется на ширину, расчитанную
                           автоматически по ширине текста.
    ColumnResizing -       Изменяется ширина колонки.
    ColumnResize -         Изменена ширина колонки.
    CreateColumn -         Создать колонку. Данная функция используется
                           для создания колонок другого класса, отличного от
                           TGridColumn.
    CreateColumns -        Создать список колонок таблицы. Функцию следует
                           использовать для создания колонок другого класса,
                           отличного от TGridColumns.
    CreateEdit -           Создать строку редактирования указанного типа или
                           стандартного класса TGridEdit, если класс не
                           указан. Для указания типа строки ввода таблицы,
                           отличного от стандартного, следует перекрыть
                           функцию GetEditClass.
                           ПРИМЕЧАНИЕ: Функцией создания строки ввода
                           следует пользоваться только для только для
                           выполнения дополнительных действий, необходимых
                           для корректного создания строки (т.е.
                           непосредственно после вызова конструктора). Если
                           необходимо только изменить класс строки, следует
                           перекрыть функцию GetEditClass. 
                           ВНИМАНИЕ: На момент создания EditCell может еще
                           не указывает на ячейку редактирования. Для
                           определения будущей ячейки редактирования следует
                           использовать CellFocused.
    CreateFixed -          Создать список фиксированных колонок. Функцию
                           следует использовать для создания фиксированных
                           другого класса, отличного от TGridFixed.
    CreateHeader -         Создать заголовок. Функцию следует использовать
                           для создания заголовка другого класса, отличного
                           от TGridHeader.
    CreateHeaderSection -  Создать секцию заголовка. Данную функцию следует
                           использовать для создания секций заголовка другого
                           класса, отличного от TGridHeaderSection.
    CreateRows -           Создать список строк. Данную функцию следует
                           использовать для создания строк другого класса,
                           отличного от TGridRows.
    CreateScrollBar -      Создать полосу прокрутки. Функцию следует
                           использовать для создания полосы прокрутки другого
                           класса, отличного от TGridScrollBar.
    EditButtonPress -      Произошло нажатие на кнопке строки редактирования.
    EditCanAcceptKey -     Можно ли вводить указанный символ в ячейку.
    EditCanceled -         Строка редактирования погашена в результате
                           нажатия клавиши Escape.
    EditCanModify -        Можно ли редактировать текст в ячейке. Вызывается
                           непосредственно перед производимым изменением в
                           строке ввода (нажатие клавиши и т.п.).
    EditCanShow -          Может ли быть показана строка ввода. По умолчанию
                           соответствует значению свойтва Column.AllowEdit.
    EditCanUndo -          Может ли строка ввода выполнять операцию Undo.
    EditChange -           Изменился текст в строке ввода.
    EditCloseUp -          Закрытие выпадающего списка с выбором значения.
    EditSelectNext -       Нажат Ctrl+Enter на кнопке с нераскрытым списком.
                           В параметр Value следует вернуть новое значение
                           для строки ввода.
    GetCellColors -        Установка цветов ячейки в зависимости от фокуса,
                           выделения и т.п.
    GetCellImage -         Вернуть номер картинки ячейки. Если картинок нет,
                           обязательно следует в вернуть -1.
    GetCellImageRect -     Прямоугольник картинки ячейки относительно левого
                           верхнего угла таблицы.
    GetCellHintRect -      Вернуть прямоугольника границы подсказки ячейки.
                           Если текст не помещяется в эти границы, то
                           показывается подсказка ячейкт (CellTips).
    GetCellText -          Вернуть текст ячейки.
    GetCellTextBounds -    Вычислить прямоугольник текста ячейки. Для
                           вычисления использует GetTextRect и GetCellText.
    GetCellTextIndent -    Вернуть смещение текста ячейки по горизонтали
                           относительно леавого и верхнего края прямоугольника
                           редактирования. Смещение слева по умолчанию равно
                           2 пиксела, если есть флажок или картинка, и
                           TextLeftIndent пикселов, если в ячейке только
                           текст. Смещение сверху умолчанию равно
                           TextTopIndent плюс два пиксела, если фиксированные
                           ячейки имеют вид кнопки (not Flat).
    GetCheckAlignment -    Вернуть выравнивание флажка ячейки. Допускаются
                           следующие значения:
                             taLeftJustify - флажок слева.
                             taCenter -      флажок по центру ячейки.
    GetCheckImage -        Венруть картинку ячейки.
                           Картинка должна быть желательно с прозрачным фоном.
    GetCheckKind -         Вернуть тип флажка ячейки. Резальтат может
                           принимать следующие значения:
                             gcNone -        Ячейка не сожержит флажка.
                             gcCheckBox -    Ячейка содержит флажок как у
                                             компонента TCheckBox.
                             gcRadioButton - Ячейка содержит флажок как
                                             у компонента TRadioButton.
                             gcUserDefine -  Ячейка содержит флажок, вид
                                             которого определяется
                                             пользователем. Для определения
                                             вида используется событие
                                             OnGetCheckImage.
    GetCheckRect -         Получить прямоугольник флажка ячейки в координатах
                           относительно таблицы.
    GetCheckState -        Вернуть состояние влажка ячейки.
    GetColumnClass -       Вернуть класс колонок таблицы. Используется для
                           переопределения класса колонок у таблиц-наследников
                           (например TDBGridView) для корректной работы
                           редакторов Delphi IDE.
    GetCursorCell -        Найти ячейку, на которую можно установить курсор.
                           В качестве параметров передается ячейка и смещение
                           от этой ячейки. Если полученная после смещения
                           ячейка не может принимать фокус, в указанном
                           направлении  ищется следующая доступная ячейка.
                           Параметр смещения может принимать следующие
                           значения:
                             goLeft -     Сместиться от указанной на одну
                                          колонку влево.
                             goRight -    Сместиться от указанной на одну
                                          колонку вправо.
                             goUp -       Сместиться от указанной на одну
                                          колонку вверх.
                             goDown -     Сместиться от указанной на одну
                                          колонку вниз.
                             goPageUp -   Сместиться от указанной на
                                          страницу вверх.
                             goPageDown - Сместиться от указанной на
                                          страницу вниз.
                             goHome -     Найти первую доступную ячейку на
                                          указанной строке.
                             goEnd -      Найти последнюю доступную ячейку
                                          на указанной строке.
                             goGridHome - Сместиться в верхний левый угол
                                          таблицы.
                             goGridEnd -  Сместиться в нижний правый угол
                                          таблицы.
                             goSelect -   Выделить указанную ячейку. Если
                                          ячейка не доступна, найти
                                          подходящую на той же строке или в
                                          той же колонке.
                             goFirst -    Найти первую попавшуюся доступную
                                          ячейку.
                             goNext -     Найти следующую ячейку. Поиск идет
                                          вправо от указанной ячейки до края,
                                          потом переходит на строку вниз и
                                          т.д. 
                             goPrev -     Найти предыдущую ячейку. Поиск
                                          идет от указанной ячейки влево до
                                          края, потом переходит на строку
                                          вверх и т.д.
    GetEditClass -         Вернуть класс строки редактирования для указанной
                           ячейки. Если класс строки не совпадает с тем, что
                           был раньше, то создается новая строка указанного
                           класса (по аналогии с тем, как это делается в
                           TApplication.ActivateHint). Т.о. на каждую ячейку
                           можно сделать свою строку редактирования.
    GetEditList -          Заполнить выпадающий список строки редактирования.
    GetEditListBounds -    Подправить положение выпадающего списка строки
                           редактирования.
    GetEditMask -          Вернуть маску ввода для редактирования. По
                           умолчанию маски нет.
    GetEditStyle -         Вернуть стиль сроки редактирования.
    GetEditText -          Вернуть текст ячейки для редактирования. По
                           умолчанию возвращает текст ячейки.
    GrtGridLineColor -     получить цвет линии сетки.
    GetHeaderColors -      Установка цветов секции заголовка.
    GetHeaderImage -       Вернуть номер картинки секции заголовка. Если
                           картинок нет, обязательно следует в вернуть -1.
    GetSortDirection -     Определить направление сортировки для указанного
                           столбца. Результат может принимать следующие
                           значения:
                             gsNone -       Нет сортировки.
                             gsAscending -  Сортировка по возрастанию.
                             gsDescending - Сортировка по убыванию.
    GetSortImage -         Вернуть картинку сортировки для указанной колонки.
    GetTextRect -          Универсальная процедура вычисления прямоугольника
                           текста для ячейки. Не привязана к полотну таблицы
                           и ячейке. Всегда должна быть идентична
                           универсальной процедуре отрисовки текста.
    GetTipsRect -          Вычислить прямоугольник для показа подсказки с
                           текстом ячейки. Для вычисления использует
                           процедуру GetTextRect.
    GetTipsText -          Вернуть текст подсказки для указанной ячейки. По
                           умолчанию возвращается текст ячейки.
    GetTipsWindowClass -   Вернуть класс окна подсказки ячейки.
    HeaderClick -          Нажата секция заголовка.
    HeaderClicking -       Начинается нажатие на секцию заголовка.
    HideCursor -           Погасить курсор.
    HideEdit -             Скрыть строку редактирования.
    HideFocus -            Погасить прямоугольник фокуса.
    PaintCell -            Рисовать ячейку.
    PaintCells -           Рисовать ячейки.
    PaintFixed -           Рисовать фиксированные ячейки.
    PaintFixedGrid -       Рисовать сетку фиксированных ячеек.
    PaintFocus -           Рисовать прямоугольник фокуса.
    PaintFreeField -       Рисовать область, свободную от ячеек.
    PaintGridLines -       Рисовать линии сетки.
    PaintHeader -          Рисовать секцию заголовка.
    PaintHeaders -         Рисовать заголовок.
    PaintHeaderSections -  Рисовать секции заголовка.
    PaintResizeLine -      Рисовать линию при изменении размера колонки.
    PaintText -            Универсальная процедура отрисовки текста ячейки.
                           Не привязана к полотну таблицы и ячейке.
                           Используется также для отрисовки CellTips. В
                           случае перекрытия процедуры необходимо учесть
                           изменения при вычислении прямоугольника подсказки
                           (т.е. внутри функции GetTextRect).
    SetEditText -          Установть текст в ячейку из строки ввода.
                           Вызывается при смене ячейки, если только ячейка
                           не ReadOnly. Если текст не принимается, следует
                           вызвать исключение.
    SetGridCursor -        Установть курсор в указанную ячейку.
    ShowCursor -           Показать курсор.
    ShowEdit -             Показать строку редактирования.
    ShowEditChar -         Показать строку редактирования и передать ей
                           символ.
    ShowFocus -            Показать прямоугольник фокуса.
    StartColResize -       Начать изменение ширины колонки.
    StartHeaderClick -     Начать нажатие на заголовок.
    StepColResize -        Продолжить изменение ширины колонки.
    StepHeaderClick -      Продолжить нажатие на заголовок.
    StopColResize -        Завершить изменение ширины колонки.
    StopHeaderClick -      Завершить нажатие на заголовок.

    ApplyEdit -            Завершение редактирования с применением текста
                           ячейки. По умолчанию вызывает SetEditText.
    CancelEdit -           Отмена редактирования ячейки. В звисимости от
                           значения AlwaysEdit устанавливает текст строки
                           ввода на текст ячейки или гасит строку ввода без
                           вывода OnSetEditText.
    DrawDragRect -         Рисовать рамку фокуса на ячейке. Используется
                           для опметки ячейки при операции DragDrop.
    FindText -             Поиск ячейки с указанным текстом. Возвращает True,
                           если ячейка найдена.
    GetCellAt -            Найти ячейку по точке. Возвращает (-1, -1), если
                           ячейка не найдена.
    GetCellRect -          Прямоугольник ячейки относительно левого верхнего
                           угла таблицы.
    GetCellsRect -         Получить прямоугольник ячеек.
    GetColumnAt -          Найти колонку по точке. Возвращает -1, если
                           колонка не найдена.
    GetColumnMaxWidth -    Максмальная ширина видимого текста в указанной
                           колонке. Если отсутствует хотябы одна видимая
                           строка, возвращает ширину колонки.
    GetColumnRect -        Прямоугольник колонки.
    GetColumnsRect -       Прямоугольник колонок с указанную по указанную.
    GetColumnsWidth -      Суммарная ширина колонок с указанной по указанную.
    GetEditRect -          Вернуть прямоугольник для строки редактирования.
                           По умолчанию равен прямоугольнику ячейки за
                           вычетом флажка и картинки слева.
    GetFirstImageColumn -  Получить номер картинки для первой видимой
                           нефиксированной колонки. По умолчанию такая
                           колонка имеет картинку, остальные не имеют.
    GetFixedRect -         Прямоугольник фиксированных колонок.
    GetFixedWidth -        Суммарная ширина фиксированных колонок.
    GetFocusRect -         Прямоугольник фокуса. По умолчанию вычисляется с
                           учетом построчного выделения и наличия картинок.
    GetGridHeight -        Высота видимой части ячеек.
    GetGridOrigin -        Сдвиг ячеек относительно левого верхнего угла
                           видимого прямоугольника таблицы (т.е. исключая
                           заголовок) в пикселах. Определяется положением
                           движков скроллеров.
    GetGridRect -          Прямоугольник видимой части ячеек (в клиентских
                           координатах таблицы).
    GetHeaderHeight -      Высота заголовка. Равна 0, если не видим.
    GetHeaderRect -        Прямоугольник заголовка.
    GetHeaderSection -     Найти секцию заголовка указанного уровня для
                           указанной колонки. Если в качестве уровня указана
                           -1, то возвращается самая нижняя секция.
    GetResizeSectionAt -   Вернуть секцию заголовка, над правой границей
                           которой находится указанная точка.
    GetRowAt -             Найти строку по точке.
    GetRowRect -           Прямоугольник строки. 
    GetRowsRect -          Прямоугольник строк с указанную по указанную.
    GetRowsHeight -        Вернуть суммарную высоту строк от указанной до
                           указанной.
    GetSectionAt -         Получить секцию заголовка, которая содержит
                           указанную точку.
    InvalidateCell -       Перерисовать ячейку.
    InvalidateCheck -      Перерисовать флажок ячейки.
    InvalidateColumn -     Перерисовать колонки.
    InvalidateColumns -    Перерисовать колонки с указанной по указанную.
    InvalidateEdit -       Перерисовать строку ввода.
    InvalidateFixed -      Перерисовать фиксированные колонки.
    InvalidateFocus -      Перерисовать фокус (ячейку или строку).
    InvalidateGrid -       Перерисовать таблицу (все ачейки).
    InvalidateHeader -     Перерисовать заголовок.
    InvalidateRect -       Обновить прямоугольник таблицы.
    InvalidateRow -        Перерисовать строку.
    InvalidateRows -       Перерисовать строки с указанной по указанную.
    IsActiveControl -      Является ли таблица текущим активным компонентом
                           формы.
    IsCellAcceptCursor -   Можно ли установить курсор в ячейку.
    IsCellHighlighted -    Подсвечивается ли выделенная ячейка цветом курсора.
                           Соотвествует SellSelected and IsCellFocused().
    IsCellHasCheck -       Проверка на наличие флажка у ячейки.
    IsCellHasImage -       Проверка на наличие картинки у ячейки.
    IsCellFocused -        Проверка на попадание ячейки в фокус, а при
                           построчном выделении (RowSelect = True) - в
                           выделенную строку.
    IsCellReadOnly -       Можно или нет менять текст в строке ввода
                           указанной ячейки. 
    IsCellValid -          Проверка корректности ячейки (ненулевая ширина,
                           видимость, выход за границы столбцов и строк).
    IsCellVisible -        Проверка видимости ячеки на экране.
    IsColumnVisible -      Проверка видимости колонки ни экране.
    IsFocusAllowed -       Признак видимости фокуса. Фокус виден, если
                           разрешено выделение строки или запрещено
                           редактирвоание. В противном случае на месте
                           фокуса находится строка ввода.
    IsHeaderHasImage -     Проверка на наличие картинки у секции заголовка.
    IsHeaderPressed -      Проверка на нажатие на секцию заголовка. Возвращает
                           True, если сейчас идет нажатие на указанную
                           секцию заголовка.
    IsRowVisible -         Проверка видимости строки.
    LockUpdate -           Заблокировать перерисовку.
    MakeCellVisible -      Сдулать видимой указанную ячейку.
    UndoEdit -             Выполение операции Undo для встроенной строки
                           ввода. Вызывается при нажатии клавиши ESC в
                           случае, если строка ввода не убирается (т.е.
                           при AlwaysEdit = True).
    UnLockUpdate -         Разблокировать перерисовку.
    UpdateCursor -         Обновление положения курсора. Если курсор
                           находится на ячейке за пределами таблицы или там,
                           где фокус не разрешен - ищется первая попавшаяся
                           ячейка.
    UpdateColors -         Обновление цветов заголовка и фиксированных при
                           изменении цвета таблицы.
    UpdateEdit -           Обновление положения, текста строки ввода и ее
                           видимости в соотвествии с положением курсора.
    UpdateEditContents -   Обновление внешнего вида и текста видимой строки
                           ввода. Данный метод следует использовать в случае,
                           когда в ходе редактирования происходит обновление
                           данных таблицы, параметров колонок и т.п. и
                           требуется соотвественно обновить строку ввода.
    UpdateEditText -       Обновление данных таблицы из строки ввода.
    UpdateFixed -          Обновление параметров фиксированных (например,
                           количества в зависимости от количества колонок
                           таблицы).
    UpdateFocus -          Установить фокус на талицу, если это возможно.
    UpdateFonts -          Обновление шрифтов заголовка и фиксированных при
                           изменении шрифта таблицы.
    UpdateHeader -         Обновление заголовка (приведение в соотвествии с
                           колонками, если стоит флаг AutoSynchronize или
                           Synchronized).
    UpdateRows -           Обновление параметров строк (например, высоты
                           строки в зависимости от размера картинок и
                           размера шрифта таблицы).
    UpdateScrollBars -     Обновление настроек полос прокрутки (максимум,
                           большой шаг, малый шаг).
                           ПРИМЕЧАНИЕ: Вертикальный скроллер настраивается
                           на число строк, горизонтальный - на ширину
                           нефиксированных колонок.
    UpdateScrollPos -      Установка положения движков полос прокрутки в
                           соответствии с текущей выделенной ячейкой.
    UpdateSelection -      Провека возможности выделения указанной ячейки.
                           Если ячейка не может быть выделена, то ищется
                           ближайшая доступная для выделения.
    UpdateText -           То же самое, что и UpdateEditText.
    UpdateVisOriginSize -  Обновление настроек видимой области ячеек по
                           положению движков полос прокрутки.

    Свойства:

    AllowEdit -            Может ли быть видима строка редактирования. 
                           ПРИМЕЧАНИЕ: Хотя строка моджет быть видима, можно
                           или нет редактировать текст ячейки определяется
                           значением свойств ReadOnly колонки и всей таблицы.
    AllowSelect -          Разрешено ли выделение ячеек (строк) цветом (т.е.
                           наличие фокуса на текущей ячейке).
    AlwaysSelected -       Всегда показывать фокус выделенным.
    BorderStyle -          Вордюр таблицы.
    CellFocused -          Текущее положение курсора.
    Cells -                Доступ к содержимому ячейки.
    CellSelected -         Признак выделенности курсора.
    CheckBoxes -           Отображение флажков ячеек.
    CheckStyle -           Стиль флажков ячеек. Может принимать следующие
                           значения:
                             csFlat -  Плоские флажки.
                             cs3D -    Выпуклые флажки.
                             csWin95 - Флажки в стиле Windows 95.
    Col -                  Текущая колонка ячейки фокуса. Равно CellFocused.Col.
    ColumnClick -          Можно или нет нажимать заголовок колонки.
    Columns -              Список колонок таблицы.
    ColumnsFullDrag -      Синхронно изменять ширину колонки при
                           перетаскивании за границу заголовка.
    CursorKeys -           Клавиши перемещения курсора по ячейкам. Может быть
                           комбинацей из следующих значений:
                             gkArrows -     Навигация по ячейкам стрелками.
                             gkTabs -       Навигация по ячейкам с помошью.
                                            клавиш TAB и SHIFT-TAB
                             gkReturn -     Переход на следующую ячейку после
                                            ввода текста в строку ввода
                                            нажатием клавиши Return.
                             gkMouse -      Выделение ячейки мышкой.
                             gkMouseMove -  Синхронное выделение ячейки при
                                            перемещении мышки с нажатой
                                            кнопкой.
                             gkMouseWheel - Навигация по ячейкам с помощью
                                            колесика мышки.
    DefaultEditMenu -      Показывать стандартное Popup меню (по правой
                           кнопке мыши) для строки ввода. Если свойство
                           выставлено в False, то для строки будет показано
                           меню таблицы.
    Edit -                 Строка редактирования.
    EditCell -             Ячейка редактирования.
    EditColumn -           Колонка, в которой находится ячейка редактирования.
    EditDropDown -         Управление видимостью выпадающего списка. При
                           установлении значения в True переводит ячейку в
                           режим редактирования и показывает список (если
                           это возможно).  
    Editing -              Признак редактирование ячейки. Позволяет начать
                           или завершить процесс редактирования ячейки.
                           ПРИМЕЧАНИЕ: При Editing := True ставит фокус на
                           строку таблицу.
    EndEllipsis -          Выводить или нет непомещающийся в ячейке текст с
                           ... (многоточием) на конце. Работает только для
                           однострочного текста с выравниваем влево. Сильно
                           влияет на скорость отрисовки.
    Fixed -                Параметры фиксированных колонок.
    FlatBorder -           Отображать "плоский" бордюр (как Bevel). 
    FlatScrollBars -       Отображать плоские скроллеры (только дял
                           comctrl32.dll версии 4.71 и выше).
    FocusOnScroll -        Забирать на себя фокус или нет при скроллировании
                           таблицы. Если FocusOnScroll = False, то
                           скроллирование мышкой за ползунок не приведет к
                           фокусированию таблицы. Если сделать FocusOnScroll
                           = True, то при установки активной ячейки со
                           скроллировнием извне (например, при управлении
                           другим компонентом), произойдет фокусировка
                           таблицы.
    GridColor -            Цвет линий сетки.
    GridLines -            Рисовать или нет линии сетки.
    GridStyle -            Стиль сетки. Может быть комбинацей из следующих
                           значений:
                             gsHorzLine -     Рисовать горизонтальные линии.
                             gsVertLine -     Рисовать вертикальные линии.
                             gsFullHorzLine - Горизонтальные линии до правого
                                              края таблицы, а не до последней
                                              колонки.
                             gsFullVertLine - Вертикальные линии до нижнего
                                              края таблицы, а не до последней
                                              строки (если Rows.Copunt > 0).
                             gsListViewLike - Рисовать горизонтальные линии
                                              на всю таблицу независимо от
                                              того, сколько строк в таблице.
                             gsDotLines -     Рисовать линии сетки точечками
                                              (красиво, но долго).
    Header -               Заголовок.
    HideSelection -        Гасить или нет курсор при пропадании фокуса.
    ImageLeftIndent -      Смещение картинки ячейки от левого края.
                           Используется для настройки отрисовки картинки
                           ячейки. Не следует без особой необходимости
                           менять это значение (а также ImageTopIndent).
    Images -               Список картинок.
    ImageIndexDef -        Номер картинки по умолчанию. Эта картинка будет
                           назначена первой видимой колонке.
    ImageHighlight -       Подсвечивать или нет картику для выделенной
                           ячейки.
    ImageTopIndent -       Смещение картинки ячейки от верхнего края.
    LeftCol -              Левая видимая (м.б. частично видимая) колонка таблицы.
                           Равно VisOrigin.Col.
    RightClickSelect -     Допускается или нет выделение ячейки правой
                           клавишей мышки.
    Row -                  Текущая строка ячейки фокуса. Равно CellFocused.Row.
    Rows -                 Параметры строк ячейки.
    RowSelect -            Выделение по целой строке или по ячейке.
    ShowCellTips -         Показывать или нет Hint с текстом ячейки, если
                           текст не помещается целиком в ячейку (для Delphi
                           версии 3 и выше).
                           ВНИМАНИЕ: Hint ячейки основан на стандартном
                           Hint-е, для отображения подсказок необходимо
                           установить ShowHint в True. Если ShowCellTips
                           установлено, то обычный Hint - отображаться не
                           будет (т.е. либо Hint, либо CellTips).
    ShowFocusRect -        Показывать рамку фокуса.
    ShowHeader -           Показываеть заголовок.

    SortLeftIndent -       Смещение картинки направления сортировки от
                           правого края текста. Используется для настройки
                           вывода картинки направления сортировки. Не следует
                           без особой необходимости менять это значение (а
                           также значение SortTopIndent).
    SortTopIndent -        Смещение картинки направления сортировки
                           относительно верхнего каря текста по вертикали.
    TextLeftIndent -       Смещение текста ячейки от левого края, в случае,
                           когда у ячейки нет картинки. Используется для
                           настройки вывода текста ячейки. Не следует без
                           особой необходимости менять это значение (а также
                           TextRightIndent и TextTopIndent).
    TextRightIndent -      Отсут текста ячейки от правого края.
    TextTopIndent -        Смещение текста ячейки сверху.
    TipsCell -             Ячейка, для которой показываетсся подсказка.
    TipsText -             Строк подсказки для ячейки TipsCell.
    TopRow -               Верхняя видимая строка таблицы. Равно VesOrigin.Row.
    VertScrollBar -        Вертикальный скроллер.
    VisibleColCount -      Количество видимых колонок. Равно VisSize.Col.
    VisibleRowCount -      Количество видимых строк. Равно VisSize.Row.
    VisOrigin -            Первая видимая ячейка.
    VisSize -              Количество видимых ячеек.

    События:

    OnCellAcceptCursor -   Можно ли устанавливать курсор на указанную ячейку.
    OnCellClick -          Щелчок мышкой на ячейке.
    OnCellTips -           Надо ли показывать подсказку по указанной ячейке.
    OnChange -             Cменилась выделенная ячейка. Вызывается сразу
                           после изменении ячеейки курсора.
    OnChangeColumns -      Произошло изменение колонок таблицы.
    OnChangeEditing -      Произошло изменение видимости строки ввода.
    OnChangeEditMode -     Произошло изменение режима редактирования.
    OnChangeFixed -        Произошло изменение фиксированных колонок таблицы.
    OnChangeRows -         Произошло изменение строк таблицы.
    OnChanging -           Меняется выделенная ячейка. Вызывается
                           непосредственно перед изменением ячейки курсора.
    OnCheckClick -         Щелчок мышкой на флажке ячейки.
    OnColumnAutoSize -     Ширина колонки меняется на ширину, подобранную
                           автоматически.
    OnColumnResize -       Ширина колонки изменилась.
    OnColumnResizing -     Изменяется ширина колонки.
    OnDraw -               Отрисовка таблицы.
    OnDrawCell -           Отрисовка ячейки. Если необходимо изменить
                           цвета ячейки, но оставить стандартную отрисовку,
                           следует переопределение цветов выполнить в
                           событии OnGetCellColors.
    OnDrawHeader -         Отрисовка секции ячейки. Если необходимо изменить
                           цвета секции, но оставить стандартную отрисовку,
                           следует переопределение цветов выполнить в
                           событии OnGetHeaderColors.
    OnEditAcceptKey -      Проверка приемлимости символа ячейки.
    OnEditButtonPress -    Нажата кнопка с многоточием у строки ввола.
    OnEditCanceled -       Ввода текста отменен, строка редактирования
                           погашена в результате нажатия клавиши Escape.
    OnEditCanModify -      Может ли изменяться текст в строке ввода.
    OnEditChange -         Изменился текст в строке ввода.
    OnEditCloseUp -        Произошло закрытие выпадающего списка с выбором
                           значения.
    OnEditSelectNext -     Нажат Ctrl+Enter на кнопке с нераскрытым списком
                           (по идее следует в ячейку вставить следующее
                           значение из списка).
    OnGetCellColors -      Определеить цвета ячейки. Событие влияет на
                           отрисовку ячейки, вид CellTips и пр.
    OnGetCellImage -       Вернуть номер картинки ячейки.
    OnGetCellImageIndent - Получить смещение картинки ячейки.
    OnGetCellReadOnly -    Получить флаг "Только для чтения" для указанной
                           ячейки.
    OnGetCellText -        Получить текст ячейки.
    OnGetCellTextIndent -  Получить смещение текста ячейки.
    OnGetCheckAlignment -  Получить выравнивание флажка ячейки.
    OnGetCheckImage -      Получить картинку флажка ячейки.
    OnGetCheckIndent -     Получить смещение картинки флажка. Зависит от
                           установленного выравнивания флажка.
    OnGetCheckKind -       Получить тип флажка ячейки.
    OnGetCheckState -      Получить состояние флажка ячейки.
    OnGetEditList -        Заполнить выпадающий список строки редактирования.
    OnGetEditListBounds -  Подправить положение и размеры выпадающего списка
                           строки редактирования.
    OnGetEditMask -        Вернуть маску ячейки для редактирования. 
    OnGetEditStyle -       Определить стиль строки редактирования.
    OnGetEditText -        Вернуть текст ячейки для редактирования. По
                           умолчанию берется текст ячейки.
    OnGetHeaderColors -    Определеить цвета секции заголовка.
    OnGetHeaderImage -     Определеть номер картинки секции заголовка.
    OnGetSortDirection -   Определить направление сортировки для указанной
                           колонки. Вызывается непосредственно при отрисовке
                           секции заголовка для определения значка сортировки. 
    OnGetSortImage -       Вернуть картинку сортировки для указанной колонки.
    OnHeaderClick -        Нажата секция заголовка.
    OnHeaderClicking -     Нажимается секция заголовка. Вызывается
                           непосредственно перед началом нажатия при
                           Header.Flat = False.
    OnResize -             Изменение размера таблицы.
    OnSetEditText -        Установить текст ячейки. Вызывается при
                           перемещении курсора. Если текст не принимается,
                           следует вызвать исключение.
  }

  {
    Protected Methods:

    CancelCellTips -       Cancels the display of a hint for a cell.
    CellClick -            Respond to user cell click.
    Change -               Generates an OnChange event. Called after the
                           selected cell or selected state change
    Changing -             Generates an OnChanging event. Called before the
                           selected cell or selected state change.
    CheckClick -           Generates an OnCheckClick event. Called when user
                           click the check box with mouse or the SPACE key.
    ColumnSizeToFit -      Generates an OnColumnSizeToFit event. Called when
                           the user double-clicks the right side of the header
                           before automatically change the width of the column
                           to fit its contents.
    ColumnResize -         Generates an OnColumnResize event. Called after
                           resizing a column.
    ColumnResizing -       Generates an OnColumnResizing event. Called when the
                           column is resized with the mouse.
    CreateColumn -         Creates the TGridColumn object. Called internally
                           to add new column to the Columns list. To define you
                           own column class overwrite the GetColumnClass method.
    CreateColumns -        Creates the TGridColumns object that stores the
                           column descriptors for the grid. Called internally
                           to create Columns instance. Overwrite this method if
                           you need to substitute a descendant of TGridColumns
                           to manage your own column descriptors.
    CreateEdit -           Creates the inplace editor that allows the cells of
                           the grid to be edited. Called internally the first
                           time the grid goes into edit mode. To define you own
                           class of inplace editor for the specified cell
                           overwrite GetEditClass method.
    CreateFixed -          Creates the TGridFixed object. Called internally
                           to create Fixed instance. Overwrite this method if
                           you need to substitute a descendant of TGridFixed.
    CreateHeader -         Creates the TGridHeader object. Called internally
                           to create Header instance. Overwrite this method if
                           you need to substitute a descendant of TGridHeader.
    CreateHeaderSection -  Creates the TGridHeaderSection object. Called
                           internally to add new section to the Header.
                           Overwrite this method if you need to substitute a
                           descendant of TGridHeaderSection.
    CreateRows -           Creates the TGridRows object. Called internally
                           to create Rows instance. Overwrite this method if
                           you need to substitute a descendant of TGridRows.
    CreateScrollBar -      Creates the TGridScrollBar object. Called internally
                           to create HorzScrollBar and VertScrollBar instances.
                           Overwrite this method if you need to substitute a
                           descendant of TGridScrollBar.
    EditButtonPress -      Generates an OnEditButtonPress event. Called when
                           the user hit Ctrl+Enter in the inplace editor or
                           presses ellipsis button with the mouse.
    EditCanAcceptKey -     Generates an OnEditCanAcceptKey event. Provides a
                           simple screening of keyboard input while the user
                           is editing the contents of a cell.
    EditCanceled -         Generates an OnEditCanceled event. Called when the
                           inplace editor is hidden by pressing ESCAPE key.
    EditCanModify -        Generates an OnEditModify event. Called before any
                           text changes in the inplace editor.
    EditCanShow -          Generates an OnEditCanShow event. Determines whether
                           the current cell in the grid can be edited.
    EditCanUndo -          Determines whether the current cell in the grid
                           can handle Undo operation.
    EditChange -           Generates an OnEditChange event. Called when the text
                           in the inplace editor has changed.
    EditCloseUp -          Generates an OnEditCloseUp and OnEditCloseUpEx events.
                           Called when the drop-down list closes up due to
                           select list item.
    EditSelectNext -       Generates an OnEditSelectNext event. Called when user
                           hit Ctrl+Enter or double click on inplace editor with
                           closed popup list.
    GetCellImage -         Generages an OnGetCellImage event to determine
                           if there is an image for the specified cell.
                           Returns image index or -1 is cell has no image.
    GetCellImageRect -     Returns the coordinates of a image in the grid.
    GetCellHintRect -      Cell hint is shown only if the cell text gets out of
                           hint rectangle. By default returns the position of
                           the cell inplace editor.
    GetCellText -          Generated OnGetCellText event.
    GetCellColors -        Sets the default color, brush and font for the
                           specified cell on the specified canvas then
                           generates OnGetCellColors event.
    GetCellTextBounds -    Returns the text coordinates of the specified cell
                           in the grid.
    GetCellTextIndent -    Generates OnGetCellTextIndent event. By default,
                           the horizontal indent of the text is 2 pixels for
                           cells with check box or an image and TextLeftIndent
                           for other cells.
    GetCheckAlignment -    Generates OnGetCheckAlignment event.
    GetCheckImage -        Generates OnGetCheckImage event. Returns the image
                           index for specified cell or -1 if cell have no image.
    GetCheckKind -         Generages an OnGetCheckKind event. Returns the type
                           of the check box for specified cell:
                             gcNone -        No check box.
                             gcCheckBox -    Like TCheckBox.
                             gcRadioButton - Like TRadioButton.
                             gcUserDefine -  User defined check box image.
    GetCheckRect -         Returns the coordinates of a check box in the grid.
                           Returns (0,0,0,0) if cell dows not have a check box.
    GetCheckState -        Generates OnGetCheckState and OnGetCheckStateEx events.
                           Returns the state of a check box.
    GetCheckStateEx -      Returns the state of a check box and its enabled state.
    GetColumnClass -       Returns the class type of the column. Descendant
                           classes can override this method to define their own
                           column class (for example, a TDBGridColumn).
    GetCursorCell -        Finds a cell in the specified direction where the
                           cursor can be positioned. Direction can be:
                             goLeft -       One column to the left.
                             goRight -      One column to the right.
                             goUp -         One row to the up.
                             goDown -       One row to the down.
                             goPageUp -     One page to the up.
                             goPageDown -   One page to the down.
                             goHome -       First cell of specified row.
                             goEnd -        Last cell of specified row.
                             goGridHome -   Upper left grid cell.
                             goGridEnd -    Bottom right grid cell.
                             goGridTop -    First row of the current column.
                             goGridBottom - Last row of the current column.
                             goSelect -     Nearest available cell.
                             goFirst -      First available cell in the grid.
                             goNext -       Next cell to the end of the grid.
                             goPrev -       Previous cell to the beginning of
                                            the grid.
    GetEditClass -         Returns the class type of the inplace editor for
                           the specified cell. Descendant classes can override
                           this method to define their own editor class (for
                           example, a TDBGridEdit).
    GetEditList -          Assigns the pick list items of the current column
                           (if present) to the drop-down list and generates
                           OnGetEditList event.
    GetEditListBounds -    Generates OnGetEditListBounds event.
    GetEditListIndex -     Generates OnGetEditListIndex event.
    GetEditMask -          Returns an input mask for the specified cell.
    GetEditStyle -         Indicates whether a specified cell has an ellipsis
                           button or drop-down arrow.
    GetEditText -          Returns the value of the specified cell formatted
                           for editing. By default returns cell text.
    GetFixedDividerColor - Returns the color of the dividing line of fixed and
                           scrollable columns depending on the background color
                           of the grid.
    GetFixedGridColor -    Returns the color of grid lines of fixed columns
                           depending on the background color of fixed columns.
    GetHeaderColors -      Sets the default header brush and font on the specified
                           canvas then generates OnGetHeaderColors event.
    GetHeaderImage -       Generages an OnGetHeaderImage event.
    GetSortArrowSize -     Calculates the size of the sort image depending on
                           the Windows theme.
    GetSortDirection -     Generates OnGetSortDirection event.
    GetSortImage -         Generates OnGetSortImage event.
    GetTextRect -          Calculates the coordinates of the text in the cell.
    GetTipsRect -          Calculates the coordinates of the cell hint in the
                           grid and generates OnGetTipsRect event.
    GetTipsText -          Generates OnGetTipsText event.
    GetTipsWindowClass -   Returns the class type of the hint window class.
    HeaderClick -          Generated OnHeaderClick event. Called when the user
                           clicks the header.
    HeaderClicking -       Generated OnHeaderClicking event. Called when the
                           user start clicking the header.
    HideCursor -           Hides the focus rectangle or inplace editor.
    HideEdit -             Hides the inplace editor.
    HideFocus -            Hides the focus rectangle.
    PaintCell -            Generates an OnDrawCell event and draws the cell
                           using DefaultDrawCell method.
    PaintCells -           Draws scrollable cells.
    PaintCheck -           Draws a check box.
    PaintFixed -           Draws fixed cells.
    PaintFixedGridLines -  Draws grid lines between fixed cells.
    PaintFocus -           Draws focus rectangle.
    PaintFreeField -       Draws the area outside the cells.
    PaintGridLines -       Draws grid lines between scrollable cells.
    PaintHeader -          Draws a header section.
    PaintHeaderBackground - Paints the header section background.
    PaintHeaders -         Draws the grid header.
    PaintResizeLine -      Draws a vertical resize line.
    PaintText -            Universal text drawing procedure. Used internally
                           to draw the cells, header and cell tips.
    SetEditText -          Generates an OnSetEditText event.
    ShowCursor -           Displays the focus rectangle or inplace editor.
    ShowEdit -             Displays the inplace editor when the grid enters
                           edit mode.
    ShowEditChar -         Displays the inplace editor, with its text set to
                           the specified character.
    ShowFocus -            Shows a focus rectangle.
    UpdateListBounds -     Calculates the bounds rectangle of drop-down list.
                           Called internally before inplace editor's drop-down
                           list is displayed.
    UpdateListItems -      Fill the drop-down item list and sets the index of
                           the selected item.

    Public Methods:

    AcquireFocus -         Changes focus to the grid, if possible. Returns False
                           if grid can not be focused.
    ApplyEdit -            Applies text changes from the inplace editor.
    ApplyEditText -        Calls SetEditText method and handles event exceptions.
                           Called internally before changing the current cell or
                           hiding inlace editor.
    CancelEdit -           Cancels editind, discards changes in the inplace
                           editor and hides it. Called when AlwaysEdit is
                           set to False and user has pressed the ESCAPE key.
    DefaultDrawCell -      Draws the contents of a cell in the grid.
    DefaultDrawHeader -    Draws the contents of a header section in the grid.
    FindText -             Searches text from selected cell up or down and sets
                           the cursor on the cell with the found text. Returns
                           True if the cell is found.
    GetCellAt -            Returns the cell located at a specified position
                           within the grid. If there is no cell located at
                           the specified position returns (-1, -1).
    GetCellRect -          Returns the coordinates of a cell in the grid.
    GetCellsRect -         Returns the coordinates of a cell range in
                           the grid.
    GetColumnAt -          Returns the column located at a specified position
                           within the grid. If there is no column located at
                           the specified position returns -1.
    GetColumnLeftRight -   Returns left and right coordinates of the column in
                           the grid.
    GetColumnMaxWidth -    Returns the maximum width of the text of the specified
                           column in visible rows. Called to automatically change
                           the width of column to fit its contents.
    GetColumnRect -        Returns the coordinates of the column in the grid.
    GetColumnsRect -       Returns the coordinates of the column range in
                           the grid.
    GetColumnsWidth -      Returns the width in pixels of the column range.
    GetEditRect -          Returns the coordinates of the inplace editor in the
                           grid. By default, it is equal to the rectangle of the
                           cell minus the check box and the image on the left.
    GetFirstImageColumn -  Returns the index of the first visible scrollable
                           column.
    GetFixedRect -         Returns the coordinates of the fixed columns in the
                           grid.
    GetFixedWidth -        Returns the width of the fixed columns in pixels.
    GetFocusRect -         Returns the coordinates of the focus rectangle in
                           the grid.
    GetGridHeight -        Returns the height of the visible cells in pixels.
    GetGridLineColor -     Generates OnGetGridColor event.
    GetGridOrigin -        Returns the offset in pixels of cells relative to the
                           upper left corner of the grid. Determined by the
                           position of the scrollers.
    GetGridRect -          Returns the rectangle of the visible cells in the
                           grid.
    GetHeaderHeight -      Same as Header.Height.
    GetHeaderRect -        Returns the coordinates of the header in the grid.
    GetHeaderSection -     Returns the header section of the specified level for
                           the specified column. If the level is -1, it returns
                           the lowest section.
    GetResizeSectionAt -   Returns the header section with the right margin
                           located at a specified position within the grid.
                           Called internally to find a section for resizing.
    GetRowAt -             Returns the row located at a specified position
                           within the grid. If there is no row located at
                           the specified position returns -1.
    GetRowRect -           Returns the coordinates of the row in the grid.
    GetRowsRect -          Returns the coordinates of the row range in the grid.
    GetRowsHeight -        Returns the height in pixels of the row range.
    GetRowTopBottom -      Returns top and bottom coordinates of the row in the
                           grid.
    GetSectionAt -         Returns the header section located at a specified
                           position within the grid. If there is no section
                           located at the specified position returns null.
    InvalidateCell -       Invalidates the region occupied by a cell.
    InvalidateCheck -      Invalidates the region occupied by a check box of
                           specified cell.
    InvalidateColumn -     Invalidates the region occupied by a column.
    InvalidateColumns -    Invalidates the region occupied by the column range.
    InvalidateEdit -       Invalidates the region occupied by the inplace editor
                           if visible.
    InvalidateFixed -      Invalidates the region occupied by a fixed columns.
    InvalidateFocus -      Invalidates the region occupied by a current cell.
    InvalidateGrid -       Invalidates the region occupied by the scrollable columns.
    InvalidateHeader -     Invalidates the region occupied by the header.
    InvalidateRect -       Invalidates the region occupied by specified rectangle.
    InvalidateRow -        Invalidates the region occupied by a row.
    InvalidateRows -       Invalidates the region occupied by a row range.
    InvalidateSection -    Invalidates the region occupied by the header
                           of the specified column.
    IsActiveControl -      Determines whether the grid or its inplace editor
                           has input focus.
    IsCellAcceptCursor -   Generates an OnCellAcceptCursor event.
    IsCellEditing -        Indicates whether the specified cell is currently
                           being edited.
    IsCellHighlighted -    Indicates whether the specified cell is selected
                           and focused and must be highlighted by color.
    IsCellHasCheck -       Indicates whether the specified cell has a check box.
    IsCellHasImage -       Indicates whether the specified cell has an image.
    IsCellFocused -        Indicates whether the specified cell is equal to
                           the focused cell.
    IsCellReadOnly -       Indicates whether the specified cell can be edited.
                           Generates an OnGetCellReadOnly event.
    IsCellValid -          Indicates that the column and row of the specified
                           cell are within the grid.
    IsCellValidEx -        Indicates that the column and row of the specified
                           cell are within the grid and column is visible.
    IsCellVisible -        Indicates whether the specified cell is currently
                           visible in the grid image.
    IsColumnVisible -      Indicates whether the specified column is currently
                           visible in the grid image.
    IsEvenRow -            Indicates whether the row of the specified cell
                           is even. Called internally to calculate cell colors.
    IsFixedVisible -       Indicates whether the fixed columns are visible.
                           Called internally while drawing grid.
    IsFocusAllowed -       Indicates whether the focus rectangle can be displayed.
    IsGridHintVisible -    Indicates whether to draw a hint for an empty grid.
    IsHeaderHasImage -     Indicates whether the specified header has an image.
    IsHeaderPressed -      Returns the state of specified header section.
                           Called internally when drawing a header.
    IsRowHighlighted -     Indicates whether the specified row is selected
                           and must be highlighted by color.
    IsRowVisible -         Indicates whether the specified row is currently
                           visible in the grid image.
    LockUpdate -           Disables grid redraw.
    MakeCellVisible -      Makes the specified cell visible to the user,
                           scrolling the grid as necessary.
    ResetEdit -            Resets the inplace editor. Called when AlwaysEdit is
                           set to True and user has pressed the ESCAPE key.
                           By default sends WM_UNDO to the inplace editor to
                           undo the last operation.  Override this method if you
                           need to perform additional actions after cancel editing.
                           For example, cancel changes to the data source.
    SetGridCursor -        Changes the current cell to a specified cell.
    SizeAllColumnsToFit -  Changes the width of all columns to fit their contents.
    SizeColumnToFit -      Changes the width of column to fit its contents.
    UnLockUpdate -         Enables grid redraw.
    UpdateCursor -         Verifies that the current cell can accept the cursor,
                           then updates the position of the cursor if necessary.
    UpdateEdit -           Creates an instance of the editor for the selected
                           cell, updates its style, font, text, then displays it.
    UpdateEditContents -   Refreshes the inplace editor. Use this method to
                           update the text of the editor if cells data have
                           changed during editing.
    UpdateFocus -          Sets focus to the grid, if possible, then show the
                           inplace edit, if necessary.
    UpdateSelection -      Determines whether the specified cell can be selected,
                           returns the closest available cell to select. Called
                           internaly before change the cursor position.
    UpdateText -           Same as ApplyEditText.

    Public Properties:

    AllowEdit -            Determines whether the user can edit data using
                           inplace editor. AlowEdit is ignored if RowSelect
                           is set to True.
    AllowSelect -          Determines whether the selected cell displays the
                           focus rectangle even when the grid does not have focus.
    AlwaysEdit -           Determines whether the grid is always in edit mode
                           and inplace editor is always visible. AlwaysEdit
                           is ignored if AlowEdit is set to False.
    AlwaysSelected -       Some strange property.
    CellFocused -          Indicates the focused cell in the grid.
    Cells -                Lists the strings for each cell in the grid.
    CellSelected -         Indicates the focused cell is in selected state.
    CheckBoxes -           Specifies whether check boxes appear next to the
                           cells in the grid.
    Checked -              Specifies whether the check box of the cell is checked.
    CheckEnabled -         Specifies whether the check box of the cell is enabled.
    CheckStyle -           Specifies the style of the check box border:
                             csFlat -  Flat border.
                             cs3D -    3D border.
                             csWin95 - Windows 95 style border.
                           If Windows themes are enabled, the check box is
                           displayed using the current theme.
    Col -                  Specifies the index of the column that contains
                           the selected cell. Same as CellFocused.Col.
    ColumnClick -          Determines whether the column header behaves like
                           a button.
    Columns -              Describes the properties of the columns in the grid.
    ColumnsFullDrag -      Specifies whether columns are repainted when they
                           are dragged.
    ContextPopupCol -      Specifies the index of the column above which the
                           context menu is displayed.
    ContextPopupRow -      Specifies the index of the row above which the
                           context menu is displayed.
    CursorKeys -           Defines the keys that can be used to move the cursor
                           in a grid:
                             gkArrows -     Move the cursor with arrows.
                             gkTabs -       Move the cursor with TAB key.
                             gkReturn -     Move to the next cell after pressing
                                            the ENTER key.
                             gkMouse -      Select the cells with the mouse.
                             gkMouseMove -  Select the cells while mouse moving
                                            with the left mouse button is pressed.
                             gkMouseWheel - Select cells with the mouse wheel.
    DefaultEditMenu -      Determines whether the Windows popup menu should be
                           displayed for the inplace editor instead of grid
                           popup menu.
    DefaultHeaderMenu -    Show Explorer-like popup menu for the header to
                           quick show and hide columns. If there is a user
                           popup menu assigned to the header, the header menu
                           items will be added to it.
    Edit -                 Designates the edit control the grid uses to allow
                           users to edit the contents of the selected cell.
    EditCell -             Indicates the edit cell in the grid.
    EditColumn -           Indicates the column of the edit cell in the grid.
    EditDropDown -         Indicates whether the inplace editor drop-down list
                           is being visible.
    Editing -              Indicates whether any cell in the grid is being
                           edited.
    EndEllipsis -          Indicates whether cell text should be replaced at
                           the end with an ellipsis (...) to fit cell borders.
                           Works only for single-line text with left alignment.
    Fixed -                Represents the fixed columns for the grid.
    FlatBorder -           Determines whether a single line border is drawn
                           around the grid.
    FocusOnScroll -        Determines whether the grid should capture the input
                           focus when scrolling.
    GridColor -            Specifies the color of the lines that separate the
                           cells of the grid.
    GridHint -             Specifies the hint for an empty grid (grid without
                           rows, like "This folder is empty" for empty folder
                           in Explorer).
    GridHintColor -        Specifies the color of the hint for an empty grid.
    GridLines -            Determines whether lines are drawn separating cells
                           in the grid.
    GridStyle -            Determines the style of the grid lines:
                             gsHorzLine -     Show horizontal lines.
                             gsVertLine -     Show vertical lines.
                             gsFullHorzLine - Draw horizontal lines to the right
                                              side of the grid, but not to the
                                              last column.
                             gsFullVertLine - Draw vertical lines to the bottom
                                              of the grid, but not to the last
                                              row (only if Rows.Count > 0).
                             gsListViewLike - TListView like grid lines.
                             gsDotLines -     Draw dotted lines.
    Header -               Represents the header for the grid.
    HideSelection -        Determines whether a selected cell appears selected
                           when the focus shifts to another control.
    HighlightEvenRows -    Enable highlight every alternate row.
    HighlightFocusCol -    Indicates whether a focused column should be
                           highlighted when it is drawn.
    HighlightFocusRow -    Indicates whether a focused row should be highlighted
                           when it is drawn.
    HorzScrollBar -        Represents the horizontal scroll bar for the grid.
    Images -               Determines which image list is associated with the grid.
    ImageIndexDef -        Determines the default image that appears on the first
                           column of the grid.
    ImageHighlight -       Indicates whether the selected cell image should be
                           highlighted too.
    LeftCol -              Specifies the index of the first visible (may be
                           partial visible) scrollable column in the grid.
                           Same as VisOrigin.Col.
    RightClickSelect -     Determines whether the CellFocused property returns
                           cells that are selected using the right mouse button.
    Row -                  Specifies the index of the row that contains the
                           selected cell. Same as CellFocused.Row.
    Rows -                 Represents the rows parameters for the grid.
    RowSelect -            Specifies whether the entire row of the selected cell
                           is highlighted.
    ShowCellTips -         Determines whether the grid displays a tooltip for a
                           cell with long text when the mouse pointer rests
                           momentarily on the cell. When ShowCellTips is True
                           the standard control hint is now working.
    ShowFocusRect -        Indicates whether to draw a focus rectangle around
                           the current cell when it has input focus.
    ShowGridHint -         Indicates whether to display hint for an empty grid.
    ShowHeader -           Indicates whether to display header of the grid.
    TextLeftIndent -       Specifies the indent of cells text from the left edge.
    TextRightIndent -      Specifies the indent of cells text from the right edge.
    TextTopIndent -        Specifies the indent of cells text from the top edge.
    TipsCell -             Indicates the cell for which tooltip is displayed.
    TipsText -             Indicates the cell's tooltip text that is displayed.
    TopRow -               Specifies the index of the first visible scrollable
                           row in the grid. Same as VisOrigin.Row.
    VertScrollBar -        Represents the vertical scroll bar for the grid.
    VisibleColCount -      Indicates the number of scrollable columns visible
                           in the grid. Same as VisSize.Col.
    VisibleRowCount -      Indicates the number of scrollable rows visible
                           in the grid. Same as VisSize.Row.
    VisOrigin -            Specifies the index of the first visible cell in
                           the grid.
    VisSize -              Indicates the number of scrollable cells visible
                           in the grid.

    Public Events:

    OnCellAcceptCursor -   Occurs when the grid checks for permission to move
                           the cursor to the selected cell.
    OnCellClick -          Occurs when the user presses the left mouse button
                           in one of the cells of the grid.
    OnCellTips -           Occurs before displaying the cell hint.
    OnChange -             Occurs after a cell in the grid is selected.
    OnChangeColumns -      Occurs after change columns.
    OnChangeEditing -      Occurs when the inplace editor is shown or hidden.
    OnChangeEditMode -     Occurs when the value of AllowEdit property changes.
    OnChangeFixed -        Occurs after changing fixed cells.
    OnChangeRows -         Occurs after changing rows.
    OnChanging -           Occurs before a cell in the grid is selected.
    OnCheckClick -         Occurs when the user clicks cell check box.
    OnColumnSizeToFit -    Occurs before the with of a column changes to fit
                           its contents.
    OnColumnResize -       Occurs after the with of a column changes.
    OnColumnResizing -     Occurs before changing the column with.
    OnDraw -               Occurs when a grid needs to be drawn.
    OnDrawCell -           Occurs when a cell needs to be drawn. If you just
                           need to change the colors of the cell but not the
                           drawing, use OnGetCellColors event.
    OnDrawHeader -         Occurs when a section of the header needs to be
                           drawn. If you just need to change the colors of
                           the section but not the drawing, use OnGetCellColors
                           event.
    OnEditAcceptKey -      Called to check whether the Key can be entered into
                           the current cell.
    OnEditButtonPress -    Occurs when the user presses the ellipsis button
                           in a inplace editor.
    OnEditCanceled -       Occurs when the inplace editor is hidden by
                           pressing ESCAPE key.
    OnEditCanModify -      Called to check whether the user can change
                           the text in the inplace editor.
    OnEditChange -         Occurs when the text in the inplace editor has
                           changed.
    OnEditCloseUp -        Occurs when the drop-down list closes up due to
                           select list item.
    OnEditCloseUpEx -      Extended version of OnEditCloseUp event.
    OnEditSelectNext -     Occurs when user hit Ctrl+Enter or double click
                           on inplace editor with closed popup list.
    OnGetCellColors -      Occurs when the grid requests the colors and font
                           of a cell.
    OnGetCellImage -       Occurs when the grid requests the image index
                           of a cell.
    OnGetCellImageEx -     Extended version OnGetCellImage event for images
                           with overlay.
    OnGetCellImageIndent - Occurs during drawing the cell when the grid
                           requests the indent of cell image.
    OnGetCellReadOnly -    Called to check whether the user can change
                           the value of the cell.
    OnGetCellText -        Occurs when the grid requests a value for a cell.
    OnGetCellTextIndent -  Occurs during drawing the cell when the grid
                           requests indent of the cell text.
    OnGetCheckAlignment -  Occurs during drawing the cell when the grid
                           requires alignment of the cell check box.
    OnGetCheckImage -      Occurs during drawing the cell when the grid
                           requires the user defined check box image. Check box
                           image must be a transparent bitmap with 16x16 pixels
                           size.
    OnGetCheckIndent -     Occurs during drawing the cell when the grid
                           requires indent of the cell check box.
    OnGetCheckKind -       Occurs when the grid requests the kind of
                           a check box of a cell.
    OnGetCheckState -      Occurs during drawing the cell when the grid
                           requires the state of a check box of a cell.
    OnGetCheckStateEx -    Extended version OnGetCheckState event for cells
                           with disabled check boxes.
    OnGetEditList -        Occurs when the grid needs to fill drop-down list
                           before open it.
    OnGetEditListBounds -  Occurs when the grid needs to adjusts the size and
                           position of the drop-down list before open it.
    OnGetEditListIndex -   Occurs when the grid needs to select an item in the
                           f drop-down list before open it.
    OnGetEditMask -        Occurs when the inplace editor requests an edit mask.
    OnGetEditStyle -       Occurs when the grid requests the style of inplace
                           editor of the cell.
    OnGetEditText -        Occurs when the inplace editor requests the value
                           of a cell. By default, returns cell text.
    OnGetGridHint -        Occurs when an empty grid's ToolTip text is needed.
                           For example: "This folder is empty".
    OnGetGridColor -       Occurs during drawing when grid requires the
                           colors of the grid lines.
    OnGetHeaderColors -    Occurs when the grid requests the colors and font
                           of a header section.
    OnGetHeaderImage -     Occurs when the drawing the header whe grid requires
                           an image of the header section.
    OnGetSortDirection -   Occurs during drawing the header when the grid needs
                           to determine whether to draw the column sort image.
    OnGetSortImage -       Occurs during drawing the header when grid requires
                           an image of the sort direction.
    OnHeaderClick -        Occurs after the user clicks a column header.
    OnHeaderClicking -     Occurs before the user clicks the column header
                           when Header.Flat is False.
    OnHeaderDetailsClick - Occurs when the "Details..." item is selected in
                           the popup menu of the header.
    OnResize -             Occurs after the control is resized.
    OnSetEditText -        Occurs when the user edits the value of a cell by
                           pressing the ENTER key or by moving to another cell.
                           If the new value is not valid, an exception should
                           be raised to abort.
  }

  TGridStyle = (gsHorzLine, gsVertLine, gsFullHorzLine, gsFullVertLine,
    gsListViewLike, gsDotLines);
  TGridStyles = set of TGridStyle;

  TGridCursorKey = (gkArrows, gkTabs, gkReturn, gkMouse, gkMouseMove, gkMouseWheel);
  TGridCursorKeys = set of TGridCursorKey;

  TGridCursorOffset = (goLeft, goRight, goUp, goDown, goPageUp, goPageDown,
    goHome, goEnd, goGridHome, goGridEnd, goSelect, goFirst, goNext, goPrev,
    goGridTop, goGridBottom);

  TGridSortDirection = (gsNone, gsAscending, gsDescending);

  TGridPaintState = (psPressed, psHot, psSorted, psFlat, psDontCare);
  TGridPaintStates = set of TGridPaintState;

  TGridTextEvent = procedure(Sender: TObject; Cell: TGridCell; var Value: string) of object;
  TGridRectEvent = procedure(Sender: TObject; Cell: TGridCell; var Rect: TRect) of object;
  TGridCellColorsEvent = procedure(Sender: TObject; Cell: TGridCell; Canvas: TCanvas) of object;
  TGridCellImageEvent = procedure(Sender: TObject; Cell: TGridCell; var ImageIndex: Integer) of object;
  TGridCellImageExEvent = procedure(Sender: TObject; Cell: TGridCell; var ImageIndex, OverlayIndex: Integer) of object;
  TGridCellClickEvent = procedure(Sender: TObject; Cell: TGridCell; Shift: TShiftState; X, Y: Integer) of object;
  TGridCellAcceptCursorEvent = procedure(Sender: TObject; Cell: TGridCell; var Accept: Boolean) of object;
  TGridCellNotifyEvent = procedure(Sender: TObject; Cell: TGridCell) of object;
  TGridCellIndentEvent = procedure(Sender: TObject; Cell: TGridCell; var Indent: TPoint) of object;
  TGridCellTipsEvent = procedure(Sender: TObject; Cell: TGridCell; var AllowTips: Boolean) of object;
  TGridCellReadOnlyEvent = procedure(Sender: TObject; Cell: TGridCell; var CellReadOnly: Boolean) of object;
  TGridHeaderColorsEvent = procedure(Sender: TObject; Section: TGridHeaderSection; Canvas: TCanvas) of object;
  TGridHeaderImageEvent = procedure(Sender: TObject; Section: TGridHeaderSection; var ImageIndex: Integer) of object;
  TGridDrawEvent = procedure(Sender: TObject; var DefaultDrawing: Boolean) of object;
  TGridDrawCellEvent = procedure(Sender: TObject; Cell: TGridCell; var Rect: TRect; var DefaultDrawing: Boolean) of object;
  TGridDrawHeaderEvent = procedure(Sender: TObject; Section: TGridHeaderSection; Rect: TRect; var DefaultDrawing: Boolean) of object;
  TGridColumnResizeEvent = procedure(Sender: TObject; Column: Integer; var Width: Integer) of object;
  TGridHeaderClickEvent = procedure(Sender: TObject; Section: TGridHeaderSection) of object;
  TGridHeaderClickingEvent = procedure(Sender: TObject; Section: TGridHeaderSection; var AllowClick: Boolean) of object;
  TGridChangingEvent = procedure(Sender: TObject; var Cell: TGridCell; var Selected: Boolean) of object;
  TGridChangedEvent = procedure(Sender: TObject; Cell: TGridCell; Selected: Boolean) of object;
  TGridEditStyleEvent = procedure(Sender: TObject; Cell: TGridCell; var Style: TGridEditStyle) of object;
  TGridEditListEvent = procedure(Sender: TObject; Cell: TGridCell; Items: TStrings) of object;
  TGridEditListIndexEvent = procedure(Sender: TObject; Cell: TGridCell; Items: TStrings; const ItemText: string; var ItemIndex: Integer) of object;
  TGridEditCloseUpEvent = procedure(Sender: TObject; Cell: TGridCell; ItemIndex: Integer; var Accept: Boolean) of object;
  TGridEditCloseUpExEvent = procedure(Sender: TObject; Cell: TGridCell; Items: TStrings; ItemIndex: Integer; var ItemText: string; var Accept: Boolean) of object;
  TGridEditCanModifyEvent = procedure(Sender: TObject; Cell: TGridCell; var CanModify: Boolean) of object;
  TGridEditCanShowEvent = procedure(Sender: TObject; Cell: TGridCell; var CanShow: Boolean) of object;
  TGridAcceptKeyEvent = procedure(Sender: TObject; Cell: TGridCell; Key: Char; var Accept: Boolean) of object;
  TGridCheckKindEvent = procedure(Sender: TObject; Cell: TGridCell; var CheckKind: TGridCheckKind) of object;
  TGridCheckStateEvent = procedure(Sender: TObject; Cell: TGridCell; var CheckState: TCheckBoxState) of object;
  TGridCheckStateExEvent = procedure(Sender: TObject; Cell: TGridCell; var CheckState: TCheckBoxState; var CheckEnabled: Boolean) of object;
  TGridCheckImageEvent = procedure(Sender: TObject; Cell: TGridCell; CheckImage: TBitmap) of object;
  TGridCheckAlignmentEvent = procedure(Sender: TObject; Cell: TGridCell; var CheckAlignment: TAlignment) of object;
  TGridSortDirectionEvent = procedure(Sender: TObject; Section: TGridHeaderSection; var SortDirection: TGridSortDirection) of object;
  TGridSortImageEvent = procedure(Sender: TObject; Section: TGridHeaderSection; SortImage: TBitmap) of object;
  TGridHintEvent = procedure(Sender: TObject; var Value: string) of object;
  TGridColorEvent = procedure(Sender: TObject; var Color: TColor) of object;
  TGridFindTextEvent = procedure(Sender: TObject; const FindText: string) of object;

  TCustomGridView = class(TCustomControl)
  private
    FHorzScrollBar: TGridScrollBar;
    FVertScrollBar: TGridScrollBar;
    FHeader: TCustomGridHeader;
    FColumns: TGridColumns;
    FRows: TCustomGridRows;
    FFixed: TCustomGridFixed;
    FImages: TImageList;
    FImagesLink: TChangeLink;
    FImageLeftIndent: Integer;
    FImageTopIndent: Integer;
    FImageHighlight: Boolean;
    FImageIndexDef: Integer;
    FCellFocused: TGridCell;
    FCellSelected: Boolean;
    FVisOrigin: TGridCell;
    FVisSize: TGridCell;
    FBorderStyle: TBorderStyle;
    FFlatBorder: Boolean;
    FHideSelection: Boolean;
    FShowHeader: Boolean;
    FGridLines: Boolean;
    FGridStyle: TGridStyles;
    FGridColor: TColor;
    FEndEllipsis: Boolean;
    FShowFocusRect: Boolean;
    FAlwaysSelected: Boolean;
    FRowSelect: Boolean;
    FRightClickSelect: Boolean;
    FAllowSelect: Boolean;
    FFocusOnScroll: Boolean;
    FCursorKeys: TGridCursorKeys;
    FTextLeftIndent: Integer;
    FTextRightIndent: Integer;
    FTextTopIndent: Integer;
    FHitTest: TPoint;
    FClickPos: TGridCell;
    FColumnsResize: Boolean;
    FColumnsFullDrag: Boolean;
    FColumnClick: Boolean;
    FColResizeSection: TGridHeaderSection;
    FColResizeLevel: Integer;
    FColResizeIndex: Integer;
    FColResizeOffset: Integer;
    FColResizeRect: TRect;
    FColResizePos: Integer;
    FColResizeMinWidth: Integer;
    FColResizeMaxWidth: Integer;
    FColResizeCount: Integer;
    FHeaderClickSection: TGridHeaderSection;
    FHeaderClickRect: TRect;
    FHeaderClickState: Boolean;
    FHeaderClicking: Boolean;
    FHotSection: TGridHeaderSection;
    FHotColumn: Integer;
    FHotLevel: Integer;
    FUpdateLock: Integer;
    FAllowEdit: Boolean;
    FAlwaysEdit: Boolean;
    FReadOnly: Boolean;
    FEdit: TCustomGridEdit;
    FEditCell: TGridCell;
    FEditing: Boolean;
    FEditPending: Boolean;
    FShowCellTips: Boolean;
    FTipsCell: TGridCell;
    FTipsText: string;
    FCheckBoxes: Boolean;
    FCheckStyle: TGridCheckStyle;
    FCheckBuffer: TBitmap;
    FSortLeftIndent: Integer;
    FSortTopIndent: Integer;
    FSortBuffer: TBitmap;
    FPatternBitmap: TBitmap;
    FCancelOnExit: Boolean;
    FDefaultEditMenu: Boolean;
    FHighlightEvenRows: Boolean;
    FHighlightFocusCol: Boolean;
    FHighlightFocusRow: Boolean;
    FOnGetCellText: TGridTextEvent;
    FOnGetCellTextIndent: TGridCellIndentEvent;
    FOnGetCellColors: TGridCellColorsEvent;
    FOnGetCellImage: TGridCellImageEvent;
    FOnGetCellImageEx: TGridCellImageExEvent;
    FOnGetCellImageIndent: TGridCellIndentEvent;
    FOnGetCellReadOnly: TGridCellreadOnlyEvent;
    FOnGetHeaderColors: TGridHeaderColorsEvent;
    FOnGetHeaderImage: TGridHeaderImageEvent;
    FOnDraw: TGridDrawEvent;
    FOnDrawCell: TGridDrawCellEvent;
    FOnDrawHeader: TGridDrawHeaderEvent;
    FOnColumnSizeToFit: TGridColumnResizeEvent;
    FOnColumnResizing: TGridColumnResizeEvent;
    FOnColumnResize: TGridColumnResizeEvent;
    FOnHeaderClick: TGridHeaderClickEvent;
    FOnHeaderClicking: TGridHeaderClickingEvent;
    FOnChangeColumns: TNotifyEvent;
    FOnChangeRows: TNotifyEvent;
    FOnChangeFixed: TNotifyEvent;
    FOnCellAcceptCursor: TGridCellAcceptCursorEvent;
    FOnChanging: TGridChangingEvent;
    FOnChange: TGridChangedEvent;
    FOnCellClick: TGridCellClickEvent;
    FOnGetEditStyle: TGridEditStyleEvent;
    FOnGetEditMask: TGridTextEvent;
    FOnGetEditText: TGridTextEvent;
    FOnSetEditText: TGridTextEvent;
    FOnGetEditList: TGridEditListEvent;
    FOnGetEditListIndex: TGridEditListIndexEvent;
    FOnGetEditListBounds: TGridRectEvent;
    FOnEditCanModify: TGridEditCanModifyEvent;
    FOnEditCanShow: TGridEditCanShowEvent;
    FOnEditAcceptKey: TGridAcceptKeyEvent;
    FOnEditButtonPress: TGridCellNotifyEvent;
    FOnEditSelectNext: TGridTextEvent;
    FOnEditCloseUp: TGridEditCloseUpEvent;
    FOnEditCloseUpEx: TGridEditCloseUpExEvent;
    FOnEditChange: TGridCellNotifyEvent;
    FOnEditCanceled: TGridCellNotifyEvent;
    FOnGetCheckKind: TGridCheckKindEvent;
    FOnGetCheckState: TGridCheckStateEvent;
    FOnGetCheckStateEx: TGridCheckStateExEvent;
    FOnGetCheckImage: TGridCheckImageEvent;
    FOnGetCheckAlignment: TGridCheckAlignmentEvent;
    FOnGetCheckIndent: TGridCellIndentEvent;
    FOnCheckClick: TGridCellNotifyEvent;
    FOnGetSortDirection: TGridSortDirectionEvent;
    FOnGetSortImage: TGridSortImageEvent;
    FOnChangeEditing: TNotifyEvent;
    FOnChangeEditMode: TNotifyEvent;
    FOnGetCellHintRect: TGridRectEvent;
    FOnCellTips: TGridCellTipsEvent;
    FOnGetTipsRect: TGridRectEvent;
    FOnGetTipsText: TGridTextEvent;
    FGridHint: string;
    FGridHintColor: TColor;
    FShowGridHint: Boolean;
    FOnGetGridHint: TGridHintEvent;
    FGrayReadOnly: Boolean;
    FDefaultHeaderMenu: Boolean;
    FHeaderPopupMenu: TPopupMenu;
    FOnHeaderDetailsClick: TNotifyEvent;
    FOnGetGridColor: TGridColorEvent;
    FFindDialog: TGridFindDialog;
    FOnTextNotFound: TGridFindTextEvent;
    FContextPopupCol: Integer;
    FContextPopupRow: Integer;
    function GetCell(Col, Row: Longint): string;
    function GetChecked(Col, Row: Longint): Boolean;
    function GetCheckBoxEnabled(Col, Row: Longint): Boolean;
    function GetCheckBoxState(Col, Row: Longint): TCheckBoxState;
    function GetCol: Longint;
    function GetFindDialog: TGridFindDialog;
    function GetFixed: TCustomGridFixed;
    function GetEdit: TCustomGridEdit;
    function GetEditColumn: TCustomGridColumn;
    function GetEditDropDown: Boolean;
    function GetEditing: Boolean;
    function GetEditFocused: Boolean;
    function GetHeader: TCustomGridHeader;
    function GetLeftCol: Longint;
    function GetRow: Longint;
    function GetRows: TCustomGridRows;
    function GetTopRow: Longint;
    function GetVisibleColCount: Longint;
    function GetVisibleRowCount: Longint;
    procedure HeaderMenuClick(Sender: TObject);
    procedure HorzScroll(Sender: TObject; {%H-}ScrollCode: Integer; var {%H-}ScrollPos: Integer);
    procedure HorzScrollChange(Sender: TObject);
    procedure SetAllowEdit(Value: Boolean);
    procedure SetAllowSelect(Value: Boolean);
    procedure SetAlwaysEdit(Value: Boolean);
    procedure SetAlwaysSelected(Value: Boolean);
    procedure SetCell(Col, Row: Longint; Value: string);
    procedure SetCellFocused(Value: TGridCell);
    procedure SetCellSelected(Value: Boolean);
    procedure SetCheckBoxes(Value: Boolean);
    procedure SetCheckStyle(Value: TGridCheckStyle);
    procedure SetCol(Value: Longint);
    procedure SetColumns(Value: TGridColumns);
    procedure SetCursorKeys(Value: TGridCursorKeys);
    procedure SetEditDropDown({%H-}Value: Boolean);
    procedure SetEditing(Value: Boolean);
    procedure SetEndEllipsis(Value: Boolean);
    procedure SetFlatBorder(Value: Boolean);
    procedure SetFixed(Value: TCustomGridFixed);
    procedure SetGrayReadOnly(const Value: Boolean);
    procedure SetGridColor(Value: TColor);
    procedure SetGridHint(const Value: string);
    procedure SetGridHintColor(Value: TColor);
    procedure SetGridLines(Value: Boolean);
    procedure SetGridStyle(Value: TGridStyles);
    procedure SetHeader(Value: TCustomGridHeader);
    procedure SetHideSelection(Value: Boolean);
    procedure SetHighlightEvenRows(const Value: Boolean);
    procedure SetHighlightFocusCol(const Value: Boolean);
    procedure SetHighlightFocusRow(const Value: Boolean);
    procedure SetHorzScrollBar(Value: TGridScrollBar);
    procedure SetImageIndexDef(Value: Integer);
    procedure SetImageHighlight(Value: Boolean);
    procedure SetImageLeftIndent(Value: Integer);
    procedure SetImages(Value: TImageList);
    procedure SetImageTopIndent(Value: Integer);
    procedure SetLeftCol(Value: Longint);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRow(Value: Longint);
    procedure SetRows(Value: TCustomGridRows);
    procedure SetRowSelect(Value: Boolean);
    procedure SetShowCellTips(Value: Boolean);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetShowGridHint(Value: Boolean);
    procedure SetShowHeader(Value: Boolean);
    procedure SetSortLeftIndent(Value: Integer);
    procedure SetSortTopIndent(Value: Integer);
    procedure SetTextLeftIndent(Value: Integer);
    procedure SetTextRightIndent(Value: Integer);
    procedure SetTextTopIndent(Value: Integer);
    procedure SetTopRow(Value: Longint);
    procedure SetVertScrollBar(Value: TGridScrollBar);
    procedure SetVisOrigin(Value: TGridCell);
    procedure VertScroll(Sender: TObject; {%H-}ScrollCode: Integer; var {%H-}ScrollPos: Integer);
    procedure VertScrollChange(Sender: TObject);
  protected
    FColResizing: Boolean;
    procedure LMGetDlgCode(var Message: TLMGetDlgCode); message LM_GETDLGCODE;
    procedure LMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure LMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure LMLButtonDown(var Message); message LM_LBUTTONDOWN;
    procedure LMChar(var Msg: TLMChar); message LM_CHAR;
    procedure LMHScroll(var Message: TLMHScroll); message LM_HSCROLL;
    procedure LMVScroll(var Message: TLMVScroll); message LM_VSCROLL;
    procedure LMNCHitTest(var Message: TLMNCHitTest); message LM_NCHITTEST;
    procedure LMEraseBkgnd(var Message: TLMessage); message LM_ERASEBKGND;
    procedure LMTimer(var {%H-}Message); message LM_TIMER;
    procedure LMUser(var {%H-}Message); message LM_USER;
    procedure CMEnabledChanged(var Message); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message); message CM_COLORCHANGED;
    procedure CMShowHintChanged(var {%H-}Message); message CM_SHOWHINTCHANGED;
    procedure CMHintShow(var Message: TLMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message); message CM_MOUSELEAVE;
    procedure CancelCellTips; virtual;
    procedure CellClick(Cell: TGridCell; Shift: TShiftState; X, Y: Integer); virtual;
    procedure CellTips(Cell: TGridCell; out AllowTips: Boolean); virtual;
    procedure Change(var Cell: TGridCell; var Selected: Boolean); virtual;
    procedure ChangeColumns; virtual;
    procedure ChangeEditing; virtual;
    procedure ChangeEditMode; virtual;
    procedure ChangeFixed; virtual;
    procedure ChangeRows; virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure Changing(var Cell: TGridCell; var Selected: Boolean); virtual;
    procedure CheckClick(Cell: TGridCell); virtual;
    procedure ColumnResize(Column: Integer; var AWidth: Integer); virtual;
    procedure ColumnResizing(Column: Integer; var AWidth: Integer); virtual;
    procedure ColumnsChange(Sender: TObject); virtual;
    procedure ColumnSizeToFit(Column: Integer; var AWidth: Integer); virtual;
    function CompareStrings(const S1, S2: string; WholeWord, MatchCase: Boolean): Boolean; virtual;
    function CreateColumn(Columns: TGridColumns): TCustomGridColumn; virtual;
    function CreateColumns: TGridColumns; virtual;
    function CreateEdit(EditClass: TGridEditClass): TCustomGridEdit; virtual;
    function CreateFixed: TCustomGridFixed; virtual;
    function CreateHeader: TCustomGridHeader; virtual;
    function CreateHeaderSection(Sections: TGridHeaderSections): TGridHeaderSection; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    function CreateRows: TCustomGridRows; virtual;
    function CreateScrollBar(Kind: TScrollBarKind): TGridScrollBar; virtual;
    procedure CreateWnd; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoExit; override;
    procedure DoHeaderPopup(MousePos: TPoint; var Handled: Boolean); virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoTextNotFound(const FindText: string); virtual;
    procedure EditButtonPress(Cell: TGridCell); virtual;
    function EditCanAcceptKey(Cell: TGridCell; Key: Char): Boolean; virtual;
    procedure EditCanceled(Cell: TGridCell); virtual;
    function EditCanModify(Cell: TGridCell): Boolean; virtual;
    function EditCanShow(Cell: TGridCell): Boolean; virtual;
    function EditCanUndo(Cell: TGridCell): Boolean; virtual;
    procedure EditChange(Cell: TGridCell); virtual;
    procedure EditCloseUp(Cell: TGridCell; Items: TStrings; ItemIndex: Integer;
      var ItemText: string; var Accept: Boolean); virtual;
    procedure EditSelectNext(Cell: TGridCell; var Value: string); virtual;
    procedure FixedChange(Sender: TObject); virtual;
    procedure GetCellColors(Cell: TGridCell; ACanvas: TCanvas); virtual;
    function GetCellImage(Cell: TGridCell; out OverlayIndex: Integer): Integer; virtual;
    function GetCellImageIndent(Cell: TGridCell): TPoint; virtual;
    function GetCellImageRect(Cell: TGridCell): TRect; virtual;
    function GetCellHintRect(Cell: TGridCell): TRect; virtual;
    function GetCellText(Cell: TGridCell): string; virtual;
    function GetCellTextBounds(Cell: TGridCell): TRect; virtual;
    function GetCellTextIndent(Cell: TGridCell): TPoint; virtual;
    function GetCheckAlignment(Cell: TGridCell): TAlignment; virtual;
    procedure GetCheckImage(Cell: TGridCell; CheckImage: Graphics.TBitmap); virtual;
    function GetCheckSize: Integer;
    function GetCheckIndent(Cell: TGridCell): TPoint; virtual;
    function GetCheckKind(Cell: TGridCell): TGridCheckKind; virtual;
    function GetCheckRect(Cell: TGridCell): TRect; virtual;
    function GetCheckState(Cell: TGridCell): TCheckBoxState; virtual;
    function GetCheckStateEx(Cell: TGridCell; out CheckEnabled: Boolean): TCheckBoxState; virtual;
    function GetClientOrigin: TPoint; override;
    function GetClientRect: TRect; override;
    function GetColumnClass: TGridColumnClass; virtual;
    function GetCursorCell(Cell: TGridCell; Offset: TGridCursorOffset): TGridCell; virtual;
    function GetEditClass: TGridEditClass; virtual;
    procedure GetEditList(Cell: TGridCell; Items: TStrings); virtual;
    procedure GetEditListBounds(Cell: TGridCell; var Rect: TRect); virtual;
    function GetEditListIndex(Cell: TGridCell; Items: TStrings; const ItemText: string): Integer; virtual;
    function GetEditMask(Cell: TGridCell): string; virtual;
    function GetEditStyle(Cell: TGridCell): TGridEditStyle; virtual;
    function GetEditText(Cell: TGridCell): string; virtual;
    function GetFixedDividerColor: TColor; virtual;
    function GetFixedGridColor: TColor; virtual;
    function GetFontHeight(AFont: TFont): Integer;
    function GetFontWidth(AFont: TFont; TextLength: Integer): Integer;
    function GetGridHint: string; virtual;
    function GetHeaderImage(Section: TGridHeaderSection): Integer; virtual;
    procedure GetHeaderColors(Section: TGridHeaderSection; ACanvas: TCanvas); virtual;
    function GetSortArrowSize: TSize; virtual;
    function GetSortDirection(Section: TGridHeaderSection): TGridSortDirection; virtual;
    procedure GetSortImage(Section: TGridHeaderSection; SortImage: TBitmap); virtual;
    function GetTextRect(ACanvas: TCanvas; Rect: TRect; LeftIndent: Integer; Alignment: TAlignment;
      WantReturns, WordWrap: Boolean; const AText: string): TRect; virtual;
    function GetTipsRect(Cell: TGridCell; const TipsText: string): TRect; virtual;
    function GetTipsText(Cell: TGridCell): string; virtual;
    function GetTipsWindowClass: TGridTipsWindowClass; virtual;
    procedure HeaderChange(Sender: TObject); virtual;
    procedure HeaderClick(Section: TGridHeaderSection); virtual;
    procedure HeaderClicking(Section: TGridHeaderSection; var AllowClick: Boolean); virtual;
    procedure HideCursor; virtual;
    procedure HideEdit; virtual;
    procedure HideFocus; virtual;
    procedure ImagesChange(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Paint3DFrame(Rect: TRect; SideFlags: Longint); virtual;
    procedure PaintCell(Cell: TGridCell; Rect: TRect); virtual;
    procedure PaintCells; virtual;
    procedure PaintCheck(Rect: TRect; CheckKind: TGridCheckKind; CheckState: TCheckBoxState; CheckEnabled: Boolean); virtual;
    procedure PaintDotGridLines(Points: Pointer; Count: Integer);
    procedure PaintFixed; virtual;
    procedure PaintFixedGridLines; virtual;
    procedure PaintFreeField; virtual;
    procedure PaintFocus; virtual;
    procedure PaintGridLines; virtual;
    procedure PaintHeader(Section: TGridHeaderSection; Rect: TRect); virtual;
    procedure PaintHeaderBackground(Rect: TRect; AColor: TColor; PaintState: TGridPaintStates); virtual;
    procedure PaintHeaders(DrawFixed: Boolean); virtual;
    procedure PaintHeaderSections(Sections: TGridHeaderSections; DrawFixed: Boolean); virtual;
    procedure PaintResizeLine;
    procedure PaintText(ACanvas: TCanvas; Rect: TRect; LeftIndent: Integer; Alignment: TAlignment;
      WantReturns, WordWrap: Boolean; const AText: string); virtual;
    procedure PreparePatternBitmap(ACanvas: TCanvas; FillColor: TColor; Remove: Boolean); virtual;
    procedure ResetClickPos; virtual;
    procedure DoOnResize; override;
    procedure RowsChange(Sender: TObject); virtual;
    procedure SetBorderStyle(Value: TBorderStyle); override;
    procedure SetEditText(Cell: TGridCell; var Value: string); virtual;
    procedure ShowCursor; virtual;
    procedure ShowEdit; virtual;
    procedure ShowEditChar(C: Char); virtual;
    procedure ShowFocus; virtual;
    procedure StartColResizeX(Section: TGridHeaderSection; X: Integer);
    procedure StartHeaderClick(Section: TGridHeaderSection; X, Y: Integer);
    procedure StepColResizeX(X: Integer);
    procedure StepHeaderClick(X, Y: Integer);
    procedure StopColResize(Abort: Boolean);
    procedure StopHeaderClick(Abort: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AcquireFocus: Boolean; virtual;
    procedure ApplyEdit; virtual;
    procedure ApplyEditText; virtual;
    procedure CancelEdit; virtual;
    procedure DefaultDrawCell(Cell: TGridCell; Rect: TRect); virtual;
    procedure DefaultDrawHeader(Section: TGridHeaderSection; Rect: TRect); virtual;
    procedure DrawDragRect(Cell: TGridCell); virtual;
    function FindText(const AText: string; Options: TFindOptions): Boolean; virtual;
    function GetCellAt(X, Y: Integer): TGridCell; virtual;
    function GetCellRect(Cell: TGridCell): TRect;
    function GetCellsRect(Cell1, Cell2: TGridCell): TRect;
    function GetColumnAtX(X: Integer): Integer; virtual;
    function GetColumnLeftRight(Column: Integer): TRect;
    function GetColumnMaxWidth(Column: Integer): Integer;
    function GetColumnRect(Column: Integer): TRect;
    function GetColumnsRect(Column1, Column2: Integer): TRect;
    function GetColumnsWidth(Column1, Column2: Integer): Integer;
    function GetEditRect(Cell: TGridCell): TRect; virtual;
    function GetFirstImageColumn: Integer;
    function GetFixedRect: TRect; virtual;
    function GetFixedWidth: Integer;
    function GetFocusRect: TRect; virtual;
    function GetGridHeight: Integer;
    function GetGridLineColor(BkColor: TColor): TColor; virtual;
    function GetGridOrigin: TPoint;
    function GetGridRect: TRect; virtual;
    function GetHeaderHeight: Integer;
    function GetHeaderRect: TRect; virtual;
    function GetHeaderSection(ColumnIndex, Level: Integer): TGridHeaderSection;
    function GetLightenColor(AColor: TColor; Amount: Integer): TColor;
    function GetResizeSectionAt(X, Y: Integer): TGridHeaderSection;
    function GetRowAtY(Y: Integer): Integer; virtual;
    function GetRowRect(Row: Integer): TRect;
    function GetRowsRect(Row1, Row2: Integer): TRect;
    function GetRowsHeight(Row1, Row2: Integer): Integer;
    function GetRowTopBottom(Row: Integer): TRect;
    function GetSectionAt(X, Y: Integer): TGridHeaderSection;
    procedure HandlerFind(Sender: TObject);
    procedure HandlerFindNext(Sender: TObject);
    procedure HandlerFindPrev(Sender: TObject);
    procedure HandlerFindMenu(Sender: TObject);
    procedure Invalidate; override;
    procedure InvalidateCell(Cell: TGridCell);
    procedure InvalidateCheck(Cell: TGridCell);
    procedure InvalidateColumn(Column: Integer);
    procedure InvalidateColumns(Column1, Column2: Integer);
    procedure InvalidateEdit;
    procedure InvalidateFixed;
    procedure InvalidateFocus; virtual;
    procedure InvalidateGrid; virtual;
    procedure InvalidateHeader;
    procedure InvalidateRect(Rect: TRect);
    procedure InvalidateRow(Row: Integer); virtual;
    procedure InvalidateRows(Row1, Row2: Integer);
    procedure InvalidateSection(Section: TGridHeaderSection); overload;
    procedure InvalidateSection(ColumnIndex, Level: Integer); overload;
    function IsActiveControl: Boolean;
    function IsCellAcceptCursor(Cell: TGridCell): Boolean; virtual;
    function IsCellEditing(Cell: TGridCell): Boolean;
    function IsCellHighlighted(Cell: TGridCell): Boolean; virtual;
    function IsCellHasCheck(Cell: TGridCell): Boolean; virtual;
    function IsCellHasImage(Cell: TGridCell): Boolean; virtual;
    function IsCellFocused(Cell: TGridCell): Boolean;
    function IsCellReadOnly(Cell: TGridCell): Boolean; virtual;
    function IsCellValid(Cell: TGridCell): Boolean;
    function IsCellValidEx(Cell: TGridCell; CheckPosition, CheckVisible: Boolean): Boolean;
    function IsCellVisible(Cell: TGridCell; PartialOK: Boolean): Boolean;
    function IsColumnVisible(Column: Integer): Boolean;
    function IsEvenRow(Cell: TGridCell): Boolean; virtual;
    function IsFixedVisible: Boolean;
    function IsFocusAllowed: Boolean;
    function IsGridHintVisible: Boolean; virtual;
    function IsHeaderHasImage(Section: TGridHeaderSection): Boolean; virtual;
    function IsHeaderPressed(Section: TGridHeaderSection): Boolean; virtual;
    function IsRowHighlighted(Row: Integer): Boolean; virtual;
    function IsRowVisible(Row: Integer): Boolean;
    procedure LockUpdate;
    procedure MakeCellVisible(Cell: TGridCell; PartialOK: Boolean); virtual;
    procedure ResetEdit; virtual;
    procedure SetGridCursor(Cell: TGridCell; ASelected, AVisible: Boolean); virtual;
    procedure SizeAllColumnsToFit;
    procedure SizeColumnToFit(ColumnIndex: Integer);
    procedure UnLockUpdate(Redraw: Boolean);
    procedure UpdateCursor; virtual;
    procedure UpdateColors; virtual;
    procedure UpdateEdit(Activate: Boolean); virtual;
    procedure UpdateEditContents(SaveText: Boolean); virtual;
    procedure UpdateFixed; virtual;
    procedure UpdateFocus; virtual;
    procedure UpdateFonts; virtual;
    procedure UpdateHeader; virtual;
    procedure UpdateRows; virtual;
    procedure UpdateScrollBars; virtual;
    procedure UpdateScrollPos; virtual;
    procedure UpdateSelection(var Cell: TGridCell; var Selected: Boolean); virtual;
    procedure UpdateText; virtual;
    procedure UpdateVisOriginSize; virtual;
    property AllowEdit: Boolean read FAllowEdit write SetAllowEdit default False;
    property AllowSelect: Boolean read FAllowSelect write SetAllowSelect default True;
    property AlwaysEdit: Boolean read FAlwaysEdit write SetAlwaysEdit default False;
    property AlwaysSelected: Boolean read FAlwaysSelected write SetAlwaysSelected default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property CancelOnExit: Boolean read FCancelOnExit write FCancelOnExit default True;
    property Canvas;
    property Cells[Col, Row: Longint]: string read GetCell write SetCell;
    property CellFocused: TGridCell read FCellFocused write SetCellFocused;
    property CellSelected: Boolean read FCellSelected write SetCellSelected;
    property CheckBoxes: Boolean read FCheckBoxes write SetCheckBoxes default False;
    property Checked[Col, Row: Longint]: Boolean read GetChecked;
    property CheckEnabled[Col, Row: Longint]: Boolean read GetCheckBoxEnabled;
    property CheckState[Col, Row: Longint]: TCheckBoxState read GetCheckBoxState;
    property CheckStyle: TGridCheckStyle read FCheckStyle write SetCheckStyle default csWin95;
    property Col: Longint read GetCol write SetCol;
    property ColumnClick: Boolean read FColumnClick write FColumnClick default True;
    property Columns: TGridColumns read FColumns write SetColumns;
    property ColumnsFullDrag: Boolean read FColumnsFullDrag write FColumnsFullDrag default False;
    property ContextPopupCol: Integer read FContextPopupCol;
    property ContextPopupRow: Integer read FContextPopupRow;
    property CursorKeys: TGridCursorKeys read FCursorKeys write SetCursorKeys default [gkArrows, gkMouse, gkMouseWheel];
    property DefaultEditMenu: Boolean read FDefaultEditMenu write FDefaultEditMenu default False;
    property DefaultHeaderMenu: Boolean read FDefaultHeaderMenu write FDefaultHeaderMenu default False;
    property Edit: TCustomGridEdit read GetEdit;
    property EditCell: TGridCell read FEditCell;
    property EditColumn: TCustomGridColumn read GetEditColumn;
    property EditDropDown: Boolean read GetEditDropDown write SetEditDropDown;
    property Editing: Boolean read GetEditing write SetEditing;
    property EditFocused: Boolean read GetEditFocused;
    property EndEllipsis: Boolean read FEndEllipsis write SetEndEllipsis default True;
    property FindDialog: TGridFindDialog read GetFindDialog;
    property GrayReadOnly: Boolean read FGrayReadOnly write SetGrayReadOnly default False;
    property GridColor: TColor read FGridColor write SetGridColor default clWindow;
    property GridHint: string read FGridHint write SetGridHint;
    property GridHintColor: TColor read FGridHintColor write SetGridHintColor default clGrayText;
    property GridLines: Boolean read FGridLines write SetGridLines default True;
    property GridStyle: TGridStyles read FGridStyle write SetGridStyle default [gsHorzLine, gsVertLine];
    property Fixed: TCustomGridFixed read GetFixed write SetFixed;
    property FlatBorder: Boolean read FFlatBorder write SetFlatBorder default False;
    property FocusOnScroll: Boolean read FFocusOnScroll write FFocusOnScroll default False;
    property Header: TCustomGridHeader read GetHeader write SetHeader;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default False;
    property HighlightEvenRows: Boolean read FHighlightEvenRows write SetHighlightEvenRows default False;
    property HighlightFocusCol: Boolean read FHighlightFocusCol write SetHighlightFocusCol default False;
    property HighlightFocusRow: Boolean read FHighlightFocusRow write SetHighlightFocusRow default False;
    property HorzScrollBar: TGridScrollBar read FHorzScrollBar write SetHorzScrollBar;
    property ImageIndexDef: Integer read FImageIndexDef write SetImageIndexDef default 0;
    property ImageHighlight: Boolean read FImageHighlight write SetImageHighlight default True;
    property ImageLeftIndent: Integer read FImageLeftIndent write SetImageLeftIndent default 2;
    property Images: TImageList read FImages write SetImages;
    property ImageTopIndent: Integer read FImageTopIndent write SetImageTopIndent default 1;
    property LeftCol: Longint read GetLeftCol write SetLeftCol;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default True;
    property Row: Longint read GetRow write SetRow;
    property Rows: TCustomGridRows read GetRows write SetRows;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
    property ShowCellTips: Boolean read FShowCellTips write SetShowCellTips;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default True;
    property ShowGridHint: Boolean read FShowGridHint write SetShowGridHint default False;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property SortLeftIndent: Integer read FSortLeftIndent write SetSortLeftIndent default 4;
    property SortTopIndent: Integer read FSortTopIndent write SetSortTopIndent default 0;
    property TextLeftIndent: Integer read FTextLeftIndent write SetTextLeftIndent default 5;
    property TextRightIndent: Integer read FTextRightIndent write SetTextRightIndent default 5;
    property TextTopIndent: Integer read FTextTopIndent write SetTextTopIndent default 0;
    property TipsCell: TGridCell read FTipsCell;
    property TipsText: string read FTipsText;
    property TopRow: Longint read GetTopRow write SetTopRow;
    property UpdateLock: Integer read FUpdateLock;
    property VertScrollBar: TGridScrollBar read FVertScrollBar write SetVertScrollBar;
    property VisibleColCount: Longint read GetVisibleColCount;
    property VisibleRowCount: Longint read GetVisibleRowCount;
    property VisOrigin: TGridCell read FVisOrigin write SetVisOrigin;
    property VisSize: TGridCell read FVisSize;
    property OnCellAcceptCursor: TGridCellAcceptCursorEvent read FOnCellAcceptCursor write FOnCellAcceptCursor;
    property OnCellClick: TGridCellClickEvent read FOnCellClick write FOnCellClick;
    property OnCellTips: TGridCellTipsEvent read FOnCellTips write FOnCellTips;
    property OnChange: TGridChangedEvent read FOnChange write FOnChange;
    property OnChangeColumns: TNotifyEvent read FOnChangeColumns write FOnChangeColumns;
    property OnChangeEditing: TNotifyEvent read FOnChangeEditing write FOnChangeEditing;
    property OnChangeEditMode: TNotifyEvent read FOnChangeEditMode write FOnChangeEditMode;
    property OnChangeFixed: TNotifyEvent read FOnChangeFixed write FOnChangeFixed;
    property OnChangeRows: TNotifyEvent read FOnChangeRows write FOnChangeRows;
    property OnChanging: TGridChangingEvent read FOnChanging write FOnChanging;
    property OnCheckClick: TGridCellNotifyEvent read FOnCheckClick write FOnCheckClick;
    property OnColumnResizing: TGridColumnResizeEvent read FOnColumnResizing write FOnColumnResizing;
    property OnColumnResize: TGridColumnResizeEvent read FOnColumnResize write FOnColumnResize;
    property OnColumnSizeToFit: TGridColumnResizeEvent read FOnColumnSizeToFit write FOnColumnSizeToFit;
    property OnDraw: TGridDrawEvent read FOnDraw write FOnDraw;
    property OnDrawCell: TGridDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnDrawHeader: TGridDrawHeaderEvent read FOnDrawHeader write FOnDrawHeader;
    property OnEditAcceptKey: TGridAcceptKeyEvent read FOnEditAcceptKey write FOnEditAcceptKey;
    property OnEditButtonPress: TGridCellNotifyEvent read FOnEditButtonPress write FOnEditButtonPress;
    property OnEditCanceled: TGridCellNotifyEvent read FOnEditCanceled write FOnEditCanceled;
    property OnEditCanModify: TGridEditCanModifyEvent read FOnEditCanModify write FOnEditCanModify;
    property OnEditCanShow: TGridEditCanShowEvent read FOnEditCanShow write FOnEditCanShow;
    property OnEditChange: TGridCellNotifyEvent read FOnEditChange write FOnEditChange;
    property OnEditCloseUp: TGridEditCloseUpEvent read FOnEditCloseUp write FOnEditCloseUp;
    property OnEditCloseUpEx: TGridEditCloseUpExEvent read FOnEditCloseUpEx write FOnEditCloseUpEx;
    property OnEditSelectNext: TGridTextEvent read FOnEditSelectNext write FOnEditSelectNext;
    property OnGetCellColors: TGridCellColorsEvent read FOnGetCellColors write FOnGetCellColors;
    property OnGetCellImage: TGridCellImageEvent read FOnGetCellImage write FOnGetCellImage;
    property OnGetCellImageEx: TGridCellImageExEvent read FOnGetCellImageEx write FOnGetCellImageEx;
    property OnGetCellImageIndent: TGridCellIndentEvent read FOnGetCellImageIndent write FOnGetCellImageIndent;
    property OnGetCellHintRect: TGridRectEvent read FOnGetCellHintRect write FOnGetCellHintRect;
    property OnGetCellReadOnly: TGridCellreadOnlyEvent read FOnGetCellReadOnly write FOnGetCellReadOnly;
    property OnGetCellText: TGridTextEvent read FOnGetCellText write FOnGetCellText;
    property OnGetCellTextIndent: TGridCellIndentEvent read FOnGetCellTextIndent write FOnGetCellTextIndent;
    property OnGetCheckAlignment: TGridCheckAlignmentEvent read FOnGetCheckAlignment write FOnGetCheckAlignment;
    property OnGetCheckImage: TGridCheckImageEvent read FOnGetCheckImage write FOnGetCheckImage;
    property OnGetCheckIndent: TGridCellIndentEvent read FOnGetCheckIndent write FOnGetCheckIndent;
    property OnGetCheckKind: TGridCheckKindEvent read FOnGetCheckKind write FOnGetCheckKind;
    property OnGetCheckState: TGridCheckStateEvent read FOnGetCheckState write FOnGetCheckState;
    property OnGetCheckStateEx: TGridCheckStateExEvent read FOnGetCheckStateEx write FOnGetCheckStateEx;
    property OnGetEditList: TGridEditListEvent read FOnGetEditList write FOnGetEditList;
    property OnGetEditListBounds: TGridRectEvent read FOnGetEditListBounds write FOnGetEditListBounds;
    property OnGetEditListIndex: TGridEditListIndexEvent read FOnGetEditListIndex write FOnGetEditListIndex;
    property OnGetEditMask: TGridTextEvent read FOnGetEditMask write FOnGetEditMask;
    property OnGetEditStyle: TGridEditStyleEvent read FOnGetEditStyle write FOnGetEditStyle;
    property OnGetEditText: TGridTextEvent read FOnGetEditText write FOnGetEditText;
    property OnGetGridHint: TGridHintEvent read FOnGetGridHint write FOnGetGridHint;
    property OnGetGridColor: TGridColorEvent read FOnGetGridColor write FOnGetGridColor;
    property OnGetHeaderColors: TGridHeaderColorsEvent read FOnGetHeaderColors write FOnGetHeaderColors;
    property OnGetHeaderImage: TGridHeaderImageEvent read FOnGetHeaderImage write FOnGetHeaderImage;
    property OnGetSortDirection: TGridSortDirectionEvent read FOnGetSortDirection write FOnGetSortDirection;
    property OnGetSortImage: TGridSortImageEvent read FOnGetSortImage write FOnGetSortImage;
    property OnGetTipsRect: TGridRectEvent read FOnGetTipsRect write FOnGetTipsRect;
    property OnGetTipsText: TGridTextEvent read FOnGetTipsText write FOnGetTipsText;
    property OnHeaderClick: TGridHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderClicking: TGridHeaderClickingEvent read FOnHeaderClicking write FOnHeaderClicking;
    property OnHeaderDetailsClick: TNotifyEvent read FOnHeaderDetailsClick write FOnHeaderDetailsClick;
    property OnSetEditText: TGridTextEvent read FOnSetEditText write FOnSetEditText;
    property OnTextNotFound: TGridFindTextEvent read FOnTextNotFound write FOnTextNotFound;
  end;

{ TGridView }

  TGridView = class(TCustomGridView)
  published
    property Align;
    property AllowEdit;
    property AllowSelect;
    property AlwaysEdit;
    property AlwaysSelected;
    property Anchors;
    property BorderStyle;
    property CancelOnExit;
    property CheckBoxes;
    property CheckStyle;
    property Color;
    property ColumnClick;
    property Columns;
    property ColumnsFullDrag;
    property Constraints;
    property CursorKeys;
    property DefaultEditMenu;
    property DefaultHeaderMenu;
    property DragCursor;
    property DragMode;
    property DoubleBuffered default False;
    property Enabled;
    property EndEllipsis;
    property Fixed;
    property FlatBorder;
    property FocusOnScroll;
    property Font;
    property GrayReadOnly;
    property GridColor;
    property GridHint;
    property GridHintColor;
    property GridLines;
    property GridStyle;
    property Header;
    property HideSelection;
    property HighlightEvenRows;
    property HighlightFocusCol;
    property HighlightFocusRow;
    property Hint;
    property HorzScrollBar;
    property ImageIndexDef;
    property ImageHighlight;
    property Images;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property Rows;
    property RowSelect;
    property ShowCellTips;
    property ShowFocusRect;
    property ShowHeader;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property VertScrollBar;
    property Visible;
    property OnCellAcceptCursor;
    property OnCellClick;
    property OnCellTips;
    property OnChange;
    property OnChangeColumns;
    property OnChangeEditing;
    property OnChangeEditMode;
    property OnChangeFixed;
    property OnChangeRows;
    property OnChanging;
    property OnCheckClick;
    property OnClick;
    property OnColumnResizing;
    property OnColumnResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDraw;
    property OnDrawCell;
    property OnDrawHeader;
    property OnEditAcceptKey;
    property OnEditButtonPress;
    property OnEditCanceled;
    property OnEditCanModify;
    property OnEditCanShow;
    property OnEditChange;
    property OnEditCloseUp;
    property OnEditSelectNext;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCellColors;
    property OnGetCellImage;
    property OnGetCellImageIndent;
    property OnGetCellHintRect;
    property OnGetCellReadOnly;
    property OnGetCellText;
    property OnGetCellTextIndent;
    property OnGetCheckAlignment;
    property OnGetCheckImage;
    property OnGetCheckIndent;
    property OnGetCheckKind;
    property OnGetCheckState;
    property OnGetEditList;
    property OnGetEditListBounds;
    property OnGetEditMask;
    property OnGetEditStyle;
    property OnGetEditText;
    property OnGetHeaderColors;
    property OnGetHeaderImage;
    property OnGetSortDirection;
    property OnGetSortImage;
    property OnGetTipsRect;
    property OnGetTipsText;
    property OnHeaderClick;
    property OnHeaderClicking;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnSetEditText;
    property OnStartDrag;
  end;

