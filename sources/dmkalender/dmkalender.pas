unit DMKalender;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics,
  Types, StdCtrls, Buttons, Grids, ExtCtrls;

// ---------------------------------------------------------------------- //
// TDMKalender                                                            //
// Copyrights 2016-2022 by Meister                                        //
// Version: 0.5.03.01 2022-06-17                                          //
// ---------------------------------------------------------------------- //

{
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

Type

  { TDMKalender }

  TDMKalender = class(TPanel)
  private
    // Font for the TStringGrid and the actual date label
    FHeaderColor    : TColor;
    FHeaderFontColor: TColor;
    FDateColor      : TColor;
    FDateFontColor  : TColor;
    FWeekEndColor   : TColor;
    FWeekEndFontColor: TColor;
    // some other property variables
    FCreating       : Boolean;
    FMondayFirst    : Boolean;         // True: Monday is first day of the week
    FKalDatum       : TDateTime;       // the actual calendar's date
    FNotKalClkd     : Boolean;         //
    FFirstCell      : Integer;         // column with first day of actual month
    FLastCell       : Integer;         // column with last day of actual month
    FLastRow        : Integer;         // row with last day of actual month
    FTageAktMonat   : Integer;         // days in actual month
    FTageVormonat   : Integer;         // days in previous month showing
    FOnDateChange   : TNotifyEvent;    // OnChange event of the calendar
    // the sub components of the calendar
    Fbbt_YearPrev   : TBitBtn;
    Fbbt_MonthPrev  : TBitBtn;
    Flab_AktDat     : TLabel;
    Fbbt_MonthNext  : TBitBtn;
    Fbbt_YearNext   : TBitBtn;
    Fsgr_Kalender   : TStringGrid;
    Flab_KW         : TLabel;
    Flab_TNum       : TLabel;
    Fbtn_Today      : TButton;
    // private functions of the calendar
    function GetTagName(col: Integer): string;
    function GetMonatName(mon: Integer): string;
    procedure updateAnzeige;
    function WochentagAusTagNum(jahr, tnum: Integer): Integer;
    function GetDaysPrevMonth(jahr, mon: word): integer;
    function GetFirstCellActMonth(wTag: integer): integer;
    // property functions of the calendar
    procedure SetDateColor(AValue: TColor);
    procedure SetDateFontColor(AValue: TColor);
    procedure SetHeaderColor(AValue: TColor);
    procedure SetHeaderFontColor(AValue: TColor);
    procedure SetWeekEndColor(AValue: TColor);
    procedure SetWeekEndFontColor(AValue: TColor);
    procedure SetMondayFirst(AValue: Boolean);
    procedure SetKalDatum(AValue: TDateTime);
    procedure UpdateKalender;
    // events of the sub components
    procedure bbt_YearPrevClick(Sender: TObject);
    procedure bbt_MonthPrevClick(Sender: TObject);
    procedure bbt_MonthNextClick(Sender: TObject);
    procedure bbt_YearNextClick(Sender: TObject);
    procedure btn_TodayClick(Sender: TObject);
    procedure sgr_KalenderClick(Sender: TObject);
    procedure sgr_KalenderResize(Sender: TObject);
    procedure sgr_KalenderSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure sgr_KalenderPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure sgr_KalenderDrawCell(sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);

  protected
    // the default size of the calendar
    class function GetControlClassDefaultSize: TSize; override;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    
  published
    // the properties of sub components
    // font colorss and background colors
    property MondayFirst: Boolean read FMondayFirst write SetMondayFirst;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor;
    property HeaderFontColor: TColor read FHeaderFontColor
             write SetHeaderFontColor;
    property DateColor      : TColor read FDateColor write SetDateColor;
    property DateFontColor  : TColor read FDateFontColor write SetDateFontColor;
    property WeekEndColor   : TColor read FWeekEndColor write SetWeekEndColor;
    property WeekEndFontColor: TColor read FWeekEndFontColor
             write SetWeekEndFontColor;
    // get/set actual calendar's date
    property KalDatum: TDateTime read FKalDatum write SetKalDatum;
    // OnChange event of the calendar
    property OnChange: TNotifyEvent read FOnDateChange write FOnDateChange;
  end;

procedure Register;

implementation
uses
  SysUtils, DateUtils, LazUTF8, LResources;

ResourceString
  // some of the hint texts
  // TODO: define all of the visible texts
  csOneYearBack   = 'ein Jahr zurück';
  csOneMonthBack  = 'einen Monat zurück';
  csOneMonthAhead = 'einen Monat weiter';
  csOneYearAhead  = 'ein Jahr weiter';
  csGoToToday     = 'den heutigen Tag einstellen';
  csCalendarWeek  = 'Kalenderwoche';
  csDayNumber     = 'Tag im Jahr';
  csFmtDayNumber  = 'Tag: %d';
  csFmtWeekNumber = 'KW: %d';
  csFmtOurDate    = 'dd. mmm yyyy';

//
// register the TDMKalender component
//
procedure Register;
begin
  {$I dmkalender_icon.lrs}
  {$I dmkalender.lrs}
  RegisterComponents('misc', [TDMKalender]);
end;

//
// some useful constants
//
const
  DefCalHeight   = 184;    // component height
  DefCalWidth    = 214;    // component width
  DefTopHead     =   8;    // top of buttons and label
  DefTopGrid     =  32;    // top of the TStringGrid
  DefGridHeight  = 120;    // grid height
  DefGridWidth   = 198;    // grid width
  DefRowHeight   =  17;    // column height
  DefColWidth    =  28;    // column width
  DefTopBott     = 153;    // top of the labels and the button

  CANZ_REIHEN    = 7;
  CANZ_SPALTEN   = 7;

  DefTStyle: TTextStyle = (Alignment: taCenter; Layout: tlCenter;
    SingleLine: True;
    Clipping: True;
    ExpandTabs: False;
    ShowPrefix: False;
    Wordbreak: False;
    Opaque: False;
    SystemFont: True;
    RightToLeft: False;
    EndEllipsis: False);

{ TDMKalender }

//
// create the whole component
//
constructor TDMKalender.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  // set a few parent's properties
  Parent := aOwner AS TWinControl;
  ParentFont := True;
  FCreating := True;
  self.Color := clWhite;

  // set the default width and height of the whole component
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  // set the min width and height  of the whole component
  Constraints.MinHeight := DefCalHeight;
  Constraints.MinWidth := DefCalWidth;

  // set the borders's styles  of the whole component
  BorderStyle := bsSingle;
  BevelInner := bvLowered;
  BevelOuter := bvNone;
  BorderWidth := 2;

  // set some properties' defaults
  FMondayFirst := True;
  FHeaderColor := 15779999;
  FHeaderFontColor := clBlack;
  FWeekEndColor := 16580314;
  FWeekEndFontColor := clRed;
  FDateColor := clWhite;
  FDateFontColor := clNavy;

  // enable the calendar
  Enabled := True;
  Visible := True;

  // create and set the Button previous year
  Fbbt_YearPrev  := TBitBtn.Create(self);
  With Fbbt_YearPrev Do
  begin
    Parent         := self;
    ParentFont     := False;
    Name           := 'bbt_YearPrev';
    Caption        := '';
    ControlStyle := ControlStyle - [csNoDesignSelectable];
    Glyph.LoadFromLazarusResource('pf_links');
    Anchors        := [akTop, akLeft];

    Left           := 8;
    Height         := 22;
    Width          := 23;
    Top            := DefTopHead;

    ParentShowHint := False;
    ShowHint       := True;
    Hint           := csOneYearBack;
    OnClick        := @bbt_YearPrevClick;
    TabOrder       := 0;
    Visible        := True;
    SetSubComponent(true);
  end;

  // create and set the button previous month
  Fbbt_MonthPrev := TBitBtn.Create(self);
  With Fbbt_MonthPrev Do
  begin
    Parent         := self;
    ParentFont     := True;
    Name           := 'bbt_MonthPrev';
    Caption        := '';
    ControlStyle := ControlStyle - [csNoDesignSelectable];
    Glyph.LoadFromLazarusResource('pf_links2');
    Anchors        := [akTop, akLeft];

    Left           := 8 + 23 + 1;        // 32
    Height         := 22;
    Width          := 23;
    Top            := DefTopHead;

    ParentShowHint := False;
    ShowHint       := True;
    Hint           := csOneMonthBack;
    OnClick        := @bbt_MonthPrevClick;
    TabOrder       := 1;
    Visible        := True;
    SetSubComponent(true);
  end;

  // create and set the label wich shows the actual date
  Flab_AktDat    := TLabel.Create(self);
  With Flab_AktDat Do
  begin
    Parent         := self;
    ParentFont     := True;
    ParentColor    := False;
    Font.Color     := clNavy;
    Alignment      := taCenter;
    Layout         := tlCenter;
    ControlStyle   := ControlStyle - [csNoDesignSelectable];

    Left           := 8 + 23 + 1 + 23 + 1; // 56
    Top            := DefTopHead - 1;
    Width          := 102;
    Height         := 22;

    Anchors        := [akTop, akLeft, akRight];
    AutoSize       := False;
    Caption        := '01. Sep 2016';
    Visible        := True;
    SetSubComponent(true);
    Name           := 'lab_AktDat';
  end;

  // create and set the button next month
  Fbbt_MonthNext    := TBitBtn.Create(self);
  With Fbbt_MonthNext Do
  begin
    Parent         := self;
    ParentFont     := False;
    Name           := 'bbt_MonthNext';
    Caption        := '';
    ControlStyle   := ControlStyle - [csNoDesignSelectable];
    Glyph.LoadFromLazarusResource('pf_rechts');
    Anchors        := [akTop, akRight];

    Left           := DefCalWidth - 8 - 23 - 1 - 23;  //199
    Top            := DefTopHead;
    Width          := 23;
    Height         := 22;

    ParentShowHint := False;
    ShowHint       := True;
    Hint           := csOneMonthAhead;
    OnClick        := @bbt_MonthNextClick;
    TabOrder       := 2;
    Visible        := True;
    SetSubComponent(true);
  end;

  // create and set the button next year
  Fbbt_YearNext     := TBitBtn.Create(self);
  With Fbbt_YearNext Do
  begin
    Parent         := self;
    ParentFont     := False;
    Name           := 'bbt_YearNext';
    Caption        := '';
    ControlStyle := ControlStyle - [csNoDesignSelectable];
    Glyph.LoadFromLazarusResource('pf_rechts2');
    Anchors        := [akTop, akRight];

    Left           := DefCalWidth - 8 - 23; // 175
    Top            := DefTopHead;
    Width          := 23;
    Height         := 22;

    ParentShowHint := False;
    ShowHint       := True;
    Hint           := csOneYearAhead;
    OnClick        := @bbt_YearNextClick;
    TabOrder       := 3;
    Visible        := True;
    SetSubComponent(true);
  end;

  // create and set the TStringGrid calendar itselfs ...
  Fsgr_Kalender    := TStringGrid.Create(self);
  With Fsgr_Kalender Do
  begin
    Parent         := self;
    ParentFont     := True;
    ParentColor    := False;
    Name           := 'sgr_Kalender';
    ControlStyle   := ControlStyle - [csNoDesignSelectable];
    FixedColor     := FHeaderColor;
    FixedCols      := 0;
    AutoFillColumns := True;
    AutoEdit       := False;
    DefaultColWidth := DefColWidth;
    DefaultRowHeight := DefRowHeight;
    SetInitialBounds(8, DefTopGrid, DefGridWidth, DefGridHeight);
    Anchors        := [akTop, akLeft, akRight, akBottom];
    ExtendedSelect := False;
    RowCount       := CANZ_REIHEN;
    ColCount       := CANZ_SPALTEN;
    ScrollBars     := ssNone;
    GridLineWidth  := 0;
    TabOrder       := 4;
    TitleStyle     := tsLazarus;
    SetSubComponent(true);

    // ... and its events
    Fsgr_Kalender.OnPrepareCanvas := @sgr_KalenderPrepareCanvas;
    Fsgr_Kalender.OnDrawCell      := @sgr_KalenderDrawCell;
    Fsgr_Kalender.OnSelectCell    := @sgr_KalenderSelectCell;
    Fsgr_Kalender.OnResize        := @sgr_KalenderResize;
    Fsgr_Kalender.OnClick         := @sgr_KalenderClick;
    Visible := True;
  end;

  // create and set the label week of year
  Flab_KW          := TLabel.Create(self);
  With Flab_KW Do
  begin
    Parent         := self;
    ParentFont     := True;
    ControlStyle   := ControlStyle - [csNoDesignSelectable];

    Left           := 8;
    Top            := DefTopBott + 5;
    Width          := 42;
    Height         := 15;

    ParentShowHint := False;
    ShowHint       := True;
    Hint           := csCalendarWeek;
    Anchors        := [akLeft, akBottom];
    Caption        := 'KW: ';
    Visible        := True;
    SetSubComponent(true);
    Name           := 'lab_KW';
  end;
  Flab_KW.ParentFont := True;

  // create and set the label day of year
  Flab_TNum        := TLabel.Create(self);
  With Flab_TNum Do
  begin
    Parent         := self;
    ParentFont     := True;
    ControlStyle   := ControlStyle - [csNoDesignSelectable];

    Left           := 65;
    Top            := DefTopBott + 5;
    Width          := 84;
    Height         := 15;

    ParentShowHint := False;
    ShowHint       := True;
    Hint           := csDayNumber;
    Anchors        := [akLeft, akBottom];
    Caption        := 'Tag im Jahr: ';
    Visible        := True;
    SetSubComponent(true);
    Name           := 'lab_TNum';
  end;
  Flab_TNum.ParentFont := True;

  // create and set the button for today
  Fbtn_Today       := TButton.Create(self);
  With Fbtn_Today Do
  begin
    Parent         := self;
    ParentFont     := True;
    ControlStyle   := ControlStyle - [csNoDesignSelectable];

    Left           := DefCalWidth - 8 - 65;
    Top            := DefTopBott;
    Height         := 23;
    Width          := 65;

    Hint           := csGoToToday;
    Anchors        := [akRight, akBottom];
    Caption        := 'Heute';
    Font.Height    := -12;
    Color          := clWhite;
    OnClick        := @btn_TodayClick;
    ParentShowHint := False;
    ShowHint       := True;
    TabOrder       := 5;
    Visible        := True;
    SetSubComponent(true);
    Name           := 'btn_Today';
  end;
  Fbtn_Today.ParentFont := True;

  // set the date and actualize the calendar
  try
    SetKalDatum(Date);
  finally
    // bäh -- in creating state of the calendar there's an exception:
    // TStringGrid can't focus yet this switch prevents us from this exception
    FCreating := False;
  end;
end;

//
// destroy the whole component
destructor TDMKalender.Destroy;
begin
  // das ererbte Destroy ausführen
  inherited Destroy;
end;

//
// set the default sizes of the component
//
class function TDMKalender.GetControlClassDefaultSize: TSize;
begin
  inherited;
  Result.cx := DefCalWidth;
  Result.cy := DefCalHeight
end;

//
// get the name of a week day: ShortDayNames gets Sunday = 1, ...
// the input col ranges from 0 to 6
//
function TDMKalender.GetTagName(col: Integer): string;
var
  i: integer;
begin
  i := col;
  If FMondayFirst Then Inc(i);
  If (i > CANZ_SPALTEN - 1) Then Dec(i, CANZ_SPALTEN);
  Result := DefaultFormatSettings.ShortDayNames[i + 1];
end;

//
// get the name of a month
//
function TDMKalender.GetMonatName(mon: Integer): string;
begin
  Result := DefaultFormatSettings.LongMonthNames[mon];
end;

//
// update the calendar's labels
//
procedure TDMKalender.updateAnzeige;
begin
  Flab_AktDat.Caption := FormatDateTime(csFmtOurDate, FKalDatum);
  Flab_KW.Caption := Format(csFmtWeekNumber, [WeekOfTheYear(FKalDatum)]);
  Flab_TNum.Caption := Format(csFmtDayNumber, [DayOfTheYear(FKalDatum)]);
end;

//
// calculate the week day from year and day number of the year
// Sunday = 0, Monday = 1, ...
// this is the Zellersche Algorithmus by Christian Zeller (1822-1899)
//
function TDMKalender.WochentagAusTagNum(jahr, tnum: Integer): Integer;
var
  j, c: Integer;
begin

  j := (jahr - 1) MOD 100;
  c := (jahr - 1) DIV 100;

  Result := (28 + j + tnum + (j DIV 4) + (c DIV 4) + 5 * c) MOD 7;

end;

//
// get the number of days of previous month
//
function TDMKalender.GetDaysPrevMonth(jahr, mon: word): integer;
begin
  If (mon = 1) Then
    Result := DaysInAMonth(jahr - 1, 12)
  else
    Result := DaysInAMonth(jahr, mon - 1);
end;

//
// calculate first cell of the actual month in first row
//
function TDMKalender.GetFirstCellActMonth(wTag: integer): integer;
var
  fc: integer;
begin
  // the week days are Sunday = 0, Monday = 1, ...
  // calculate if Monday is first week day
  fc := wTag;
  if FMondayFirst then
    Dec(fc);
  If fc < 0 Then
    fc := 6;
  Result := fc;
end;

//
// property backgorund colour of working days
//
procedure TDMKalender.SetDateColor(AValue: TColor);
begin
  if FDateColor <> AValue then
  begin
    FDateColor := AValue;
    Invalidate;
  end;
end;

//
// property font color of working days
//
procedure TDMKalender.SetDateFontColor(AValue: TColor);
begin
  if FDateFontColor <> AValue then
  begin
    FDateFontColor := AValue;
    Invalidate;
  end;
end;

//
// property backgorund color of columns' headers
//
procedure TDMKalender.SetHeaderColor(AValue: TColor);
begin
  if FHeaderColor <> AValue then
  begin
    FHeaderColor := AValue;
    Fsgr_Kalender.FixedColor := FHeaderColor;
    Invalidate;
  END
end;

//
// property font color of columns' headers
//
procedure TDMKalender.SetHeaderFontColor(AValue: TColor);
begin
  if FHeaderFontColor <> AValue then
  begin
    FHeaderFontColor := AValue;
    Fsgr_Kalender.TitleFont.Color := AValue;
    Invalidate;
  end;
end;

//
// property backgorund color of weekend days
//
procedure TDMKalender.SetWeekEndColor(AValue: TColor);
begin
  if FWeekEndColor <> AValue then
  BEGIN
    FWeekEndColor := AValue;
    Invalidate;
  END
end;

//
// property font color of weekend days
//
procedure TDMKalender.SetWeekEndFontColor(AValue: TColor);
begin
  if FWeekEndFontColor <> AValue then
  begin
    FWeekEndFontColor := AValue;
    Invalidate;
  end;
end;

//
// if first week day changes we have to repaint the TStringGrid
//
procedure TDMKalender.SetMondayFirst(AValue: Boolean);
begin
  if FMondayFirst = AValue then
    Exit;
  FMondayFirst := AValue;
  UpdateKalender;
end;

//
// set a new date to the calendar and repaint the TStringGrid
//
procedure TDMKalender.SetKalDatum(AValue: TDateTime);
begin
  if FKalDatum = AValue Then
    Exit;
  FKalDatum := AValue;
  UpdateKalender;
end;

//
// calc and paint the calendar
//
procedure TDMKalender.UpdateKalender;
var
  jahr, mon, dag: word;
  sDat: TDateTime;
  aktRow, aktCol: Integer;
  hilfsint: Integer;
  FWochentag: Integer;
begin
  // decode actual date
  DecodeDate(FKalDatum, jahr, mon, dag);

  // get the number of days of actual and previous month
  FTageAktMonat := DaysInAMonth(jahr, mon);
  FTageVormonat := GetDaysPrevMonth(jahr, mon);

  // get the day number of first of actual month...
  sDat := EncodeDate(jahr, mon, 1);
  hilfsint := DayOfTheYear(sDat);

  // ... and then the week day (Sunday = 0, Monday = 1, ...)
  FWochentag := WochentagAusTagNum(jahr, hilfsint);

  // get first cell of actual month in first row
  FFirstCell := GetFirstCellActMonth(FWochentag);

  // get first visible day of previous month to show in first row
  hilfsint := FTageVormonat - FFirstCell + 1;
  If (hilfsint < FTageVormonat) Then
    FTageVormonat := hilfsint;

  // calculate last row and last cell (+1) of last day in month
  FLastCell := ((FFirstCell + FTageAktMonat) MOD 7);
  FLastRow  := ((FFirstCell + FTageAktMonat) DIV 7) + 1;

  // calculate the new position of actual day to select it later
  aktRow  := ((FFirstCell + dag) DIV 7) + 1;
  aktCol := ((FFirstCell + dag) MOD 7) - 1;
  if (aktCol < 0) Then
  begin
    Inc(aktCol, 7);
    Dec(aktRow);
  end;

  // repaint the TStringGrid
  Invalidate;

  // set the position of the actual day...
  // TODO: all of following three functions makes one OnSelectCell
  // but without first of it the selecting of right day
  // is failing in some cases
  Fsgr_Kalender.Row := 2;        // set row to a full filled row first
  Fsgr_Kalender.Col := aktCol;
  Fsgr_Kalender.Row := aktRow;

  // ...actualize other calendar's components...
  updateAnzeige;
end;

//
// this event should fire only if we have clicked into the TStringGrid
// but it fires all clicks we do in the whole component
//
procedure TDMKalender.sgr_KalenderClick(Sender: TObject);
begin
  // this doesn't work: all clicks in the component comes here
  If (Sender As TStringGrid) = Fsgr_Kalender then
    FNotKalClkd := False;
end;

//
// resize width and height of the TStringGrid and it's cols and rows
//
procedure TDMKalender.sgr_KalenderResize(Sender: TObject);
begin
  With Fsgr_Kalender Do
  begin
    if ColCount <> 0 then begin
      DefaultColWidth := Width div ColCount;
    end;
    if RowCount <> 0 then begin
      DefaultRowHeight := Height div RowCount;
    end;
  end;
end;

//
// PrepareCanvas: setting the color of text and background for one cell
//
procedure TDMKalender.sgr_KalenderPrepareCanvas(sender: TObject;
  aCol, aRow: Integer; aState: TGridDrawState);

begin
  // ------------------------------------ set the colors of the columns' title
  if (aRow = 0) then
  begin
    (Sender AS TStringGrid).Canvas.Font.Color := FHeaderFontColor;
    (Sender AS TStringGrid).Canvas.Brush.Color := FHeaderColor;
    Exit;
  end;

  // ---------------------------------- it's a day from previous or next month
  // first row is special: maybe it has days from previous month
  If ((aRow = 1) AND (aCol < FFirstCell))

  // FLastRow is special: maybe it has days from next month
  OR ((aRow = FLastRow) AND (aCol >= FLastCell))

  // if there's a row greater than FLastRow then it's the next month only
  OR (aRow > FLastRow) Then
  begin
    // the font.color is the same in every case
    (Sender AS TStringGrid).Canvas.Font.Color := clSilver;
    // column 0 is weekend if NOT Monday is first day of the week
    If (aCol = 0) Then
    begin
      If NOT(FMondayFirst) Then
        (Sender AS TStringGrid).Canvas.Brush.Color := FWeekEndColor
      else
        (Sender AS TStringGrid).Canvas.Brush.Color := FDateColor;
    end
    // column 5 is weekend if Monday is first day of week
    else if (aCol = 5) then
    begin
      If FMondayFirst then
        (Sender AS TStringGrid).Canvas.Brush.Color := FWeekEndColor
      else
        (Sender AS TStringGrid).Canvas.Brush.Color := FDateColor;
    end
    // column 6 is weekend in any case
    else if (aCol = 6) then
      (Sender AS TStringGrid).Canvas.Brush.Color := FWeekEndColor
    // and the other days
    else
      (Sender AS TStringGrid).Canvas.Brush.Color := FDateColor;
    Exit;
  end;

  // ------------------------------------------ it's a day of the actual month
  If (aCol = 0) Then
  begin
    // column 0 is weekend if NOT Monday is first day of the week
    If NOT(FMondayFirst) then
    begin
      (Sender AS TStringGrid).Canvas.Brush.Color := FWeekEndColor;
      (Sender AS TStringGrid).Canvas.Font.Color := FWeekEndFontColor;
    end
    else
    begin
      (Sender AS TStringGrid).Canvas.Brush.Color := FDateColor;
      (Sender AS TStringGrid).Canvas.Font.Color := FDateFontColor;
    end;
  end
  // column 5 is weekend if Monday is first day of week
  else if (aCol = 5) then
  begin
    If FMondayFirst then
    begin
      (Sender AS TStringGrid).Canvas.Brush.Color := FWeekEndColor;
      (Sender AS TStringGrid).Canvas.Font.Color := FWeekEndFontColor;
    end
    else
    begin
      (Sender AS TStringGrid).Canvas.Brush.Color := FDateColor;
      (Sender AS TStringGrid).Canvas.Font.Color := FDateFontColor;
    end;
  end
  // column 6 is weekend in any case
  else if (aCol = 6) then
  begin
    (Sender AS TStringGrid).Canvas.Brush.Color := FWeekEndColor;
    (Sender AS TStringGrid).Canvas.Font.Color := FWeekEndFontColor;
  end
  // and another day
  else
  begin
    (Sender AS TStringGrid).Canvas.Brush.Color := FDateColor;
    (Sender AS TStringGrid).Canvas.Font.Color := FDateFontColor;
  end;

  // if the cell is selcted
  If (gdSelected in aState) Then
  begin
    (Sender AS TStringGrid).Canvas.Brush.Color := clSilver;
    (Sender AS TStringGrid).Canvas.Font.Color := clWhite;
  end;
end;

//
// DrawCell: write the text of one cell
// TODO: this doesn't work at design time
//
procedure TDMKalender.sgr_KalenderDrawCell(sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  at: Integer;

begin
  // row 0 has the week day names
  If (aRow = 0) Then
  begin
    Fsgr_Kalender.Canvas.TextRect(aRect, 0, 0, GetTagName(aCol), DefTStyle);
    Exit;
  end;

  // first row is special: maybe it has days from prev month
  If (aRow = 1) AND (aCol < FFirstCell) Then
  begin
    at := FTageVormonat + aCol;
    Fsgr_Kalender.Canvas.TextRect(aRect, 0, 0, IntToStr(at), DefTStyle);
    Exit;
  end;

  // last row is special: maybe it has days from next month
  If (aRow = FLastRow) AND (aCol >= FLastCell) Then
  begin
    at := aCol + 1 - FLastCell;
    Fsgr_Kalender.Canvas.TextRect(aRect, 0, 0, IntToStr(at), DefTStyle);
    Exit;
  end;

  // if there's a row greater then FLastRow it's the next month only
  If (aRow > FLastRow) Then
  begin
    at := CANZ_SPALTEN - FLastCell + 1;
    at := at + aCol;
    Fsgr_Kalender.Canvas.TextRect(aRect, 0, 0, IntToStr(at), DefTStyle);
    Exit;
  end;

  // it's a day of the actual month
  at := ((aRow - 1) * 7) + (aCol + 1) - FFirstCell;
  Fsgr_Kalender.Canvas.TextRect(aRect, 0, 0, IntToStr(at), DefTStyle);
end;

//
// OnSelectCell event
//
procedure TDMKalender.sgr_KalenderSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
var
  jahr, mon, dag: Word;

begin
  CanSelect := False;

  // we can't select row 0, it has the days's names
  If (aRow = 0) Then
    Exit;

  // we can't selcect days in previous month...
  If ((aRow = 1) AND (aCol < FFirstCell)) Then
    Exit;

  // ...and not in next month
  If (aRow = FLastRow) AND (aCol >= FLastCell) Then
    Exit;
  If (aRow > FLastRow) Then
    Exit;

  // calculate the selected day from its position
  DecodeDate(FKalDatum, jahr, mon, dag);
  dag := ((aRow - 1) * 7) + (aCol + 1) - FFirstCell;
  FKalDatum := EncodeDate(jahr, mon, dag);

  CanSelect := True;

  // update the calendar's labels
  updateAnzeige;

  // and fire the OnChange event
  If Assigned(FOnDateChange) then
    FOnDateChange(self);
end;

//
// set the date of today
//
procedure TDMKalender.btn_TodayClick(Sender: TObject);
begin
  // set the new date
  FNotKalClkd := True;
  SetKalDatum(Date);
end;

//
// set same day of next month (if possible)
//
procedure TDMKalender.bbt_MonthNextClick(Sender: TObject);
var
  jahr, mon, dag: Word;
  diam: Word;
  dat: TDateTime;

begin
  // calculate next month but same day
  DecodeDate(FKalDatum, jahr, mon, dag);
  Inc(mon);
  If mon > 12 Then
  begin
    mon := 1;
    Inc(jahr);
  end;
  diam := DaysInAMonth(jahr, mon);
  If dag > diam Then
    dag := diam;
  dat := EncodeDate(jahr, mon, dag);
  // set the new date
  FNotKalClkd := True;
  SetKalDatum(dat);
end;

//
// set same day of previous month (if possible)
//
procedure TDMKalender.bbt_MonthPrevClick(Sender: TObject);
var
  jahr, mon, dag: Word;
  diam: Word;
  dat: TDateTime;
begin
  // calculate previous month but same day
  DecodeDate(FKalDatum, jahr, mon, dag);
  Dec(mon);
  If mon < 1 Then
  begin
    mon := 12;
    Dec(jahr);
  end;
  diam := DaysInAMonth(jahr, mon);
  If dag > diam Then
    dag := diam;
  dat := EncodeDate(jahr, mon, dag);
  // and set the new date
  FNotKalClkd := True;
  SetKalDatum(dat);
end;

//
// set same month and day of next year (if possible)
//
procedure TDMKalender.bbt_YearNextClick(Sender: TObject);
var
  jahr, mon, dag: Word;
  diam: Word;
  dat: TDateTime;
begin
  // calculate next year but same month and day
  DecodeDate(FKalDatum, jahr, mon, dag);
  Inc(jahr);
  diam := DaysInAMonth(jahr, mon);
  If dag > diam Then
    dag := diam;
  dat := EncodeDate(jahr, mon, dag);
  // and set the new date
  FNotKalClkd := True;
  SetKalDatum(dat);
end;

//
// set same month and day of previous year (if possible)
//
procedure TDMKalender.bbt_YearPrevClick(Sender: TObject);
var
  jahr, mon, dag: Word;
  diam: Word;
  dat: TDateTime;
begin
  // calculate previous year but same month and day
  DecodeDate(FKalDatum, jahr, mon, dag);
  Dec(jahr);
  diam := DaysInAMonth(jahr, mon);
  If dag > diam Then
    dag := diam;
  dat := EncodeDate(jahr, mon, dag);
  // and set the new date
  FNotKalClkd := True;
  SetKalDatum(dat);
end;

initialization
  {$I dmkalender.lrs}

end.

