//**************************************************************************************************
//                                                                                                  
// Unit Vcl.Styles.DbGrid                                                                           
// unit for the VCL Styles Utils                                                                    
// https://github.com/RRUZ/vcl-styles-utils/                                                       
//                                                                                                  
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); 
// you may not use this file except in compliance with the License. You may obtain a copy of the    
// License at http://www.mozilla.org/MPL/                                                           
//                                                                                                  
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   
// ANY KIND, either express or implied. See the License for the specific language governing rights  
// and limitations under the License.                                                               
//                                                                                                  
// The Original Code is Vcl.Styles.DbGrid.pas.                                                      
//                                                                                                  
// The Initial Developer of the Original Code is Rodrigo Ruz V.                                     
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2019 Rodrigo Ruz V.                         
// All Rights Reserved.                                                                             
//                                                                                                  
//**************************************************************************************************
unit Vcl.Styles.DbGrid;

interface

uses
  Winapi.Windows,
  Vcl.Grids,
  Vcl.Graphics,
  Vcl.DBGrids;

type
  TDBGrid = class(Vcl.DBGrids.TDBGrid)
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  end;

implementation

uses
  Data.DB,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Forms,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Controls;

type
   TCustomGridClass = class(TCustomGrid);
   TDbGridHelper = class helper for TCustomDBGrid
  private
    function GetTitleOffset: Byte;
    function GetIndicators: TImageList;
    function GetSelRow: Integer;
    procedure SetSelRow(const Value: Integer);
   public
    property TitleOffset : Byte read GetTitleOffset;
    property Indicators: TImageList read GetIndicators;
    property SelRow: Integer read GetSelRow write SetSelRow;
   end;

{ TDbGridHelper }

function TDbGridHelper.GetIndicators: TImageList;
begin
  with Self do
    Result := FIndicators;
end;

function TDbGridHelper.GetSelRow: Integer;
begin
  with Self do
    Result := FSelRow;
end;

function TDbGridHelper.GetTitleOffset: Byte;
begin
  with Self do
    Result := FTitleOffset;
end;

procedure TDbGridHelper.SetSelRow(const Value: Integer);
begin
  with Self do
    FSelRow := Value;
end;

procedure _WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer; const AText: string; Alignment: TAlignment; ARightToLeft: Boolean);
var
  X: Integer;
begin
  if (ACanvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft) then
    ChangeBiDiModeAlignment(Alignment);
  case Alignment of
    taLeftJustify : X := ARect.Left + DX;
    taRightJustify: X := ARect.Right - ACanvas.TextWidth(AText) - 3;
  else
    X := ARect.Left + (ARect.Right - ARect.Left) shr 1 - (ACanvas.TextWidth(AText) shr 1);
  end;
  ACanvas.TextRect(ARect, X, ARect.Top + DY, AText);
end;


{ TDBGrid }

procedure TDBGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  OldActive: Integer;
  Indicator: TThemedGrid;
  Value: string;
  CurrentColumn: TColumn;
  MultiSelected: Boolean;
  X: Integer;
  LStyleServices: TCustomStyleServices;
  DeltaX: Byte;
  Index: Integer;
begin
  LStyleServices := StyleServices;
  if not TStyleManager.IsCustomStyleActive or (ARow-TitleOffset<0) then
  begin
    inherited DrawCell(ACol, ARow, ARect, AState);
    exit;
  end;

  if csLoading in ComponentState then
  begin
    Canvas.Brush.Color := LStyleServices.GetStyleColor(scGrid);
    Canvas.FillRect(ARect);
    Exit;
  end;

  Dec(ARow, TitleOffset);
  Dec(ACol, IndicatorOffset);

  if (gdFixed in AState) and ([dgRowLines, dgColLines] * Options =
    [dgRowLines, dgColLines]) then
  begin
    Winapi.Windows.InflateRect(ARect, -1, -1);
    DeltaX := 1;
  end
  else
    DeltaX := 2;

  if (gdFixed in AState) and (ACol < 0) then
  begin
    DrawCellBackground(ARect, FixedColor, AState, ACol, ARow);
    if Assigned(DataLink) and DataLink.Active  then
    begin
      MultiSelected := False;

      if ARow >= 0 then
      begin
        OldActive := DataLink.ActiveRecord;
        try
          DataLink.ActiveRecord := ARow;
          MultiSelected :=  (dgMultiSelect in Options) and Datalink.Active and  SelectedRows.Find(Datalink.Datasource.Dataset.Bookmark, Index);;
        finally
          DataLink.ActiveRecord := OldActive;
        end;
      end;

      if (ARow = DataLink.ActiveRecord) or MultiSelected then
      begin
        Indicator := tgIndicatorArrow;
        if DataLink.DataSet <> nil then
          case DataLink.DataSet.State of
            dsEdit: Indicator := tgIndicatorEdit;
            dsInsert: Indicator := tgIndicatorInsert;
            dsBrowse:
              if MultiSelected then
                if (ARow <> DataLink.ActiveRecord) then
                  Indicator := tgIndicatorMultiDot
                else
                  Indicator := tgIndicatorMultiArrow;
          end;
        Indicators.BkColor := FixedColor;
        X := ARect.Right - Indicators.Width - DeltaX;
        if Canvas.CanvasOrientation = coRightToLeft then
          Inc(X);

        if LStyleServices.Enabled and not LStyleServices.IsSystemStyle then
          LStyleServices.DrawElement(Canvas.Handle, LStyleServices.GetElementDetails(Indicator), ARect)
        else
          Indicators.Draw(Canvas, X, (ARect.Top + ARect.Bottom - Indicators.Height) shr 1, Integer(Indicator) - Integer(tgIndicatorArrow), True);

        if ARow = Datalink.ActiveRecord then
          SelRow := ARow + TitleOffset;
      end;
    end;
  end
  else
  with Canvas do
  begin
    CurrentColumn := Columns[ACol];
    if not CurrentColumn.Showing then Exit;
    if not (gdFixed in AState) then
    begin
      Font := CurrentColumn.Font;
      Brush.Color := CurrentColumn.Color;

      if (Brush.Color=LStyleServices.GetStyleColor(scGrid)) then
      begin
        Font.Color := LStyleServices.GetStyleFontColor(sfGridItemNormal);
        Brush.Color := LStyleServices.GetStyleColor(scGrid);
      end;

    end;

    if (DataLink = nil) or not DataLink.Active then
      FillRect(ARect)
    else
    begin
      Value := '';
      OldActive := DataLink.ActiveRecord;
      try
        DataLink.ActiveRecord := ARow;
        if Assigned(CurrentColumn.Field) then
          Value := CurrentColumn.Field.DisplayText;
        if HighlightCell(ACol, ARow, Value, AState) and DefaultDrawing then
          DrawCellHighlight(ARect, AState, ACol, ARow);
        if not Enabled then
          Font.Color := clGrayText;

        if DefaultDrawing then
          _WriteText(Canvas, ARect, 3, 2, Value, CurrentColumn.Alignment, UseRightToLeftAlignmentForField(CurrentColumn.Field, CurrentColumn.Alignment));

        if Columns.State = csDefault then
          DrawDataCell(ARect, CurrentColumn.Field, AState);
        DrawColumnCell(ARect, ACol, CurrentColumn, AState);
      finally
        DataLink.ActiveRecord := OldActive;
      end;
      Canvas.Brush.Style := bsSolid;

      if DefaultDrawing and (gdSelected in AState) and ((dgAlwaysShowSelection in Options) or Focused)
        and not (csDesigning in ComponentState) and not (dgRowSelect in Options)
        and (UpdateLock = 0) and (ValidParentForm(Self).ActiveControl = Self) and
      (FInternalDrawingStyle = gdsThemed) and (Win32MajorVersion >= 6) then
        Winapi.Windows.InflateRect(ARect, -1, -1);

    end;
  end;

  if (gdFixed in AState) and ([dgRowLines, dgColLines] * Options =
     [dgRowLines, dgColLines]) and (FInternalDrawingStyle = gdsClassic) and
     not (gdPressed in AState) then
    Winapi.Windows.InflateRect(ARect, 1, 1);
end;


end.
