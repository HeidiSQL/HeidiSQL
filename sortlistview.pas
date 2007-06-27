unit SortListView;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

type
  TSortListView = class(TListView)
  private

  protected
     procedure ColClick(Column: TListColumn); override;
  public

  published
end;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('Rentsch', [TSortListView]);
end;

function CustomSortProc(Item1, Item2: TListItem; ParamSort: integer): integer;
stdcall;
var
  i,j,codea,codeb : Integer;
begin
  result:=0;

  if paramsort = 0 then
    if item1.caption <= item2.caption then
      result:=-1
    else
      result:=1;

  if paramsort > 0 then
  begin
    if (item1.subitems.count > paramsort-1) and (item2.subitems.count > paramsort-1) then
    begin
      val(item1.subitems[ParamSort-1], i, codea);
      val(item2.subitems[ParamSort-1], j, codeb);
      if (codea = 0) and (codeb = 0) then
      begin
        if i < j then
          result:=1
        else
          result:=-1;
      end else
      begin
        if item1.subitems[ParamSort-1] <= item2.subitems[ParamSort-1] then
          result:=-1
        else
          result:=1;
      end;
    end;
  end;

end;

procedure tsortlistview.ColClick(Column: TListColumn);
begin inherited colclick(column);
      CustomSort(@CustomSortProc, column.index);
end;
end.
