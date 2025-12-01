unit generic_types;

{$mode ObjFPC}

interface

type
  TThreeStateBoolean = (nbUnset, nbFalse, nbTrue);

  TFileExtImageIndex = record
    Ext: String;
    ImageIndex: Integer;
  end;

  function GetFileExtImageIndex(Ext: String): Integer;

const FileExtImageIndex: array[0..16] of TFileExtImageIndex = (
  (Ext: 'csv';           ImageIndex: 50),
  (Ext: 'html';          ImageIndex: 32),
  (Ext: 'xml';           ImageIndex: 48),
  (Ext: 'sql';           ImageIndex: 201),
  (Ext: 'LaTeX';         ImageIndex: 153),
  (Ext: 'textile';       ImageIndex: 154),
  (Ext: 'jira-textile';  ImageIndex: 154),
  (Ext: 'php';           ImageIndex: 202),
  (Ext: 'md';            ImageIndex: 199),
  (Ext: 'json';          ImageIndex: 200),
  (Ext: 'jsonl';         ImageIndex: 200),
  (Ext: 'txt';           ImageIndex: 67),
  (Ext: 'zip';           ImageIndex: 53),
  (Ext: 'png';           ImageIndex: 47),
  (Ext: 'jpg';           ImageIndex: 47),
  (Ext: 'pdf';           ImageIndex: 44),
  (Ext: 'sqlite3';       ImageIndex: 196)
  );


implementation

function GetFileExtImageIndex(Ext: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i:=Low(FileExtImageIndex) to High(FileExtImageIndex) do begin
    if (FileExtImageIndex[i].Ext = Ext) or ('.'+FileExtImageIndex[i].Ext = Ext) then begin
      Result := FileExtImageIndex[i].ImageIndex;
      break;
    end;
  end;
end;

end.
