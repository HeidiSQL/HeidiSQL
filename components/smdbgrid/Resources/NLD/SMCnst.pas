unit SMCnst;

interface

{Dutch strings}
{translated by sam francke s.j.francke@hccnet.nl}
const
  strMessage = 'Print...';
  strSaveChanges = 'Wilt u werkelijk de veranderingen bewaren op de Database Server?';
  strErrSaveChanges = 'Kan data niet bewaren ! Check Server verbinding of validate de data.';
  strDeleteWarning = 'Wilt u werkelijk tabel %s verwijderen ?';
  strEmptyWarning = 'Wilt u werkelijk tabel %s leegmaken?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('voeg record toe',
    'record invoegen',
    'wijzig record',
    'verwijder record',
    '-',
    'print ...',
    'export ...',
    '-',
    'bewaar veranderingen',
    'doe veranderingen teniet',
    'verversen',
    '-',
    'selecteer/deselecteer records',
       'selecteer record',
       'selecteer alle records',
       '-',
       'deselecteer record',
       'deselecteer alle records',
    '-',
    'bewaar kolom layout',
    'herstel kolom layout',
    '-',
    'setup...');

const //for TSMSetDBGridDialog
   SgbTitle = ' titel ';
   SgbData = ' gegevens ';
   STitleCaption = 'kop:';
   STitleAlignment = 'uitlijnen:';
   STitleColor = 'achtergrond:';
   STitleFont = 'font:';
   SWidth = 'breedte:';
   SWidthFix = 'letters';
   SAlignLeft = 'links uitlijnen';
   SAlignRight = 'rechts uitlijen';
   SAlignCenter = 'centreren';
 
const //for TSMDBFilterDialog
  strEqual = 'equal';
  strNonEqual = 'not equal';
  strNonMore = 'no greater';
  strNonLess = 'no less';
  strLessThan = 'less than';
  strLargeThan = 'greater than';
  strExist = 'empty';
  strNonExist = 'not empty';
  strIn = 'in list';
  strBetween = 'between';

  strOR = 'OR';
  strAND = 'AND';

  strField = 'Field';
  strCondition = 'Condition';
  strValue = 'Value';

  strAddCondition = ' Define the additional condition:';
  strSelection = ' Select the records by the next conditions:';

  strAddToList = 'Add to list';
  strDeleteFromList = 'Delete from list';

  strTemplate = 'Filter template dialog';
  strFLoadFrom = 'Load from...';
  strFSaveAs = 'Save as..';
  strFDescription = 'Description';
  strFFileName = 'File name';
  strFCreate = 'Created: %s';
  strFModify = 'Modified: %s';
  strFProtect = 'Protect for rewrite';
  strFProtectErr = 'File is protected!';

const //for SMDBNavigator
  SFirstRecord = 'eerste record';
  SPriorRecord = 'vorige record';
  SNextRecord = 'volgende record';
  SLastRecord = 'laatste record';
  SInsertRecord = 'voeg record in';
  SCopyRecord = 'kopieer record';
  SDeleteRecord = 'verwijder record';
  SEditRecord = 'wijzig record';
  SFilterRecord = 'filter voorwaarden';
  SFindRecord = 'zoek het record';
  SPrintRecord = 'print de records';
  SExportRecord = 'exporteer de records';
  SPostEdit = 'bewaar veranderingen';
  SCancelEdit = 'doe veranderingen teniet';
  SRefreshRecord = 'ververs data';
  SChoice = 'kies record';
  SClear = 'Clear record keuze';
  SDeleteRecordQuestion = 'verwijder record?';
  SDeleteMultipleRecordsQuestion = 'alle geselecteerde records verwijderen?';
  SRecordNotFound = 'geen record gevonden';

  SFirstName = 'eerste';
  SPriorName = 'vorige';
  SNextName = 'volgende';
  SLastName = 'laatste';
  SInsertName = 'invoegen';
  SCopyName = 'kopieer';
  SDeleteName = 'verwijder';
  SEditName = 'wijzig';
  SFilterName = 'filter';
  SFindName = 'vind';
  SPrintName = 'print';
  SExportName = 'export';
  SPostName = 'bewaar';
  SCancelName = 'afbreken';
  SRefreshName = 'ververs';
  SChoiceName = 'kies';
  SClearName = 'maak leeg';

  SBtnOk = 'OK';
  SBtnCancel = 'annuleren';
  SBtnLoad = 'openen';
  SBtnSave = 'opslaan';
  SBtnCopy = 'kopieren';
  SBtnPaste = 'plakken';
  SBtnClear = 'Clear';
 
  SRecNo = 'rec.';
  SRecOf = ' van ';

const //for EditTyped
  etValidNumber = 'geldig nummer';
  etValidInteger = 'geldig integer nummer';
  etValidDateTime = 'geldige datum/tijd';
  etValidDate = 'geldige datum';
  etValidTime = 'geldige tijd';
  etValid = 'geldig(e)';
  etIsNot = 'is geen';
  etOutOfRange = 'waarde %s buiten bereik %s..%s';


implementation

end.
