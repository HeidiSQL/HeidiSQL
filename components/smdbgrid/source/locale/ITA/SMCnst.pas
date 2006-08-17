unit SMCnst;

interface

{Italian strings}
{translated by Giuliano Zorzi, gzorzi@misam.it}
const
  strMessage = 'Stampa...';
  strSaveChanges = 'Salvare le modifiche sul  Database Server?';
  strErrSaveChanges = 'Impeossibile salvare i dati! Controllare la
connessione al server o il metodo di verifica dei dati.';
  strDeleteWarning = 'Candellare la tabella %s?';
  strEmptyWarning = 'Svuotare la tabella %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Aggiungi record',
    'Inserisci record',
    'Insert record',
    'Modifica record',
    'Cancella record',
    '-',
    'Stampa ...',
    'Esporta ...',
    '-',
    'Salva modifiche',
    'Annulla modifiche',
    'Rileggi',
    '-',
    'Seleziona / Deseleziona records',
       'Seleziona record',
       'Seleziona tutti i records',
       '-',
       'Deseleziona record',
       'Deseleziona tutti i records',
    '-',
    'Salva colonne',
    'Ripristina colonne',
    '-',
    'Impostazioni...');

const //for TSMSetDBGridDialog
   SgbTitle = 'Titolo';
   SgbData = ' Dati ';
   STitleCaption = 'Intestazione:';
   STitleAlignment = 'Allineamento:';
   STitleColor = 'Sfondo:';
   STitleFont = 'Primo piano:';
   SWidth = 'Altezza:';
   SWidthFix = 'Caratteri';
   SAlignLeft = 'Sinistra';
   SAlignRight = 'Destra';
   SAlignCenter = 'Centro';

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
  SFirstRecord = 'Primo record';
  SPriorRecord = 'Record precedente';
  SNextRecord = 'Record successivo';
  SLastRecord = 'Ultimo record';
  SInsertRecord = 'Inserisci record';
  SCopyRecord = 'Copia record';
  SDeleteRecord = 'Cancella record';
  SEditRecord = 'Modifica record';
  SFilterRecord = 'Filtri';
  SFindRecord = 'Ricerca';
  SPrintRecord = 'Stampa records';
  SExportRecord = 'Esporta records';
  SPostEdit = 'Salva modifiche';
  SCancelEdit = 'Elimina modifiche';
  SRefreshRecord = 'Refresh data';
  SChoice = 'Scegli un record';
  SClear = 'Elimina la scelta di un record';
  SDeleteRecordQuestion = 'Cancella il record?';
  SDeleteMultipleRecordsQuestion = 'Cancellare i records selezionati?';
  SRecordNotFound = 'Record non trovato';

  SFirstName = 'Primo';
  SPriorName = 'Precedente';
  SNextName = 'Successivo';
  SLastName = 'Ultimo';
  SInsertName = 'Inserisci';
  SCopyName = 'Copia';
  SDeleteName = 'Cancella';
  SEditName = 'Modifica';
  SFilterName = 'Filtro';
  SFindName = 'Trova';
  SPrintName = 'Stampa';
  SExportName = 'Esporta';
  SPostName = 'Salva';
  SCancelName = 'Annulla';
  SRefreshName = 'Rileggi';
  SChoiceName = 'Scegli';
  SClearName = 'Cancella';

  SBtnOk = '&OK';
  SBtnCancel = '&Cancella';
  SBtnLoad = 'Apri';
  SBtnSave = 'Salva';
  SBtnCopy = 'Copia';
  SBtnPaste = 'Incolla';
  SBtnClear = 'Cancella';

  SRecNo = 'rec.';
  SRecOf = ' di ';

const //for EditTyped
  etValidNumber = 'numero valido';
  etValidInteger = 'numero intero valido';
  etValidDateTime = 'data/ora valida';
  etValidDate = 'data valida';
  etValidTime = 'ora valida';
  etValid = 'valido';
  etIsNot = 'non e un';
  etOutOfRange = 'Il valore %s non e compreso in %s..%s';


implementation

end.
