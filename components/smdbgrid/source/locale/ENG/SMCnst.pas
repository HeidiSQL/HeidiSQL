unit SMCnst;

interface

{English strings}
const
  strMessage = 'Print...';
  strSaveChanges = 'Do you really want to save a changes on the Database Server?';
  strErrSaveChanges = 'Can''t save a data! Check a Server connection or data validation.';
  strDeleteWarning = 'Do you really want to delete a table %s?';
  strEmptyWarning = 'Do you really want to empty a table %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Add record',
    'Insert record',
    'Edit record',
    'Delete record',
    '-',
    'Print ...',
    'Export ...',
    '-',
    'Save changes',
    'Discard changes',
    'Refresh',
    '-',
    'Select/Unselect records',
       'Select record',
       'Select All records',
       '-',
       'UnSelect record',
       'UnSelect All records',
    '-',
    'Save column layout',
    'Restore column layout',
    '-',
    'Setup...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Title ';
  SgbData = ' Data ';
  STitleCaption = 'Caption:';
  STitleAlignment = 'Alignment:';
  STitleColor = 'Background:'; 
  STitleFont = 'Font:';
  SWidth = 'Width:';
  SWidthFix = 'characters';
  SAlignLeft = 'left';
  SAlignRight = 'right';
  SAlignCenter = 'center';
  
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
  SFirstRecord = 'First record';
  SPriorRecord = 'Prev record';
  SNextRecord = 'Next record';
  SLastRecord = 'Last record';
  SInsertRecord = 'Insert record';
  SCopyRecord = 'Copy record';
  SDeleteRecord = 'Delete record';
  SEditRecord = 'Edit record';
  SFilterRecord = 'Filter conditions';
  SFindRecord = 'Search of the record';
  SPrintRecord = 'Print of the records';
  SExportRecord = 'Export of the records';
  SPostEdit = 'Save changes';
  SCancelEdit = 'Cancel changes';
  SRefreshRecord = 'Refresh data';
  SChoice = 'Choose a record';
  SClear = 'Clear a record choose';
  SDeleteRecordQuestion = 'Delete a record?';
  SDeleteMultipleRecordsQuestion = 'Do you really want to delete a selected records?';
  SRecordNotFound = 'Record not found';

  SFirstName = 'First';
  SPriorName = 'Prev';
  SNextName = 'Next';
  SLastName = 'Last';
  SInsertName = 'Insert';
  SCopyName = 'Copy';
  SDeleteName = 'Delete';
  SEditName = 'Edit';
  SFilterName = 'Filter';
  SFindName = 'Find';
  SPrintName = 'Print';
  SExportName = 'Export';
  SPostName = 'Save';
  SCancelName = 'Cancel';
  SRefreshName = 'Refresh';
  SChoiceName = 'Choose';
  SClearName = 'Clear';

  SBtnOk = '&OK';
  SBtnCancel = '&Cancel';
  SBtnLoad = 'Load';
  SBtnSave = 'Save';
  SBtnCopy = 'Copy';
  SBtnPaste = 'Paste';
  SBtnClear = 'Clear';

  SRecNo = 'rec.';
  SRecOf = ' of ';

const //for EditTyped
  etValidNumber = 'valid number';
  etValidInteger = 'valid integer number';
  etValidDateTime = 'valid date/time';
  etValidDate = 'valid date';
  etValidTime = 'valid time';
  etValid = 'valid';
  etIsNot = 'is not a';
  etOutOfRange = 'Value %s out of range %s..%s';

implementation

end.
