unit SMCnst;

interface

{English strings}
const
  strMessage = 'Tlaè...';
  strSaveChanges = 'Chcete zapísa zmeny do Databázového Servera?';
  strErrSaveChanges = 'Nemono vykona zápis. Skontrolujte platnos dát alebo konektivitu so Serverom.';
  strDeleteWarning = 'Chcete naozaj vymaza tabu¾ku %s?';
  strEmptyWarning = 'Chcete naozaj vyprázni tabu¾ku %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Novı záznam',
    'Vloi záznam',
    'Oprava',
    'Vymaza záznam',
    '-',
    'Tlaè ...',
    'Export ...',
    '-',
    'Uloi zmeny',
    'Vráti zmeny',
    'Refresh',
    '-',
    'Oznaèi/ODznaèi záznamy',
       'Oznaèi záznam',
       'Oznaèi Všetky záznamy',
       '-',
       'ODznaèi záznam',
       'ODznaèi Všetky záznamy',
    '-',
    'Uloi štruktúru ståpcov',
    'Naèíta štruktúru ståpcov',
    '-',
    'Setup...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Titulok ';
  SgbData = ' Data ';
  STitleCaption = 'Nálepka:';
  STitleAlignment = 'Zarovnanie:';
  STitleColor = 'Pozadie:';
  STitleFont = 'Písmo:';
  SWidth = 'Šírka:';
  SWidthFix = 'znaky';
  SAlignLeft = 'v¾avo';
  SAlignRight = 'vpravo';
  SAlignCenter = 'do stredu';

const //for TSMDBFilterDialog
  strEqual = 'rovná sa';
  strNonEqual = 'nerovná sa';
  strNonMore = 'je menší alebo sa rovná';
  strNonLess = 'je väèší alebo sa rovná';
  strLessThan = 'je menší';
  strLargeThan = 'je väèší';
  strExist = 'prázdny';
  strNonExist = 'neprázdny';
  strIn = 'sa nachádza v';
  strBetween = 'je v rozsahu';

  strOR = 'alebo';
  strAND = 'a súèastne';

  strField = 'Poloka';
  strCondition = 'Podmienka';
  strValue = 'Hodnota';

  strAddCondition = ' Definovanie dodatoènej podmienky: ';
  strSelection = ' Zoznam záznamov pre následné podmienky:';

  strAddToList = 'Prida do zoznamu';
  strDeleteFromList = 'Odstráni zo zoznamu';

  strTemplate = 'Filter template dialog';
  strFLoadFrom = 'Naèíta z...';  // New constatns
  strFSaveAs = 'Uloi ako..';
  strFDescription = 'Popis';
  strFFileName = 'Názov';
  strFCreate = 'Vytvorené : %s';
  strFModify = 'Upravené  : %s';
  strFProtect = 'Zamedzi prepísaniu';
  strFProtectErr = 'Súbor nemono prapísa !';
  
const //for SMDBNavigator
  SFirstRecord = 'Prvı záznam';
  SPriorRecord = 'Predchádzajúci record';
  SNextRecord = 'Ïalší záznam';
  SLastRecord = 'Poslednı záznam';
  SInsertRecord = 'Vloi záznam';
  SCopyRecord = 'Kopírova záznam';
  SDeleteRecord = 'Vymaza záznam';
  SEditRecord = 'Upravi záznam';
  SFilterRecord = 'Filtrovacia podmienka';
  SFindRecord = 'H¾adanie v záznamoch';
  SPrintRecord = 'Tlaèenie záznamov';
  SExportRecord = 'Exportovanie záznamov';
  SPostEdit = 'Uloenie zmien';
  SCancelEdit = 'Ukonèenie zmien';
  SRefreshRecord = 'Obnovi dáta';
  SChoice = 'Vyberte si záznam';
  SClear = 'Ostraòujem vybrané záznamy';
  SDeleteRecordQuestion = 'Vymaza záznam?';
  SDeleteMultipleRecordsQuestion = 'Skutoène chcete vymaza vybrané záznamy?';
  SRecordNotFound = 'Záznam sa nenašiel';

  SFirstName = 'Prvı';
  SPriorName = 'Predchádzajúci';
  SNextName = 'Nasledovnı';
  SLastName = 'Poslednı';
  SInsertName = 'Vloi';
  SCopyName = 'Kopírova';
  SDeleteName = 'Vymaza';
  SEditName = 'Upravi';
  SFilterName = 'Filter';
  SFindName = 'H¾adanie';
  SPrintName = 'Tlaè';
  SExportName = 'Export';
  SPostName = 'Uloi';
  SCancelName = 'Storno';
  SRefreshName = 'Obnovi';
  SChoiceName = 'Vo¾ba';
  SClearName = 'Odstráni';

  SBtnOk = '&OK';
  SBtnCancel = '&Návrat';
  SBtnLoad = 'Naèíta';
  SBtnSave = 'Uloi';
  SBtnCopy = 'Kopírova';
  SBtnPaste = 'Vloi';
  SBtnClear = 'Odstráni';

  SRecNo = 'záz.';
  SRecOf = ' z ';

const //for EditTyped
  etValidNumber = 'platné èíslo';
  etValidInteger = 'platná num.hodnota';
  etValidDateTime = 'platnı dátum/èas';
  etValidDate = 'platnı dátum';
  etValidTime = 'platnı èas';
  etValid = 'platnı';
  etIsNot = 'Nie je';
  etOutOfRange = 'Hodnota %s je mimo rozsahu %s..%s';

implementation

end.
