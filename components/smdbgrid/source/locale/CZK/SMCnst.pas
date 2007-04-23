unit SMCnst;

interface

{English strings}
const
  strMessage = 'Tisk...';
  strSaveChanges = 'Chcete zapsat zmìny do Databázového Serveru?';
  strErrSaveChanges = 'Nelze provést zápis. Zkontrolujte platnost dat nebo konektivitu se Serverem.';
  strDeleteWarning = 'Chcete doopravdy vymazat tabulku %s?';
  strEmptyWarning = 'Chcete doopravdy vypráznit tabulku %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Nový záznam',
    'Vložit záznam',
    'Oprava záznamu',
    'Vymazat záznam',
    '-',
    'Tisk ...',
    'Export ...',
    '-',
    'Uložit zmìny',
    'Vrátit zmìny',
    'Obnovit',
    '-',
    'Oznaèit/Odznaèit záznamy',
       'Oznaèit záznam',
       'Oznaèit všechny záznamy',
       '-',
       'Odznaèit záznam',
       'Odznaèit všechny záznamy',
    '-',
    'Uložit strukturu sloupcù',
    'Naèíst strukturu sloupcù',
    '-',
    'Nastavení...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Titulek ';
  SgbData = ' Data ';
  STitleCaption = 'Nadpis:';
  STitleAlignment = 'Zarovnání:';
  STitleColor = 'Pozadí:';
  STitleFont = 'Písmo:';
  SWidth = 'Šírka:';
  SWidthFix = 'znaky';
  SAlignLeft = 'vlevo';
  SAlignRight = 'vpravo';
  SAlignCenter = 'na støed';

const //for TSMDBFilterDialog
  strEqual = 'rovná sa';
  strNonEqual = 'nerovná sa';
  strNonMore = 'je menší nebo se rovná';
  strNonLess = 'je vìtší nebo se rovná';
  strLessThan = 'je menší';
  strLargeThan = 'je vìtší';
  strExist = 'prázdny';
  strNonExist = 'neprázdny';
  strIn = 'nachází se v';
  strBetween = 'je v rozsahu';

  strOR = 'nebo';
  strAND = 'a souèastnì';

  strField = 'Položka';
  strCondition = 'Podmínka';
  strValue = 'Hodnota';

  strAddCondition = ' Definice dodateèné podmínky: ';
  strSelection = ' Seznam záznamù pro následné podmínky:';

  strAddToList = 'Pøidat do seznamu';
  strDeleteFromList = 'Odstranit ze seznamu';

  strTemplate = 'Filtr vzorového dialogu';
  strFLoadFrom = 'Èíst z...';  // New constatns
  strFSaveAs = 'Uložit jako..';
  strFDescription = 'Popis';
  strFFileName = 'Název';
  strFCreate = 'Vytvoøené : %s';
  strFModify = 'Upravené  : %s';
  strFProtect = 'Zamezit pøepsání';
  strFProtectErr = 'Soubor nelze pøepsat !';

const //for SMDBNavigator
  SFirstRecord = 'První záznam';
  SPriorRecord = 'Pøedchozí záznam';
  SNextRecord = 'Další záznam';
  SLastRecord = 'Poslední záznam';
  SInsertRecord = 'Pøidat záznam';
  SCopyRecord = 'Kopírovat záznam';
  SDeleteRecord = 'Vymazat záznam';
  SEditRecord = 'Upravit záznam';
  SFilterRecord = 'Filtrovací podmínka';
  SFindRecord = 'Hledání v záznamech';
  SPrintRecord = 'Tist záznamù';
  SExportRecord = 'Export záznamù';
  SPostEdit = 'Uložení zmìn';
  SCancelEdit = 'Ukonèení zmìn';
  SRefreshRecord = 'Obnovit data';
  SChoice = 'Vyberte si záznam';
  SClear = 'Ostraòují vybrané záznamy';
  SDeleteRecordQuestion = 'Vymazat záznam?';
  SDeleteMultipleRecordsQuestion = 'Skuteènì chcete vymazat vybrané záznamy?';
  SRecordNotFound = 'Záznam nenalezen';

  SFirstName = 'První';
  SPriorName = 'Pøedchozí';
  SNextName = 'Další';
  SLastName = 'Poslední';
  SInsertName = 'Pøidat';
  SCopyName = 'Kopírovat';
  SDeleteName = 'Vymazat';
  SEditName = 'Upravit';
  SFilterName = 'Filt';
  SFindName = 'Hledání';
  SPrintName = 'Tlisk';
  SExportName = 'Export';
  SPostName = 'Uložit';
  SCancelName = 'Storno';
  SRefreshName = 'Obnovit';
  SChoiceName = 'Volba';
  SClearName = 'Odstranit';

  SBtnOk = '&OK';
  SBtnCancel = '&Storno';
  SBtnLoad = 'Èíst';
  SBtnSave = 'Uložit';
  SBtnCopy = 'Kopírovat';
  SBtnPaste = 'Pøidat';
  SBtnClear = 'Odstranit';

  SRecNo = 'záz.';
  SRecOf = ' z ';

const //for EditTyped
  etValidNumber = 'platné èíslo';
  etValidInteger = 'platná num.hodnota';
  etValidDateTime = 'platný datum/èas';
  etValidDate = 'platný datum';
  etValidTime = 'platný èas';
  etValid = 'platný';
  etIsNot = 'Není';
  etOutOfRange = 'Hodnota %s je mimo rozsahu %s..%s';

implementation

end.
