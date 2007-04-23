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
  PopUpCaption: array [0..24] of string[33] =
   ('Nový záznam',
    'Vložit záznam',
    'Oprava záznamu',
    'Vymazat záznam',
  '-',
    'Tisk ...',
    'Export ...',
    'Filtr ...',
    'Hledej ...',
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
  SWidth = 'šíøka:';
  SWidthFix = 'znaky';
  SAlignLeft = 'vlevo';
  SAlignRight = 'vpravo';
  SAlignCenter = 'na støed';
  
const //for TSMDBFilterDialog
  strEqual = 'rovná se';
  strNonEqual = 'nerovná se';
  strNonMore = 'je menší nebo se rovná';
  strNonLess = 'je vìtší nebo se rovná';
  strLessThan = 'je menší';
  strLargeThan = 'je vìtší';
  strExist = 'prázdný';
  strNonExist = 'neprázdný';
  strIn = 'nachází se v';
  strBetween = 'je v rozsahu';
  strLike = 'obsahuje';

  strOR = 'nebo';
  strAND = 'a zároveò';

  strField          = 'Položka';
  strCondition      = 'Podmínka';
  strValue          = 'Hodnota';

  strAddCondition   = ' Definice dodateèné podmínky: ';
  strSelection      = ' Seznam záznamù pro následné podmínky:';

  strAddToList      = 'Pøidat do seznamu';
  strEditInList     = 'Upravit do seznamu';
  strDeleteFromList = 'Odstranit ze seznamu';

  strTemplate       = 'Filtr vzorového dialogu';
  strFLoadFrom      = 'Èíst z...';  // New constatns
  strFSaveAs        = 'Uložit jako..';
  strFDescription   = 'Popis';
  strFFileName      = 'Název';
  strFCreate        = 'Vytvoøené : %s';
  strFModify        = 'Upravené  : %s';
  strFProtect       = 'Zamezit pøepsání';
  strFProtectErr    = 'Soubor nelze pøepsat !';

const //for SMDBNavigator
  SFirstRecord      = 'První záznam';
  SPriorRecord      = 'Pøedchozí záznam';
  SNextRecord       = 'Další záznam';
  SLastRecord       = 'Poslední záznam';
  SInsertRecord     = 'Pøidat záznam';
  SCopyRecord       = 'Kopírovat záznam';
  SDeleteRecord     = 'Vymazat záznam';
  SEditRecord       = 'Upravit záznam';
  SFilterRecord     = 'Filtrovací podmínka';
  SFindRecord       = 'Hledání v záznamech';
  SPrintRecord      = 'Tisk záznamù';
  SExportRecord     = 'Export záznamù';
  SImportRecord     = 'Import záznamù';
  SPostEdit         = 'Uložení zmìn';
  SCancelEdit       = 'Zrušení zmìn';
  SRefreshRecord    = 'Obnovit data';
  SChoice           = 'Vyberte si záznam';
  SClear            = 'Ostranit vybrané záznamy';
  SDeleteRecordQuestion = 'Smazat záznam?';
  SDeleteMultipleRecordsQuestion = 'Skuteènì chcete smazat vybrané záznamy?';
  SRecordNotFound = 'Záznam nenalezen';


  SFirstName = 'První';
  SPriorName = 'Pøedchozí';
  SNextName = 'Další';
  SLastName = 'Poslední';
  SInsertName = 'Pøidat';
  SCopyName = 'Kopírovat';
  SDeleteName = 'Vymazat';
  SEditName = 'Upravit';
  SFilterName = 'Filtr';
  SFindName = 'Hledání';
  SPrintName = 'Tisk';
  SExportName = 'Export';
  SImportName = 'Import';
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

  SApplyAll = 'Použít na všechny';

  SNoDataToDisplay = '<žádná data k zobrazení>';

implementation

end.
