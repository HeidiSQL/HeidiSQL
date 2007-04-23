unit SMCnst;

interface

{English strings}
const
  strMessage = 'Tisk...';
  strSaveChanges = 'Chcete zapsat zm�ny do Datab�zov�ho Serveru?';
  strErrSaveChanges = 'Nelze prov�st z�pis. Zkontrolujte platnost dat nebo konektivitu se Serverem.';
  strDeleteWarning = 'Chcete doopravdy vymazat tabulku %s?';
  strEmptyWarning = 'Chcete doopravdy vypr�znit tabulku %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Nov� z�znam',
    'Vlo�it z�znam',
    'Oprava z�znamu',
    'Vymazat z�znam',
  '-',
    'Tisk ...',
    'Export ...',
    'Filtr ...',
    'Hledej ...',
    '-',
    'Ulo�it zm�ny',
    'Vr�tit zm�ny',
    'Obnovit',
       '-',
    'Ozna�it/Odzna�it z�znamy',
       'Ozna�it z�znam',
       'Ozna�it v�echny z�znamy',
       '-',
       'Odzna�it z�znam',
       'Odzna�it v�echny z�znamy',
    '-',
    'Ulo�it strukturu sloupc�',
    'Na��st strukturu sloupc�',
    '-',
    'Nastaven�...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Titulek ';
  SgbData = ' Data ';
  STitleCaption = 'Nadpis:';
  STitleAlignment = 'Zarovn�n�:';
  STitleColor = 'Pozad�:'; 
  STitleFont = 'P�smo:';
  SWidth = '���ka:';
  SWidthFix = 'znaky';
  SAlignLeft = 'vlevo';
  SAlignRight = 'vpravo';
  SAlignCenter = 'na st�ed';
  
const //for TSMDBFilterDialog
  strEqual = 'rovn� se';
  strNonEqual = 'nerovn� se';
  strNonMore = 'je men�� nebo se rovn�';
  strNonLess = 'je v�t�� nebo se rovn�';
  strLessThan = 'je men��';
  strLargeThan = 'je v�t��';
  strExist = 'pr�zdn�';
  strNonExist = 'nepr�zdn�';
  strIn = 'nach�z� se v';
  strBetween = 'je v rozsahu';
  strLike = 'obsahuje';

  strOR = 'nebo';
  strAND = 'a z�rove�';

  strField          = 'Polo�ka';
  strCondition      = 'Podm�nka';
  strValue          = 'Hodnota';

  strAddCondition   = ' Definice dodate�n� podm�nky: ';
  strSelection      = ' Seznam z�znam� pro n�sledn� podm�nky:';

  strAddToList      = 'P�idat do seznamu';
  strEditInList     = 'Upravit do seznamu';
  strDeleteFromList = 'Odstranit ze seznamu';

  strTemplate       = 'Filtr vzorov�ho dialogu';
  strFLoadFrom      = '��st z...';  // New constatns
  strFSaveAs        = 'Ulo�it jako..';
  strFDescription   = 'Popis';
  strFFileName      = 'N�zev';
  strFCreate        = 'Vytvo�en� : %s';
  strFModify        = 'Upraven�  : %s';
  strFProtect       = 'Zamezit p�eps�n�';
  strFProtectErr    = 'Soubor nelze p�epsat !';

const //for SMDBNavigator
  SFirstRecord      = 'Prvn� z�znam';
  SPriorRecord      = 'P�edchoz� z�znam';
  SNextRecord       = 'Dal�� z�znam';
  SLastRecord       = 'Posledn� z�znam';
  SInsertRecord     = 'P�idat z�znam';
  SCopyRecord       = 'Kop�rovat z�znam';
  SDeleteRecord     = 'Vymazat z�znam';
  SEditRecord       = 'Upravit z�znam';
  SFilterRecord     = 'Filtrovac� podm�nka';
  SFindRecord       = 'Hled�n� v z�znamech';
  SPrintRecord      = 'Tisk z�znam�';
  SExportRecord     = 'Export z�znam�';
  SImportRecord     = 'Import z�znam�';
  SPostEdit         = 'Ulo�en� zm�n';
  SCancelEdit       = 'Zru�en� zm�n';
  SRefreshRecord    = 'Obnovit data';
  SChoice           = 'Vyberte si z�znam';
  SClear            = 'Ostranit vybran� z�znamy';
  SDeleteRecordQuestion = 'Smazat z�znam?';
  SDeleteMultipleRecordsQuestion = 'Skute�n� chcete smazat vybran� z�znamy?';
  SRecordNotFound = 'Z�znam nenalezen';


  SFirstName = 'Prvn�';
  SPriorName = 'P�edchoz�';
  SNextName = 'Dal��';
  SLastName = 'Posledn�';
  SInsertName = 'P�idat';
  SCopyName = 'Kop�rovat';
  SDeleteName = 'Vymazat';
  SEditName = 'Upravit';
  SFilterName = 'Filtr';
  SFindName = 'Hled�n�';
  SPrintName = 'Tisk';
  SExportName = 'Export';
  SImportName = 'Import';
  SPostName = 'Ulo�it';
  SCancelName = 'Storno';
  SRefreshName = 'Obnovit';
  SChoiceName = 'Volba';
  SClearName = 'Odstranit';


  SBtnOk = '&OK';
  SBtnCancel = '&Storno';
  SBtnLoad = '��st';
  SBtnSave = 'Ulo�it';
  SBtnCopy = 'Kop�rovat';
  SBtnPaste = 'P�idat';
  SBtnClear = 'Odstranit';

  SRecNo = 'z�z.';
  SRecOf = ' z ';

const //for EditTyped
  etValidNumber = 'platn� ��slo';
  etValidInteger = 'platn� num.hodnota';
  etValidDateTime = 'platn� datum/�as';
  etValidDate = 'platn� datum';
  etValidTime = 'platn� �as';
  etValid = 'platn�';
  etIsNot = 'Nen�';
  etOutOfRange = 'Hodnota %s je mimo rozsahu %s..%s';

  SApplyAll = 'Pou��t na v�echny';

  SNoDataToDisplay = '<��dn� data k zobrazen�>';

implementation

end.
