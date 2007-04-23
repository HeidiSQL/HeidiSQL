unit SMCnst;

interface

{German strings}
{translated by Thomas Grimm, tgrimm@allegro-itc.de}
{changed by Reinhold Bauer, reinhold.bauer@onlinehome.de, 30.06.2006}
const
  strMessage = 'Drucke...';
  strSaveChanges = 'Sollen die �nderungen auf dem Server gespeichert werden?';
  strErrSaveChanges = 'Kann die Daten nicht speichern! Pr�fen Sie die Serververbindung oder die G�ltigkeit der Daten';
  strDeleteWarning = 'Soll gel�scht werden, Tabelle %s?';
  strEmptyWarning = 'Soll geleert werden, Tabelle %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Datensatz anf�gen',
    'Datensatz einf�gen',
    'Datensatz bearbeiten',
    'Datensatz l�schen',
    '-',
    'Drucke ...',
    'Exportiere ...',
    'Filter ...',
    'Suche ...',
    '-',
    'Speichere �nderungen',
    'Verwerfe �nderungen',
    'Aktualisiere',
    '-',
    'Datensatz ausw�hlen/abw�hlen',
       'Datensatz ausw�hlen',
       'Alle Datens�tze ausw�hlen',
       '-',
       'Datensatz abw�hlen',
       'Alle Datens�tze abw�hlen',
    '-',
    'Spaltenlayout speichern',
    'Spaltenlayout wiederherstellen',
    '-',
    'Optionen...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Titel ';
  SgbData = ' Daten ';
  STitleCaption = 'Beschriftung:';
  STitleAlignment = 'Ausrichtung:';
  STitleColor = 'Farbe:';
  STitleFont = 'Schrift:';
  SWidth = 'Breite:';
  SWidthFix = 'Zeichen';
  SAlignLeft = 'links';
  SAlignRight = 'rechts';
  SAlignCenter = 'zentriert';

const //for TSMDBFilterDialog
  strEqual = 'gleich';
  strNonEqual = 'ungleich';
  strNonMore = 'nicht gr��er';
  strNonLess = 'nicht kleiner';
  strLessThan = 'kleiner als';
  strLargeThan = 'gr��er als';
  strExist = 'leer';
  strNonExist = 'nicht leer';
  strIn = 'in Liste';
  strBetween = 'zwischen';
  strLike = 'wie';

  strOR = 'ODER';
  strAND = 'UND';

  strField = 'Feld';
  strCondition = 'Bedingung';
  strValue = 'Wert';

  strAddCondition = ' Weitere Bedingung eingeben:';
  strSelection = ' Auswahl der Datens�tze gem�� der folgenden Bedingungen:';

  strAddToList = 'Hinzuf�gen';
  strEditInList = 'Editieren';
  strDeleteFromList = 'L�schen';

  strTemplate = 'Filter-Dialog';
  strFLoadFrom = '�ffnen...';
  strFSaveAs = 'Speichern unter...';
  strFDescription = 'Beschreibung';
  strFFileName = 'Dateiname';
  strFCreate = 'Erstellt: %s';
  strFModify = 'Ge�ndert: %s';
  strFProtect = 'Schreibgesch�tzt';
  strFProtectErr = 'Datei ist schreibgesch�tzt';

const //for SMDBNavigator
  SFirstRecord = 'Erster Datensatz';
  SPriorRecord = 'Vorheriger Datensatz';
  SNextRecord = 'N�chster Datensatz';
  SLastRecord = 'Letzter Datensatz';
  SInsertRecord = 'Datensatz einf�gen';
  SCopyRecord = 'Datensatz kopieren';
  SDeleteRecord = 'Datensatz l�schen';
  SEditRecord = 'Datensatz bearbeiten';
  SFilterRecord = 'Filterbedingungen';
  SFindRecord = 'Gefundene Datens�tze';
  SPrintRecord = 'Gedruckte Datens�tze';
  SExportRecord = 'Exportierte Datens�tze';
  SImportRecord = 'Importiere Datens�tze'; 
  SPostEdit = 'Speichere �nderungen';
  SCancelEdit = 'Verwerfe �nderungen';
  SRefreshRecord = 'Aktualisiere Daten';
  SChoice = 'Datensatz ausw�hlen';
  SClear = 'Auswahl l�schen';
  SDeleteRecordQuestion = 'Datensatz l�schen?';
  SDeleteMultipleRecordsQuestion = 'Sollen die ausgew�hlten Datens�tze gel�scht werden?';
  SRecordNotFound = 'Datensatz nicht gefunden';

  SFirstName = 'Erste';
  SPriorName = 'Vorherige';
  SNextName = 'N�chste';
  SLastName = 'Letzte';
  SInsertName = 'Einf�gen';
  SCopyName = 'Kopieren';
  SDeleteName = 'L�schen';
  SEditName = 'Bearbeiten';
  SFilterName = 'Filter';
  SFindName = 'Suche';
  SPrintName = 'Drucken';
  SExportName = 'Exportieren';
  SImportName = 'Importieren';
  SPostName = 'Speichern';
  SCancelName = 'Abbrechen';
  SRefreshName = 'Aktualisieren';
  SChoiceName = 'Ausw�hlen';
  SClearName = 'L�schen';

  SBtnOk = '&OK';
  SBtnCancel = '&Abbrechen';
  SBtnLoad = '�ffnen';
  SBtnSave = 'Speichern';
  SBtnCopy = 'Kopieren';
  SBtnPaste = 'Einf�gen';
  SBtnClear = 'Entfernen';

  SRecNo = '#';
  SRecOf = ' von ';

const //for EditTyped
  etValidNumber = 'g�ltige Zahl';
  etValidInteger = 'g�ltige Ganzzahl';
  etValidDateTime = 'g�ltiges Datum/Zeit';
  etValidDate = 'g�ltiges Datum';
  etValidTime = 'g�ltige Zeit';
  etValid = 'g�ltig';
  etIsNot = 'ist kein(e)';
  etOutOfRange = 'Wert %s ist au�erhalb des Bereiches %s..%s';

  SApplyAll = 'Auf alle anwenden';

const //for DMDBAccessNavigator
  dbanOf = 'von';
  SNoDataToDisplay = '<Keine Daten>';


implementation

end.
