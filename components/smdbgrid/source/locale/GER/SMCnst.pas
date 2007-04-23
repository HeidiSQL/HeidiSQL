unit SMCnst;

interface

{German strings}
{translated by Thomas Grimm, tgrimm@allegro-itc.de}
{changed by Reinhold Bauer, reinhold.bauer@onlinehome.de, 30.06.2006}
const
  strMessage = 'Drucke...';
  strSaveChanges = 'Sollen die Änderungen auf dem Server gespeichert werden?';
  strErrSaveChanges = 'Kann die Daten nicht speichern! Prüfen Sie die Serververbindung oder die Gültigkeit der Daten';
  strDeleteWarning = 'Soll gelöscht werden, Tabelle %s?';
  strEmptyWarning = 'Soll geleert werden, Tabelle %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Datensatz anfügen',
    'Datensatz einfügen',
    'Datensatz bearbeiten',
    'Datensatz löschen',
    '-',
    'Drucke ...',
    'Exportiere ...',
    'Filter ...',
    'Suche ...',
    '-',
    'Speichere Änderungen',
    'Verwerfe Änderungen',
    'Aktualisiere',
    '-',
    'Datensatz auswählen/abwählen',
       'Datensatz auswählen',
       'Alle Datensätze auswählen',
       '-',
       'Datensatz abwählen',
       'Alle Datensätze abwählen',
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
  strNonMore = 'nicht größer';
  strNonLess = 'nicht kleiner';
  strLessThan = 'kleiner als';
  strLargeThan = 'größer als';
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
  strSelection = ' Auswahl der Datensätze gemäß der folgenden Bedingungen:';

  strAddToList = 'Hinzufügen';
  strEditInList = 'Editieren';
  strDeleteFromList = 'Löschen';

  strTemplate = 'Filter-Dialog';
  strFLoadFrom = 'Öffnen...';
  strFSaveAs = 'Speichern unter...';
  strFDescription = 'Beschreibung';
  strFFileName = 'Dateiname';
  strFCreate = 'Erstellt: %s';
  strFModify = 'Geändert: %s';
  strFProtect = 'Schreibgeschützt';
  strFProtectErr = 'Datei ist schreibgeschützt';

const //for SMDBNavigator
  SFirstRecord = 'Erster Datensatz';
  SPriorRecord = 'Vorheriger Datensatz';
  SNextRecord = 'Nächster Datensatz';
  SLastRecord = 'Letzter Datensatz';
  SInsertRecord = 'Datensatz einfügen';
  SCopyRecord = 'Datensatz kopieren';
  SDeleteRecord = 'Datensatz löschen';
  SEditRecord = 'Datensatz bearbeiten';
  SFilterRecord = 'Filterbedingungen';
  SFindRecord = 'Gefundene Datensätze';
  SPrintRecord = 'Gedruckte Datensätze';
  SExportRecord = 'Exportierte Datensätze';
  SImportRecord = 'Importiere Datensätze'; 
  SPostEdit = 'Speichere Änderungen';
  SCancelEdit = 'Verwerfe Änderungen';
  SRefreshRecord = 'Aktualisiere Daten';
  SChoice = 'Datensatz auswählen';
  SClear = 'Auswahl löschen';
  SDeleteRecordQuestion = 'Datensatz löschen?';
  SDeleteMultipleRecordsQuestion = 'Sollen die ausgewählten Datensätze gelöscht werden?';
  SRecordNotFound = 'Datensatz nicht gefunden';

  SFirstName = 'Erste';
  SPriorName = 'Vorherige';
  SNextName = 'Nächste';
  SLastName = 'Letzte';
  SInsertName = 'Einfügen';
  SCopyName = 'Kopieren';
  SDeleteName = 'Löschen';
  SEditName = 'Bearbeiten';
  SFilterName = 'Filter';
  SFindName = 'Suche';
  SPrintName = 'Drucken';
  SExportName = 'Exportieren';
  SImportName = 'Importieren';
  SPostName = 'Speichern';
  SCancelName = 'Abbrechen';
  SRefreshName = 'Aktualisieren';
  SChoiceName = 'Auswählen';
  SClearName = 'Löschen';

  SBtnOk = '&OK';
  SBtnCancel = '&Abbrechen';
  SBtnLoad = 'Öffnen';
  SBtnSave = 'Speichern';
  SBtnCopy = 'Kopieren';
  SBtnPaste = 'Einfügen';
  SBtnClear = 'Entfernen';

  SRecNo = '#';
  SRecOf = ' von ';

const //for EditTyped
  etValidNumber = 'gültige Zahl';
  etValidInteger = 'gültige Ganzzahl';
  etValidDateTime = 'gültiges Datum/Zeit';
  etValidDate = 'gültiges Datum';
  etValidTime = 'gültige Zeit';
  etValid = 'gültig';
  etIsNot = 'ist kein(e)';
  etOutOfRange = 'Wert %s ist außerhalb des Bereiches %s..%s';

  SApplyAll = 'Auf alle anwenden';

const //for DMDBAccessNavigator
  dbanOf = 'von';
  SNoDataToDisplay = '<Keine Daten>';


implementation

end.
