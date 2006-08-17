unit SMCnst;

interface

{French strings}
{translated by Remy, walloon@euronet.be}
const
  strMessage = 'Imprimer...';
  strSaveChanges = 'Voulez-vous enregistrer les modifications sur le serveur de données?';
  strErrSaveChanges = 'Sauvegarde impossible! Vérifiez la connection au serveur ou les règles de validation.';
  strDeleteWarning = 'Voulez-vous effacer le fichier %s?';
  strEmptyWarning = 'Voulez-vous vider la table %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Ajouter fiche',
    'Ajouter une fiche',
    'Modifier fiche',
    'Effacer fiche',
    '-',
    'Imprimer ...',
    'Exporter ...',
    '-',
    'Sauver les changements',
    'Annuler les changements',
    'Rafraîchir les données',
    '-',
    'Sélect./Désélect. fiche',
       'Sélect. fiche actuelle',
       'Sélect. toutes les fiches',
       '-',
       'Désélect. fiche actuelle',
       'Désélect. toutes les fiches',
    '-',
    'Sauve layout des col.',
    'Récharge layout des col.',
    '-',
    'Configuration...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Titre ';
  SgbData = ' Donnee ';
  STitleCaption = 'Libelle:';
  STitleAlignment = 'Alignement:';
  STitleColor = 'Fond:';
  STitleFont = 'Police:';
  SWidth = 'Largeur:';
  SWidthFix = 'caracteres';
  SAlignLeft = 'a gauche';
  SAlignRight = 'a droite';
  SAlignCenter = 'au centre';

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
  SFirstRecord = 'Première fiche';
  SPriorRecord = 'Fiche précédente';
  SNextRecord = 'Fiche suivante';
  SLastRecord = 'Dernière fiche';
  SInsertRecord = 'Ajouter une fiche';
  SCopyRecord = 'Copier la fiche';
  SDeleteRecord = 'Effacer la fiche';
  SEditRecord = 'Editer la fiche';
  SFilterRecord = 'Conditions de filtre';
  SFindRecord = 'Chercher une fiche';
  SPrintRecord = 'Impression des fiches';
  SExportRecord = 'Exportation des fiches';
  SPostEdit = 'Sauver les changements';
  SCancelEdit = 'Annuler les changements';
  SRefreshRecord = 'Rafraîchir les données';
  SChoice = 'Choisir une fiche';
  SClear = 'Effacer la sélection des fiches';
  SDeleteRecordQuestion = 'Effacer une fiche?';
  SDeleteMultipleRecordsQuestion = 'Voulez-vous effacer les fiches choisies?';
  SRecordNotFound = 'Fiche introuvable';

  SFirstName =  'Premier';
  SPriorName =  'Précédent';
  SNextName =   'Suivant';
  SLastName =   'Dernier';
  SInsertName = 'Ajouter';
  SCopyName =   'Copier';
  SDeleteName = 'Détruire';
  SEditName =   'Editer';
  SFilterName = 'Filtrer';
  SFindName =   'Touver';
  SPrintName =  'Imprimer';
  SExportName = 'Exporter';
  SPostName =   'Sauver';
  SCancelName = 'Annuler';
  SRefreshName= 'Rafraîchir';
  SChoiceName = 'Choisir';
  SClearName =  'Effacer';

  SBtnOk = '&OK';
  SBtnCancel = '&Annuler';
  SBtnLoad = 'Charger';
  SBtnSave = 'Sauver';
  SBtnCopy = 'Copier';
  SBtnPaste = 'Coller';
  SBtnClear = 'Effacer';

  SRecNo = 'enreg.';
  SRecOf = ' de ';

const //for EditTyped
  etValidNumber = 'nombre valable';
  etValidInteger = 'nombre entier valable';
  etValidDateTime = 'date/heure valable';
  etValidDate = 'date valable';
  etValidTime = 'heure valable';
  etValid = 'valable';
  etIsNot = 'n''est pas un';
  etOutOfRange = 'Valeur %s n''est pas dans les bornes %s..%s';


implementation

end.
