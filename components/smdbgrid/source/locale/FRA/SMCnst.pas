unit SMCnst;

interface

{French strings}
{translated by Remy, walloon@euronet.be
 additional translation made by Martin Ledoux, martinlmtl@hotmail.com
 and Daniel Lepage, daniel_lepage@hotmail.com) }
const
  strMessage = 'Imprimer...';
  strSaveChanges = 'Voulez-vous enregistrer les modifications sur le serveur de données?';
  strErrSaveChanges = 'Sauvegarde impossible! Vérifiez la connection au serveur ou les règles de validation.';
  strDeleteWarning = 'Voulez-vous effacer le fichier %s?';
  strEmptyWarning = 'Voulez-vous vider la table %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Ajouter fiche',
    'Ajouter une fiche',
    'Modifier fiche',
    'Effacer fiche',
    '-',
    'Imprimer ...',
    'Exporter ...',
    'Filtrer ...',
    'Rechercher ...',
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
  strEqual = 'égal';
  strNonEqual = 'n''est pas égal';
  strNonMore = 'pas plus grand';
  strNonLess = 'pas plus petit';
  strLessThan = 'plus petit que';
  strLargeThan = 'plus grand que';
  strExist = 'vide';
  strNonExist = 'pas vide';
  strIn = 'dans la liste';
  strBetween = 'entre';
  strLike = 'comme';

  strOR = 'OU';
  strAND = 'ET';

  strField = 'Champ';
  strCondition = 'Condition';
  strValue = 'Valeur';

  strAddCondition = ' Définir une condition additionnelle:';
  strSelection = ' Choisir les enregistrements par les conditions suivantes:';

  strAddToList = 'Ajouter à la liste';
  strEditInList = 'Modifier dans la liste';
  strDeleteFromList = 'Effacer de la liste';

  strTemplate = 'Dialogue de modèle de Filtre';
  strFLoadFrom = 'Ouvrir...';
  strFSaveAs = 'Enregistrer sous..';
  strFDescription = 'Description';
  strFFileName = 'Nom de fichier';
  strFCreate = 'Créé: %s';
  strFModify = 'Modifié: %s';
  strFProtect = 'Protégé en écriture';
  strFProtectErr = 'Fichier protégé!';

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

  SApplyAll = 'Appliquez à tous';
  
implementation

end.
