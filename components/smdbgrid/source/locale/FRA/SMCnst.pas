unit SMCnst;

interface

{French strings}
{translated by Remy, walloon@euronet.be
 additional translation made by Martin Ledoux, martinlmtl@hotmail.com
 and Daniel Lepage, daniel_lepage@hotmail.com) }
const
  strMessage = 'Imprimer...';
  strSaveChanges = 'Voulez-vous enregistrer les modifications sur le serveur de donn�es?';
  strErrSaveChanges = 'Sauvegarde impossible! V�rifiez la connection au serveur ou les r�gles de validation.';
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
    'Rafra�chir les donn�es',
    '-',
    'S�lect./D�s�lect. fiche',
       'S�lect. fiche actuelle',
       'S�lect. toutes les fiches',
       '-',
       'D�s�lect. fiche actuelle',
       'D�s�lect. toutes les fiches',
    '-',
    'Sauve layout des col.',
    'R�charge layout des col.',
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
  strEqual = '�gal';
  strNonEqual = 'n''est pas �gal';
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

  strAddCondition = ' D�finir une condition additionnelle:';
  strSelection = ' Choisir les enregistrements par les conditions suivantes:';

  strAddToList = 'Ajouter � la liste';
  strEditInList = 'Modifier dans la liste';
  strDeleteFromList = 'Effacer de la liste';

  strTemplate = 'Dialogue de mod�le de Filtre';
  strFLoadFrom = 'Ouvrir...';
  strFSaveAs = 'Enregistrer sous..';
  strFDescription = 'Description';
  strFFileName = 'Nom de fichier';
  strFCreate = 'Cr��: %s';
  strFModify = 'Modifi�: %s';
  strFProtect = 'Prot�g� en �criture';
  strFProtectErr = 'Fichier prot�g�!';

const //for SMDBNavigator
  SFirstRecord = 'Premi�re fiche';
  SPriorRecord = 'Fiche pr�c�dente';
  SNextRecord = 'Fiche suivante';
  SLastRecord = 'Derni�re fiche';
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
  SRefreshRecord = 'Rafra�chir les donn�es';
  SChoice = 'Choisir une fiche';
  SClear = 'Effacer la s�lection des fiches';
  SDeleteRecordQuestion = 'Effacer une fiche?';
  SDeleteMultipleRecordsQuestion = 'Voulez-vous effacer les fiches choisies?';
  SRecordNotFound = 'Fiche introuvable';

  SFirstName =  'Premier';
  SPriorName =  'Pr�c�dent';
  SNextName =   'Suivant';
  SLastName =   'Dernier';
  SInsertName = 'Ajouter';
  SCopyName =   'Copier';
  SDeleteName = 'D�truire';
  SEditName =   'Editer';
  SFilterName = 'Filtrer';
  SFindName =   'Touver';
  SPrintName =  'Imprimer';
  SExportName = 'Exporter';
  SPostName =   'Sauver';
  SCancelName = 'Annuler';
  SRefreshName= 'Rafra�chir';
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

  SApplyAll = 'Appliquez � tous';
  
implementation

end.
