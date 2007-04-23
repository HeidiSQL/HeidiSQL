unit SMCnst;

interface
//
// Portuguese strings 
//   Translated by Fernando Dias  
//   e-mail: fernandodias@easygate.com.pt
//   Last Update: 3-Nov-2005 
//
const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Pretende mesmo guardar as altera��es na base de dados ?';
  strErrSaveChanges = 'N�o foi poss�vel guardar os dados! Verifique a liga��o ao servidor e a validade dos dados.';
  strDeleteWarning = 'Pretende mesmo eliminar a tabela %s?';
  strEmptyWarning = 'Pretende mesmo limpar o conteudo da tabela %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Acrescentar registo',
    'Inserir registo',
    'Editar registo',
    'Apagar registo',
    '-',
    'Imprimir ...',
    'Exportar ...',
    'Filtrar ...',
    'Procurar ...',
    '-',
    'Guardar altera��es',
    'Cancelar altera��es',
    'Refrescar',
    '-',
    'Marcar/Desmarcar registos',
       'Marcar registo',
       'Marcar todos',
       '-',
       'Desmarcar registo',
       'Desmarcar todos',
    '-',
    'Guardar configura��o da coluna',
    'Recuperar configura��o da coluna',
    '-',
    'Configura��o...');

const //for TSMSetDBGridDialog
  SgbTitle = ' T�tulo ';
  SgbData = ' Dados ';
  STitleCaption = 'R�tulo:';
  STitleAlignment = 'Alinhamento:';
  STitleColor = 'Fundo:'; 
  STitleFont = 'Letra:';
  SWidth = 'Largura:';
  SWidthFix = 'caracteres';
  SAlignLeft = 'esquerda';
  SAlignRight = 'direita';
  SAlignCenter = 'centro';
  
const //for TSMDBFilterDialog
  strEqual = 'igual';
  strNonEqual = 'n�o igual';
  strNonMore = 'n�o maior';
  strNonLess = 'n�o menor';
  strLessThan = 'menor que';
  strLargeThan = 'maior que';
  strExist = 'vazio';
  strNonExist = 'n�o vazio';
  strIn = 'na lista';
  strBetween = 'entre';
  strLike = 'como';

  strOR = 'OU';
  strAND = 'E';

  strField = 'Campo';
  strCondition = 'Condi��o';
  strValue = 'Valor';

  strAddCondition = ' Defina a condi��o adicional:';
  strSelection = ' Seleccionar registos pela seguinte condi��o:';

  strAddToList = 'Adicionar � lista';
  strEditInList = 'Editar a lista';
  strDeleteFromList = 'Apagar da lista';

  strTemplate = 'Modelo de filtro';
  strFLoadFrom = 'L�r de...';
  strFSaveAs = 'Guardar como...';
  strFDescription = 'Descri��o';
  strFFileName = 'Ficheiro';
  strFCreate = 'Criado: %s';
  strFModify = 'Modificado: %s';
  strFProtect = 'Proteger contra escrita';
  strFProtectErr = 'Ficheiro est� protegido!';

const //for SMDBNavigator
  SFirstRecord = 'Primeiro registo';
  SPriorRecord = 'Registo anterior';
  SNextRecord = 'Pr�ximo registo';
  SLastRecord = '�ltimo registo';
  SInsertRecord = 'Inserir registo';
  SCopyRecord = 'Copiar registo';
  SDeleteRecord = 'Apagar registo';
  SEditRecord = 'Editar registo';
  SFilterRecord = 'Filtrar';
  SFindRecord = 'Procurar';
  SPrintRecord = 'Imprimir';
  SExportRecord = 'Exportar';
  SPostEdit = 'Guardar altera��es';
  SCancelEdit = 'Cancelar altera��es';
  SRefreshRecord = 'Refrescar dados';
  SChoice = 'Escolher registo';
  SClear = 'Anular escolha de registo';
  SDeleteRecordQuestion = 'Apagar registo?';
  SDeleteMultipleRecordsQuestion = 'Pretende mesmo apagar os registos seleccionados?';
  SRecordNotFound = 'Registo n�o encontrado';

  SFirstName = 'Primeiro';
  SPriorName = 'Anterior';
  SNextName = 'Pr�ximo';
  SLastName = '�ltimo';
  SInsertName = 'Inserir';
  SCopyName = 'Copiar';
  SDeleteName = 'Apagar';
  SEditName = 'Editar';
  SFilterName = 'Filtrar';
  SFindName = 'Procurar';
  SPrintName = 'Imprimir';
  SExportName = 'Exportar';
  SPostName = 'Guardar';
  SCancelName = 'Cancelar';
  SRefreshName = 'Refrescar';
  SChoiceName = 'Escolher';
  SClearName = 'Limpar';

  SBtnOk = '&OK';
  SBtnCancel = '&Cancelar';
  SBtnLoad = 'Abrir';
  SBtnSave = 'Guardar';
  SBtnCopy = 'Copiar';
  SBtnPaste = 'Colar';
  SBtnClear = 'Limpar';

  SRecNo = 'reg.';
  SRecOf = ' de ';

const //for EditTyped
  etValidNumber = 'numero v�lido';
  etValidInteger = 'numero inteiro v�lido';
  etValidDateTime = 'data/hora v�lida';
  etValidDate = 'data v�lida';
  etValidTime = 'hora v�lida';
  etValid = 'v�lido';
  etIsNot = 'n�o � um(a)';
  etOutOfRange = 'Valor %s fora do intervalo %s..%s';

  SApplyAll = 'Aplicar a todos';
  
  SNoDataToDisplay = '<Sem dados para apresentar>';
  
implementation

end.
