unit SMCnst;

interface


{Portuguese strings}
{translated by Augusto Campos, augcampos@augcampos.com}
const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Deseja mesmo guardar as altera��es na base de Dados ?';
  strErrSaveChanges = 'N�o � possivel guardar a informa��o ! Veja a liga��o ao servidor ou regras de valida��o.';
  strDeleteWarning = 'Deseja mesmo apagar a tabela  %s?';
  strEmptyWarning = 'Deseja mesmo limpar a tabela  %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Adicionar registo',
    'Inserir Registo',
    'Editar registo',
    'Apagar Registo',
    '-',
    'Imprimir ...',
    'Exportar ...',
    '-',
    'Guardar altera��es',
    'N�o Guardar altera��es',
    'Refrescar',
    '-',
    'selecionar/deselecionar registos',
       'Selecionar registo',
       'Selecionar todos os registos',
       '-',
       'Deselecionar registo',
       'deselecionar todos registos',
    '-',
    'salvar formato da coluna',
    'resturar formato da coluna',
    '-',
    'Config...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Titulo ';
  SgbData = ' Data ';
  STitleCaption = 'Titulo';
  STitleAlignment = 'Alinhamento :';
  STitleColor = 'Fundo:';
  STitleFont = 'Fonte:';
  SWidth = 'Largura:';
  SWidthFix = 'characteres';
  SAlignLeft = 'esquerda';
  SAlignRight = 'direita';
  SAlignCenter = 'centro';

const //for TSMDBFilterDialog
  strEqual = 'igual';
  strNonEqual = 'diferente';
  strNonMore = 'nao maior';
  strNonLess = 'nao menor';
  strLessThan = 'menor que';
  strLargeThan = 'maior que';
  strExist = 'vazio';
  // exist = existe
  strNonExist = 'nao vazio';
  // nonexit = Nao existe

  strIn = 'Na Lista';
  strBetween = 'entre';
 
  strOR = 'OU';
  strAND = 'E';
 
  strField = 'Campo';
  strCondition = 'Condi��o';
  strValue = 'Valor';

  strAddCondition = ' Define a condicao addicional:';
  strSelection = ' Selecione os Registos pela seguinte(s) Condi��es:';
 
  strAddToList = 'Adicionar a Lista';
  strEditInList = 'Editar da lista';
  strDeleteFromList = 'Apagar Da Lista';

  strTemplate = 'Filtrar por Dialogo';
  strFLoadFrom = 'Obter de...';
  strFSaveAs = 'Gravar Como..';
  strFDescription = 'Descri��o';
  strFFileName = 'Nome do Cheiro';
  strFCreate = 'Criado: %s';
  strFModify = 'Modificado: %s';
  strFProtect = 'Protegido Contra Escrita';
  strFProtectErr = 'Ficheiro Protegido!';

const //for SMDBNavigator
  SFirstRecord = 'primeiro registo';
  SPriorRecord = 'registo anterior';
  SNextRecord = 'proximo registo';
  SLastRecord = 'ultimo registo';
  SInsertRecord = 'Inserir registo';
  SCopyRecord = 'Copiar registo';
  SDeleteRecord = 'Apagar registo';
  SEditRecord = 'Editar registo';
  SFilterRecord = 'condi��es de filtro';
  SFindRecord = 'procurar registos';
  SPrintRecord = 'imprimir registos';
  SExportRecord = 'Exportar registos';
  SPostEdit = 'guardar altera��es';
  SCancelEdit = 'Cancelar altera��es';
  SRefreshRecord = 'Refrescar dados';
  SChoice = 'escolher um registo';
  SClear = 'limpar escolha de registo';
  SDeleteRecordQuestion = 'apagar registo?';
  SDeleteMultipleRecordsQuestion = 'deseja mesmo apagar os registos selecionados ?';
  SRecordNotFound = 'N�o existe dados ';

  SFirstName = 'primeiro';
  SPriorName = 'anterior';
  SNextName = 'proximo';
  SLastName = 'ultimo';
  SInsertName = 'Inserir';
  SCopyName = 'Copiar';
  SDeleteName = 'apagar';
  SEditName = 'Editar';
  SFilterName = 'Filtrar';
  SFindName = 'Localizar';
  SPrintName = 'imprimir';
  SExportName = 'Exportar';
  SPostName = 'guardar';
  SCancelName = 'Cancelar';
  SRefreshName = 'Refrescar';
  SChoiceName = 'escolher';
  SClearName = 'limpar';

  SBtnOk = '&OK';
  SBtnCancel = '&Cancelar';
  SBtnLoad = 'Carregar';
  SBtnSave = 'guardar';
  SBtnCopy = 'Copiar';
  SBtnPaste = 'colar';
  SBtnClear = 'limpar';

  SRecNo = 'reg.';
  SRecOf = ' de ';

const //for EditTyped
  etValidNumber = 'n�mero v�lido';
  etValidInteger = 'n�mero inteiro v�lido';
  etValidDateTime = 'data/hora v�lida';
  etValidDate = 'data v�lida';
  etValidTime = 'hora v�lida';
  etValid = 'v�lido';
  etIsNot = 'n�o � um';
  etOutOfRange = 'Valor %s est� fora dos limites %s..%s';


implementation

end.
