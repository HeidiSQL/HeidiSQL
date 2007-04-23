unit SMCnst;

interface


{Brazilian Portuguese strings}
{translated by Rodrigo Hjort, rodrigo_hjort@excite.com}
{**30/04/2005** update by F�bio H. Souza, fabio@cefise.com.br}

const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Deseja realmente salvar altera��es no Servidor de Banco de Dados?';
  strErrSaveChanges = 'N�o foi poss�vel salvar um dado! Verifique a conex�o com o Servidor ou valida��o de dados.';
  strDeleteWarning = 'Deseja realmente excluir a tabela %s?';
  strEmptyWarning = 'Deseja realmente esvaziar a tabela %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Incluir registro',
    'Inserir registro',
    'Alterar registro',
    'Excluir registro',
    '-',
    'Imprimir ...',
    'Exportar ...',
    'Filtrar ...',
    'Procurar ...',
    '-',
    'Salvar altera��es',
    'Cancelar altera��es',
    'Atualizar',
    '-',
    'Selecionar/Desselecionar registros',
       'Selecionar registro',
       'Selecionar todos os registros',
       '-',
       'Desselecionar registro',
       'Desselecionar todos os registros',
    '-',
    'Salvar layout da coluna',
    'Abrir layout da coluna',
    '-',
    'Configurar...');

const //for TSMSetDBGridDialog
  SgbTitle = ' T�tulo ';
  SgbData = ' Dado ';
  STitleCaption = 'T�tulo:';
  STitleAlignment = 'alinhamento:';
  STitleColor = 'Cor:';
  STitleFont = 'Fonte:';
  SWidth = 'Largura:';
  SWidthFix = 'caracteres';
  SAlignLeft = 'esquerda';
  SAlignRight = 'direita';
  SAlignCenter = 'centralizado';

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

  strAddCondition = ' Define a condi��o adicional:';
  strSelection = ' Selecione os Registros pela seguintes condi��es:';

  strAddToList = 'Adicionar a lista';
  strEditInList = 'Editar na lista';
  strDeleteFromList = 'Deletar da lista';

  strTemplate = 'Dialogo de modelo de Filtro';
  strFLoadFrom = 'Abrir de...';
  strFSaveAs = 'Salvar como..';
  strFDescription = 'Descri��o';
  strFFileName = 'Nome do Arquivo';
  strFCreate = 'Criado: %s';
  strFModify = 'Modificado: %s';
  strFProtect = 'Protegido contra grava��o';
  strFProtectErr = 'Arquivo protegido!';


const //for SMDBNavigator
  SFirstRecord = 'Primeiro registro';
  SPriorRecord = 'Registro anterior';
  SNextRecord = 'Pr�ximo registro';
  SLastRecord = '�ltimo registro';
  SInsertRecord = 'Inserir registro';
  SCopyRecord = 'Copiar registro';
  SDeleteRecord = 'Excluir registro';
  SEditRecord = 'Alterar registro';
  SFilterRecord = 'Condi��es de filtragem';
  SFindRecord = 'Localizar registro';
  SPrintRecord = 'Imprimir registros';
  SExportRecord = 'Exportar registros';
  SPostEdit = 'Salvar altera��es';
  SCancelEdit = 'Cancelar altera��es';
  SRefreshRecord = 'Atualizar dados';
  SChoice = 'Escolher registro';
  SClear = 'Limpar escolha de registro';
  SDeleteRecordQuestion = 'Excluir registro selecionado?';
  SDeleteMultipleRecordsQuestion = 'Deseja realmente excluir registros selecionados?';
  SRecordNotFound = 'Registro n�o encontrado';

  SFirstName = 'Primeiro';
  SPriorName = 'Anterior';
  SNextName = 'Pr�ximo';
  SLastName = '�ltimo';
  SInsertName = 'Inserir';
  SCopyName = 'Copiar';
  SDeleteName = 'Excluir';
  SEditName = 'Alterar';
  SFilterName = 'Filtrar';
  SFindName = 'Localizar';
  SPrintName = 'Imprimir';
  SExportName = 'Exportar';
  SPostName = 'Salvar';
  SCancelName = 'Cancelar';
  SRefreshName = 'Atualizar';
  SChoiceName = 'Escolher';
  SClearName = 'Limpar';

  SBtnOk = '&OK';
  SBtnCancel = '&Cancelar';
  SBtnLoad = 'Abrir';
  SBtnSave = 'Salvar';
  SBtnCopy = 'Copiar';
  SBtnPaste = 'Colar';
  SBtnClear = 'Limpar';

  SRecNo = '#';
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

  SApplyAll = 'Aplicar a Todos';
implementation

end.
