unit SMCnst;

interface


{Brazilian Portuguese strings}
{translated by Rodrigo Hjort, rodrigo_hjort@excite.com}
{**30/04/2005** update by Fábio H. Souza, fabio@cefise.com.br}

const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Deseja realmente salvar alterações no Servidor de Banco de Dados?';
  strErrSaveChanges = 'Não foi possível salvar um dado! Verifique a conexão com o Servidor ou validação de dados.';
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
    'Salvar alterações',
    'Cancelar alterações',
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
  SgbTitle = ' Título ';
  SgbData = ' Dado ';
  STitleCaption = 'Título:';
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
  strNonEqual = 'não igual';
  strNonMore = 'não maior';
  strNonLess = 'não menor';
  strLessThan = 'menor que';
  strLargeThan = 'maior que';
  strExist = 'vazio';
  strNonExist = 'não vazio';
  strIn = 'na lista';
  strBetween = 'entre';
  strLike = 'como';

  strOR = 'OU';
  strAND = 'E';

  strField = 'Campo';
  strCondition = 'Condição';
  strValue = 'Valor';

  strAddCondition = ' Define a condição adicional:';
  strSelection = ' Selecione os Registros pela seguintes condições:';

  strAddToList = 'Adicionar a lista';
  strEditInList = 'Editar na lista';
  strDeleteFromList = 'Deletar da lista';

  strTemplate = 'Dialogo de modelo de Filtro';
  strFLoadFrom = 'Abrir de...';
  strFSaveAs = 'Salvar como..';
  strFDescription = 'Descrição';
  strFFileName = 'Nome do Arquivo';
  strFCreate = 'Criado: %s';
  strFModify = 'Modificado: %s';
  strFProtect = 'Protegido contra gravação';
  strFProtectErr = 'Arquivo protegido!';


const //for SMDBNavigator
  SFirstRecord = 'Primeiro registro';
  SPriorRecord = 'Registro anterior';
  SNextRecord = 'Próximo registro';
  SLastRecord = 'Último registro';
  SInsertRecord = 'Inserir registro';
  SCopyRecord = 'Copiar registro';
  SDeleteRecord = 'Excluir registro';
  SEditRecord = 'Alterar registro';
  SFilterRecord = 'Condições de filtragem';
  SFindRecord = 'Localizar registro';
  SPrintRecord = 'Imprimir registros';
  SExportRecord = 'Exportar registros';
  SPostEdit = 'Salvar alterações';
  SCancelEdit = 'Cancelar alterações';
  SRefreshRecord = 'Atualizar dados';
  SChoice = 'Escolher registro';
  SClear = 'Limpar escolha de registro';
  SDeleteRecordQuestion = 'Excluir registro selecionado?';
  SDeleteMultipleRecordsQuestion = 'Deseja realmente excluir registros selecionados?';
  SRecordNotFound = 'Registro não encontrado';

  SFirstName = 'Primeiro';
  SPriorName = 'Anterior';
  SNextName = 'Próximo';
  SLastName = 'Último';
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
  etValidNumber = 'número válido';
  etValidInteger = 'número inteiro válido';
  etValidDateTime = 'data/hora válida';
  etValidDate = 'data válida';
  etValidTime = 'hora válida';
  etValid = 'válido';
  etIsNot = 'não é um';
  etOutOfRange = 'Valor %s está fora dos limites %s..%s';

  SApplyAll = 'Aplicar a Todos';
implementation

end.
