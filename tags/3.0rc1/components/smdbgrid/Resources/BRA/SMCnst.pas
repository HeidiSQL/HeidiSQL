unit SMCnst;

interface


{Brazilian Portuguese strings}
{translated by Rodrigo Hjort, rodrigo_hjort@excite.com}
const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Deseja realmente salvar alterações no Servidor de Banco de Dados?';
  strErrSaveChanges = 'Não foi possível salvar um dado! Verifique a conexão com o Servidor ou validação de dados.';
  strDeleteWarning = 'Deseja realmente excluir a tabela %s?';
  strEmptyWarning = 'Deseja realmente esvaziar a tabela %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Incluir registro',
    'Insert registro',
    'Alterar registro',
    'Excluir registro',
    '-',
    'Imprimir ...',
    'Exportar ...',
    '-',
    'Salvar alterações',
    'Cancelar alterações',
    'Atualizar',
    '-',
    'Selecionar/Desselecionar registros',
       'Selecionar registro',
       'Selecionar todos registros',
       '-',
       'Desselecionar registro',
       'Desselecionar todos registros',
    '-',
    'Salvar layout da coluna',
    'Abrir layout da coluna',
    '-',
    'Configurar...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Title ';
  SgbData = ' Data ';
  STitleCaption = 'Caption:';
  STitleAlignment = 'Alignment:';
  STitleColor = 'Color:'; 
  STitleFont = 'Font:';
  SWidth = 'Width:';
  SWidthFix = 'characters';
  SAlignLeft = 'left';
  SAlignRight = 'right';
  SAlignCenter = 'center';

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
  SDeleteRecordQuestion = 'Excluir registro?';
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
  SBtnLoad = 'Load';
  SBtnSave = 'Save';
  SBtnCopy = 'Copy';
  SBtnPaste = 'Paste';
  SBtnClear = 'Clear';

  SRecNo = '#';
  SRecOf = ' of ';

const //for EditTyped
  etValidNumber = 'número válido';
  etValidInteger = 'número inteiro válido';
  etValidDateTime = 'data/hora válida';
  etValidDate = 'data válida';
  etValidTime = 'hora válida';
  etValid = 'válido';
  etIsNot = 'não é um';
  etOutOfRange = 'Valor %s está fora dos limites %s..%s';


implementation

end.
