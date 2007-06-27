unit SMCnst;

interface


{Portuguese strings}
{translated by Antonio Roque, antonio_roque@clix.pt}
const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Deseja mesmo guardar as alterações na base de Dados ?';
  strErrSaveChanges = 'Não é possivel guardar a informação ! Veja a ligação ao servidor ou regras de validação.';
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
    'Guardar alterações',
    'Não Guardar alterações',
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
    'Setup...');

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
  strNonExist = 'nao vazio';ÿ
  // nonexit = Nao existe

  strIn = 'Na Lista' 
  strBetween = 'entre';
 
  strOR = 'OU';
  strAND = 'E';
 
  strField = 'Campo';
  strCondition = 'Condicao';
  strValue = 'Valor';
 
  strAddCondition = ' Defineÿa condicao addicional:';
  strSelection = ' SelecioneÿosÿRegistos pela seguinte(s) Condicoes:';
 
  strAddToList = 'Adicionar a Listaÿ';
  strDeleteFromList = 'Apagar Da Lista';

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
  SFirstRecord = 'primeiro registo';
  SPriorRecord = 'registo anterior';
  SNextRecord = 'proximo registo';
  SLastRecord = 'ultimo registo';
  SInsertRecord = 'Inserir registo';
  SCopyRecord = 'Copiar registo';
  SDeleteRecord = 'Apagar registo';
  SEditRecord = 'Editar registo';
  SFilterRecord = 'condições de filtro';
  SFindRecord = 'procurar registos';
  SPrintRecord = 'imprimir registos';
  SExportRecord = 'Exportar registos';
  SPostEdit = 'guardar alterações';
  SCancelEdit = 'Cancelar alterações';
  SRefreshRecord = 'Refrescar dados';
  SChoice = 'escolher um registo';
  SClear = 'limpar escolha de registo';
  SDeleteRecordQuestion = 'apagar registo?';
  SDeleteMultipleRecordsQuestion = 'deseja mesmo apagar os registos selecionados ?';
  SRecordNotFound = 'Não existe dados ';

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
