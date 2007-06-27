unit SMCnst;

interface

{Spanish strings}
const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Confirma la grabación de datos en el Servidor';
  strErrSaveChanges = 'Imposible grabar! Chequea la conexión con el servidor o valida los datos.';
  strDeleteWarning = 'Confirma el borrado de la tabla %s?';
  strEmptyWarning = 'Confirma el vaciado de la table %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Agregar registro',
    'Insertar registro',
    'Editar registro',
    'Eliminar registro',
    '-',
    'Imprimir ...',
    'Exportar ...',
    '-',
    'Grabar cambios',
    'Descartar cambios',
    'Refrescar',
    '-',
    'Selecciona/Deselecciona registros',
       'Seleccionar registro',
       'Selecciona todos los registros',
       '-',
       'Deselecciona registro',
       'Deselecciona todos los reg.',
    '-',
    'Guarda Formato de columna',
    'Recupera formato de columna',
    '-',
    'Configurar...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Título ';
  SgbData = ' Datos ';
  STitleCaption = 'Título:';
  STitleAlignment = 'Alineación:';
  STitleColor = 'Fondo:'; 
  STitleFont = 'Fuente:';
  SWidth = 'Ancho:';
  SWidthFix = 'caracteres';
  SAlignLeft = 'izquierda';
  SAlignRight = 'derecha';
  SAlignCenter = 'centro';
  
const //for TSMDBFilterDialog
  strEqual = 'igual';
  strNonEqual = 'diferente';
  strNonMore = 'menor o igual';
  strNonLess = 'mayor o igual';
  strLessThan = 'menor';
  strLargeThan = 'mayor';
  strExist = 'vacio';
  strNonExist = 'lleno';
  strIn = 'en la lista';
  strBetween = 'entre';

  strOR = 'OR';
  strAND = 'AND';

  strField = 'Campo';
  strCondition = 'Condición';
  strValue = 'Valor';

  strAddCondition = ' Define condición adicional:';
  strSelection = ' Selecciona los registros por las siguientes condiciones:';

  strAddToList = 'Agrega a la lista';
  strDeleteFromList = 'Borra de la lista';

  strTemplate = 'Dialogo plantilla de filtro';
  strFLoadFrom = 'Cargar desde...';
  strFSaveAs = 'Guardar como..';
  strFDescription = 'Descripción';
  strFFileName = 'Archivo';
  strFCreate = 'Creado: %s';
  strFModify = 'Modificado: %s';
  strFProtect = 'Protege para escritura';
  strFProtectErr = 'Archivo protegido!';

const //for SMDBNavigator
  SFirstRecord = 'Primero';
  SPriorRecord = 'Anterior';
  SNextRecord = 'Siguiente';
  SLastRecord = 'Ultimo';
  SInsertRecord = 'Nuevo';
  SCopyRecord = 'Copiar';
  SDeleteRecord = 'Eliminar';
  SEditRecord = 'Modificar';
  SFilterRecord = 'Filtrar';
  SFindRecord = 'Buscar';
  SPrintRecord = 'Impresión';
  SExportRecord = 'Exportar';
  SPostEdit = 'Guardar';
  SCancelEdit = 'Deshacer Cambios';
  SRefreshRecord = 'Refrescar Datos';
  SChoice = 'Elegir registro';
  SClear = 'Vaciar registro elegido';
  SDeleteRecordQuestion = 'Eliminar registro?';
  SDeleteMultipleRecordsQuestion = 'Realmente deseas eliminar los registro seleccionados?';
  SRecordNotFound = 'Registro no encontrado';

  SFirstName = 'Primero';
  SPriorName = 'Anterior';
  SNextName = 'Siguiente';
  SLastName = 'Ultimo';
  SInsertName = 'Nuevo';
  SCopyName = 'Copiar';
  SDeleteName = 'Eliminar';
  SEditName = 'Modificar';
  SFilterName = 'Filtrar';
  SFindName = 'Buscar';
  SPrintName = 'Imprimir';
  SExportName = 'Exportar';
  SPostName = 'Guardar';
  SCancelName = 'Cancelar';
  SRefreshName = 'Refrescar';
  SChoiceName = 'Elegir';
  SClearName = 'Vaciar';

  SBtnOk = '&Aceptar';
  SBtnCancel = '&Cancelar';
  SBtnLoad = 'Cargar';
  SBtnSave = 'Guardar';
  SBtnCopy = 'Copiar';
  SBtnPaste = 'Pegar';
  SBtnClear = 'Vaciar';

  SRecNo = 'reg.';
  SRecOf = ' de ';

const //for EditTyped
  etValidNumber = 'número correcto';
  etValidInteger = 'entero correcto';
  etValidDateTime = 'fecha/hora correcto ';
  etValidDate = 'fecha correcta';
  etValidTime = 'hora correcta';
  etValid = 'correcto';
  etIsNot = 'no es un/a';
  etOutOfRange = 'Valor %s fuera del rango %s..%s';

implementation

end.
