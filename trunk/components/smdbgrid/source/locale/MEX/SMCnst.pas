unit SMCnst;

interface

{   Language:
       Spanish (Mexican) strings
    Translator:
       Daniel Ramirez Jaime
    E-Mail:
       rdaniel2000@hotmail.com
}

const
  strMessage = 'Imprimir...';
  strSaveChanges = '¿En readlidad desea guardar los cambios a la Base de Datos?';
  strErrSaveChanges = 'No se puede guardar los datos, verifique la conección'+
                      ' con el Servidor o la validación de datos';
  strDeleteWarning = '¿En readlidad desea eliminar de la tabla %s?';
  strEmptyWarning = '¿En realidad desea vaciar la Tabla %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Agregar registro',
    'Insertar registro',
    'Modificar registro',
    'Eliminar registro',
    '-',
    'Imprimir ...',
    'Exportar ...',
    '-',
    'Guardar cambios',
    'Cancelar cambios',
    'Actualizar',
    '-',
    'Seleccionar/Deseleccionar registros',
       'Selecccionar registros',
       'Seleccionar Todos los registros',
       '-',
       'Deseleccionar registro',
       'Deseleccionar Todos los registro',
    '-',
    'Guardar el esquema de la columna',
    'Restaurar el esquema de la columna',
    '-',
    'Configurar...');

const //for TSMSetDBGridDialog
  SgbTitle = ' TŒtulo ';
  SgbData = ' Dato ';
  STitleCaption = 'Descripci‘n:';
  STitleAlignment = 'Alinear:';
  STitleColor = 'Fondo:';
  STitleFont = 'Fuente:';
  SWidth = 'Ancho:';
  SWidthFix = 'caracteres';
  SAlignLeft = 'izquierdo';
  SAlignRight = 'derecho';
  SAlignCenter = 'centro';

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
  SFirstRecord = 'Primer registro';
  SPriorRecord = 'Anterior  registro';
  SNextRecord = 'Siguiente registro';
  SLastRecord = 'Ultimo registro';
  SInsertRecord = 'Insertar registro';
  SCopyRecord = 'Copiar registro';
  SDeleteRecord = 'Eliminar registro';
  SEditRecord = 'Modificar registro';
  SFilterRecord = 'Filtrar condiciones';
  SFindRecord = 'Buscar el registro';
  SPrintRecord = 'Imprimir los registros';
  SExportRecord = 'Exportar los registros';
  SPostEdit = 'Guardar cambios';
  SCancelEdit = 'Cancelar cambios';
  SRefreshRecord = 'Actualiar datos';
  SChoice = 'Escojer registro';
  SClear = 'Borrar el registro selecionado';
  SDeleteRecordQuestion = '¿Eliminar registro?';
  SDeleteMultipleRecordsQuestion = '¿En realidad desea Eliminar los registros seleccionados?';
  SRecordNotFound = 'Registro no encontrado';

  SFirstName = 'Primer';
  SPriorName = 'Anterior';
  SNextName = 'Siguiente';
  SLastName = 'Ultimo';
  SInsertName = 'Insertar';
  SCopyName = 'Copiar';
  SDeleteName = 'Eliminar';
  SEditName = 'Modificar';
  SFilterName = 'Filtrar';
  SFindName = 'Encontrar';
  SPrintName = 'Imprimir';
  SExportName = 'Exportar';
  SPostName = 'Guardar';
  SCancelName = 'Cancelar';
  SRefreshName = 'Actualizar';
  SChoiceName = 'Escojer';
  SClearName = 'Borrar';

  SBtnOk = '&Aceptar';
  SBtnCancel = '&Cancelar';
  SBtnLoad = 'Abrir';
  SBtnSave = 'Guardar';
  SBtnCopy = 'Copiar';
  SBtnPaste = 'Pegar';
  SBtnClear = 'Limpiar';

  SRecNo = 'reg.';
  SRecOf = ' de ';

const //for EditTyped
  etValidNumber = 'número válido';
  etValidInteger = 'número entero válido';
  etValidDateTime = 'fecha/hora válido';
  etValidDate = 'fecha válido';
  etValidTime = 'hora válido';
  etValid = 'válido';
  etIsNot = 'no es';
  etOutOfRange = 'Valor %s fuera de rango %s..%s';

implementation

end.
