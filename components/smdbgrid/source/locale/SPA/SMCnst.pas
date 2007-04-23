{ Copyright (C) 1998-2006, written by Mike Shkolnik, Scalabium Software
  E-Mail: mshkolnik@scalabium
  WEB: http://www.scalabium.com

  Const strings for localization
  freeware SMComponent library
}
unit SMCnst;

interface

{Spanish strings}
const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Confirma la grabaci�n de datos en el Servidor';
  strErrSaveChanges = 'Imposible grabar! Chequea la conexi�n con el servidor o valida los datos.';
  strDeleteWarning = 'Confirma el borrado de la tabla %s?';
  strEmptyWarning = 'Confirma el vaciado de la table %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Agregar registro',
    'Insertar registro',
    'Editar registro',
    'Eliminar registro',
    '-',
    'Imprimir ...',
    'Exportar ...',
    'Filtrar...',
    'Buscar...',
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
  SgbTitle = ' T�tulo ';
  SgbData = ' Datos ';
  STitleCaption = 'T�tulo:';
  STitleAlignment = 'Alineaci�n:';
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
  strLike = 'como';

  strOR = 'OR';
  strAND = 'AND';

  strField = 'Campo';
  strCondition = 'Condici�n';
  strValue = 'Valor';

  strAddCondition = ' Define condici�n adicional:';
  strSelection = ' Selecciona los registros por las siguientes condiciones:';

  strAddToList = 'Agregar a la lista';
  strEditInList = 'Editar en la lista';
  strDeleteFromList = 'Borrar de la lista';

  strTemplate = 'Dialogo plantilla de filtro';
  strFLoadFrom = 'Cargar desde...';
  strFSaveAs = 'Guardar como..';
  strFDescription = 'Descripci�n';
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
  SPrintRecord = 'Impresi�n';
  SExportRecord = 'Exportar los registros';
  SImportRecord = 'Importar los registros';
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
  SImportName = 'Importar';
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
  etValidNumber = 'n�mero correcto';
  etValidInteger = 'entero correcto';
  etValidDateTime = 'fecha/hora correcto ';
  etValidDate = 'fecha correcta';
  etValidTime = 'hora correcta';
  etValid = 'correcto';
  etIsNot = 'no es un/a';
  etOutOfRange = 'Valor %s fuera del rango %s..%s';

  SApplyAll = 'Aplicar a Todo';

  SNoDataToDisplay = '<No hay datos para mostrar>';

implementation

end.

