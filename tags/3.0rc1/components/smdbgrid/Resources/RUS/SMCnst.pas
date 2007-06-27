unit SMCnst;

interface

{Russian strings}
const
  strMessage = 'Печать...';
  strSaveChanges = 'Вы хотите сохранить внесенные изменения на сервере?';
  strErrSaveChanges = 'Данные не удалось сохранить! Проверьте соединение с сервером и допустимость введенных данных.';
  strDeleteWarning = 'Вы действительно хотите удалить таблицу %s?';
  strEmptyWarning = 'Вы действительно хотите удалить данные из таблицы %s?';

const PopUpCaption: array [0..22] of string[33] =
   ('Добавить запись',
    'Вставить запись',
    'Редактировать запись',
    'Удалить запись',
    '-',
    'Печать ...',
    'Экспорт данных ...',
    '-',
    'Сохранить изменения',
    'Отменить изменения',
    'Обновить данные',
    '-',
    'Пометка/Разотметка записей',
       'Пометить запись',
       'Пометить все записи',
       '-',
       'Разотметить запись',
       'Разотметить все записи',
    '-',
    'Сохранить расположение колонок',
    'Восстановить расположение колонок',
    '-',
    'Настройка...');


const //for TSMSetDBGridDialog
  SgbTitle = ' Заголовок ';
  SgbData = ' Данные ';
  STitleCaption = 'Название:';
  STitleAlignment = 'Выравнивание:';
  STitleColor = 'Цвет фона:'; 
  STitleFont = 'Шрифт:';
  SWidth = 'Ширина:';
  SWidthFix = 'символов';
  SAlignLeft = 'слева';
  SAlignRight = 'справа';
  SAlignCenter = 'по центру';

const //for TSMDBFilterDialog
  strEqual = 'равно';
  strNonEqual = 'не равно';
  strNonMore = 'не более';
  strNonLess = 'не менее';
  strLessThan = 'меньше чем';
  strLargeThan = 'больше чем';
  strExist = 'пусто';
  strNonExist = 'не пусто';
  strIn = 'в списке';
  strBetween = 'между';

  strOR = 'ИЛИ';
  strAND = 'И';

  strField = 'Поле';
  strCondition = 'Условие';
  strValue = 'Значение';

  strAddCondition = ' Определите дополнительное условие:';
  strSelection = ' Выбор записей по следующим условиям:';

  strAddToList = 'Добавить в список';
  strDeleteFromList = 'Удалить из списка';

  strTemplate = 'Шаблоны фильтров';
  strFLoadFrom = 'Загрузить из...';
  strFSaveAs = 'Сохранить как..';
  strFDescription = 'Описание';
  strFFileName = 'Имя файла';
  strFCreate = 'Создан: %s';
  strFModify = 'Модифицирован: %s';
  strFProtect = 'Защита от перезаписи';
  strFProtectErr = 'Файл защищен от записи!';

const //for SMDBNavigator
  SFirstRecord = 'Первая запись';
  SPriorRecord = 'Предыдущая запись';
  SNextRecord = 'Следующая запись';
  SLastRecord = 'Последняя запись';
  SInsertRecord = 'Вставка записи';
  SCopyRecord = 'Копирование записи';
  SDeleteRecord = 'Удаление записи';
  SEditRecord = 'Редактирование записи';
  SFilterRecord = 'Выборка записей';
  SFindRecord = 'Поиск записи';
  SPrintRecord = 'Печать записей';
  SExportRecord = 'Экспорт записей';
  SPostEdit = 'Сохранение изменений';
  SCancelEdit = 'Отмена изменений';
  SRefreshRecord = 'Обновление данных';
  SChoice = 'Выбор данных';
  SClear = 'Очистка данных';
  SDeleteRecordQuestion = 'Удалить запись?';
  SDeleteMultipleRecordsQuestion = 'Удалить все выбранные записи?';
  SRecordNotFound = 'Запись не найдена';

  SFirstName = 'Начало';
  SPriorName = 'Пред.';
  SNextName = 'След.';
  SLastName = 'Конец';
  SInsertName = 'Вставка';
  SCopyName = 'Копирование';
  SDeleteName = 'Удаление';
  SEditName = 'Редактирование';
  SFilterName = 'Фильтр';
  SFindName = 'Поиск';
  SPrintName = 'Печать';
  SExportName = 'Экспорт';
  SPostName = 'Сохранение';
  SCancelName = 'Отмена';
  SRefreshName = 'Обновление';
  SChoiceName = 'Выбор';
  SClearName = 'Сброс';

  SBtnOk = '&OK';
  SBtnCancel = 'О&тмена';
  SBtnLoad = 'Загрузить';
  SBtnSave = 'Сохранить';
  SBtnCopy = 'Копировать';
  SBtnPaste = 'Вставить';
  SBtnClear = 'Очистить';

  SRecNo = '№';
  SRecOf = ' из ';

const //for EditTyped
  etValidNumber = 'корректным числом';
  etValidInteger = 'корректным целым числом';
  etValidDateTime = 'корректной датой и временем';
  etValidDate = 'корректной датой';
  etValidTime = 'корректным временем';
  etValid = 'корректным';
  etIsNot = 'не является';
  etOutOfRange = 'Значение %s не принадлежит диапазону %s..%s';


implementation

end.
