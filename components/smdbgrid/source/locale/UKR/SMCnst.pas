unit SMCnst;

interface

{Ukrainian strings}
const
  strMessage = 'Друк...';
  strSaveChanges = 'Ви бажаете зберегти внесенi змiни на серверi?';
  strErrSaveChanges = 'Даннi не вдалося зберегти! Перевiрте з''єднання з сервером та коректнiсть внесених данних.';
  strDeleteWarning = 'Ви дiйсно бажаєте видалити таблицю %s?';
  strEmptyWarning = 'Ви дiйсно бажаєте видалити данi з таблицi %s?';

const PopUpCaption: array [0..22] of string[33] =
   ('Додати запис',
    'Вставити запис',
    'Редактувати запис',
    'Видалити запис',
    '-',
    'Друк ...',
    'Експорт данних ...',
    '-',
    'Зберегти змiни',
    'Вiдмiнити змiни',
    'Обновити даннi',
    '-',
    'Помiтка/Зняти відмутку з записiв',
       'Помiтити запис',
       'Помiтити усi записи',
       '-',
       'Зняти відмітку запис',
       'Зняти відмітку з усiх записів',
    '-',
    'Зберегти положення колонок',
    'Відновити положення колонок',
    '-',
    'Налаштування...');


const //for TSMSetDBGridDialog
  SgbTitle = ' Заголовок ';
  SgbData = ' Данi ';
  STitleCaption = 'Назва:';
  STitleAlignment = 'Вирiвнювання:';
  STitleColor = 'Колір фону:';
  STitleFont = 'Шрифт:';
  SWidth = 'Ширина:';
  SWidthFix = 'символiв';
  SAlignLeft = 'злiва';
  SAlignRight = 'зправа';
  SAlignCenter = 'по центру';

const //for TSMDBFilterDialog
  strEqual = 'рiвно';
  strNonEqual = 'не рiвно';
  strNonMore = 'не бiльше';
  strNonLess = 'не менше';
  strLessThan = 'меньше нiж';
  strLargeThan = 'бiльше нiж';
  strExist = 'пусте';
  strNonExist = 'не пусте';
  strIn = 'в списку';
  strBetween = 'мiж';

  strOR = 'АБО';
  strAND = 'ТА';

  strField = 'Поле';
  strCondition = 'Умова';
  strValue = 'Значення';

  strAddCondition = ' Введiть додаткову умову:';
  strSelection = ' Вибiр записiв згiдно наступних умов:';

  strAddToList = 'Додати до списку';
  strEditInList = 'Редактувати';
  strDeleteFromList = 'Видалити з списку';

  strTemplate = 'Шаблони фiльтрiв';
  strFLoadFrom = 'Загрузити з...';
  strFSaveAs = 'Зберегти як..';
  strFDescription = 'Опис';
  strFFileName = 'Ім''я файла';
  strFCreate = 'Створено: %s';
  strFModify = 'Модифiковано: %s';
  strFProtect = 'Захист вiд перезапису';
  strFProtectErr = 'Файл закритий вiд запису!';

const //for SMDBNavigator
  SFirstRecord = 'Перший запис';
  SPriorRecord = 'Попередній запис';
  SNextRecord = 'Наступний запис';
  SLastRecord = 'Останній запис';
  SInsertRecord = 'Вставка запису';
  SCopyRecord = 'Копіювання запису';
  SDeleteRecord = 'Видалення запису';
  SEditRecord = 'Редагування запису';
  SFilterRecord = 'Вiдбiр записiв';
  SFindRecord = 'Пошук запису';
  SPrintRecord = 'Друк записiв';
  SExportRecord = 'Експорт записiв';
  SPostEdit = 'Збереження змiн';
  SCancelEdit = 'Вiдмiна змiн';
  SRefreshRecord = 'Оновлення даних';
  SChoice = 'Вибiр даних';
  SClear = 'Очистити данi';
  SDeleteRecordQuestion = 'Видалити запис?';
  SDeleteMultipleRecordsQuestion = 'Видалити усi вибранi записи?';
  SRecordNotFound = 'Запис не знайдено';

  SFirstName = 'Початок';
  SPriorName = 'Попер.';
  SNextName = 'Наступ.';
  SLastName = 'Кiнець';
  SInsertName = 'Вставка';
  SCopyName = 'Копiювання';
  SDeleteName = 'Видалення';
  SEditName = 'Корегування';
  SFilterName = 'Фiльтр';
  SFindName = 'Пошук';
  SPrintName = 'Друк';
  SExportName = 'Експорт';
  SPostName = 'Збереження';
  SCancelName = 'Скасувати';
  SRefreshName = 'Оновлення';
  SChoiceName = 'Вибiр';
  SClearName = 'Зброс';

  SBtnOk = '&OK';
  SBtnCancel = '&Вiдмiна';
  SBtnLoad = 'Загрузити';
  SBtnSave = 'Зберегти';
  SBtnCopy = 'Копiювати';
  SBtnPaste = 'Вставити';
  SBtnClear = 'Очистити';

  SRecNo = '№';
  SRecOf = ' з ';

const //for EditTyped
  etValidNumber = 'коректним числом';
  etValidInteger = 'коректним цiлым числом';
  etValidDateTime = 'коректною датою та часом';
  etValidDate = 'коректною датою';
  etValidTime = 'коректним часом';
  etValid = 'коректним';
  etIsNot = 'не є';
  etOutOfRange = 'Значення %s не лежить в діапазонi %s..%s';

  SApplyAll = 'Применити усiм';

implementation

end.
