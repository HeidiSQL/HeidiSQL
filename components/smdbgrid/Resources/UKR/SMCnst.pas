unit SMCnst;

interface

{Ukrainian strings}
const
  strMessage = 'Друк...';
  strSaveChanges = 'Ви бажаете зберегти внесенi змiни на серверi?';
  strErrSaveChanges = 'Даннi не вдалося зберегти! Перевiрте з''еднання з сервером та коректнiсть внесених данних.';
  strDeleteWarning = 'Ви дiйсно бажаете видалити таблицю %s?';
  strEmptyWarning = 'Ви дiйсно бажаете видалити даннi з таблицi %s?';

const PopUpCaption: array [0..21] of string[33] =
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
    'Помiтка/Разотмiтка записiв',
       'Помiтити запис',
       'Помiтити усi записи',
       '-',
       'Разотмiтити запис',
       'Разотмiтити усi записи',
    '-',
    'Зберегти положення колонок',
    'Востановити положення колонок',
    '-',
    'Настройка...');


const //for TSMSetDBGridDialog
  SgbTitle = ' Заголовок ';
  SgbData = ' Данi ';
  STitleCaption = 'Назва:';
  STitleAlignment = 'Вирiвнювання:';
  STitleColor = 'Колор фону:'; 
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
  strExist = 'пустене';
  strNonExist = 'не пусте';
  strIn = 'в списке';
  strBetween = 'мiж';

  strOR = 'АБО';
  strAND = 'ТА';

  strField = 'Поле';
  strCondition = 'Умова';
  strValue = 'Значення';

  strAddCondition = ' Введiть додаткову умову:';
  strSelection = ' Вибiр записiв згiдно слiдуючих умов:';

  strAddToList = 'Додати до списку';
  strDeleteFromList = 'Видалити з списку';

  strTemplate = 'Шаблони фiльтрiв';
  strFLoadFrom = 'Загрузити з...';
  strFSaveAs = 'Зберегти як..';
  strFDescription = 'Описание';
  strFFileName = 'Им''я файла';
  strFCreate = 'Создан: %s';
  strFModify = 'Модифiкован: %s';
  strFProtect = 'Защита вiд перезапису';
  strFProtectErr = 'Файл закрит вiд запису!';

const //for SMDBNavigator
  SFirstRecord = 'Перша запис';
  SPriorRecord = 'Попередня запис';
  SNextRecord = 'Следуюча запис';
  SLastRecord = 'Остання запис';
  SInsertRecord = 'Вставка запису';
  SCopyRecord = 'Копиювання запису';
  SDeleteRecord = 'Видалення запису';
  SEditRecord = 'Редагування запису';
  SFilterRecord = 'Вiдбiр записiв';
  SFindRecord = 'Поiск запису';
  SPrintRecord = 'Друк записiв';
  SExportRecord = 'Експорт записiв';
  SPostEdit = 'Збереження змiн';
  SCancelEdit = 'Вiдмiна змiн';
  SRefreshRecord = 'Оновлення данних';
  SChoice = 'Вибiр данних';
  SClear = 'Очистити даннi';
  SDeleteRecordQuestion = 'Видалити запис?';
  SDeleteMultipleRecordsQuestion = 'Видалити усi вибранi записи?';
  SRecordNotFound = 'Запис не знайдена';

  SFirstName = 'Початок';
  SPriorName = 'Попер.';
  SNextName = 'Слiд.';
  SLastName = 'Кiнец';
  SInsertName = 'Вставка';
  SCopyName = 'Копiювання';
  SDeleteName = 'Видалення';
  SEditName = 'Редактування';
  SFilterName = 'Фiльтр';
  SFindName = 'Поiск';
  SPrintName = 'Друк';
  SExportName = 'Експорт';
  SPostName = 'Збереження';
  SCancelName = 'Вiдмiна';
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
  etIsNot = 'не являеться';
  etOutOfRange = 'Значення %s не лежить в диапазонi %s..%s';

implementation

end.
