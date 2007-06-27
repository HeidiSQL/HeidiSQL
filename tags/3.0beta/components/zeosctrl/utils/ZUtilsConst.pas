{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Common resource constants                 }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZUtilsConst;

interface

{$INCLUDE ..\Zeos.inc}

resourcestring

{$IFNDEF RUSSIAN} {$IFNDEF GERMAN} {$IFNDEF PORTUGUESE}
{$IFNDEF FRENCH} {$IFNDEF POLISH} {$IFNDEF CZECH}
{$IFNDEF ITALIAN} {$IFNDEF SPANISH} {$IFNDEF HUNGARY}

  SDetails         = 'Information';
  SLoadLibError    = 'Can''t load library "%s"';
  SFileNotExec     = 'Specified file is''t execute file, dynamic library or icon';
  SNotImplemented  = 'Function not implemented';
  SResNotFound     = 'Resource not found';
  SOutOfResources  = 'Resource overflow';
  SReadError       = 'Read error';

  SHashNotFound    = 'Hash-item with key = "%s" not found';
  SAllocError      = 'Memory allocation error';
  STooManyHash     = 'Too many items in hash-table';

  SLoadWinsockError = 'Unable to load Winsock';
  SWinsockError     = 'Winsocket error: %s No %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Unpossible to change GIF size';
  SNoGIFData        = 'No data to write GIF';
  SUnrecognizedGIFExt = 'Incorrect GIF extention: %.2x';
  SWrongGIFColors   = 'Incorrect colors number; must be power of 2';
  SBadGIFCodeSize   = 'Size of GIF code isn''t between 2 and 9';
  SGIFDecodeError   = 'Incorrect data in GIF format';
  SGIFEncodeError   = 'GIF format data encoding error';
  SGIFVersion       = 'Unknown GIF version';
  SInvalidBitmap    = 'Incorrect bitmap format';
  SGridOutbound     = 'Out of grid bound';

{$ENDIF} {$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}

{$IFDEF RUSSIAN}

  SDetails         = 'Информация';
  SLoadLibError    = 'Не могу загрузить библиотеку = "%s"';
  SFileNotExec     = 'Указанный файл не является исполняемым, динамической библиотекой или иконкой';
  SNotImplemented  = 'Функция не реализована';
  SResNotFound     = 'Ресурс не найден';
  SOutOfResources  = 'Переполнение ресурсов';
  SReadError       = 'Ошибка чтения';

  SHashNotFound    = 'Элемент хеша с ключем = "%s" не найден';
  SAllocError      = 'Ошибка распределения памяти';
  STooManyHash     = 'Слишком много элементов в хеше';

  SLoadWinsockError = 'Невозможно загрузить Winsock';
  SWinsockError     = 'Ошибка Winsocket: %s номер %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Невозможно изменить размер GIF';
  SNoGIFData        = 'Нет данных для записи в GIF';
  SUnrecognizedGIFExt = 'Неправильное расширение GIF: %.2x';
  SWrongGIFColors   = 'Неверное количество цветов; должно быть степенью 2';
  SBadGIFCodeSize   = 'Размер кода GIF не между 2 и 9';
  SGIFDecodeError   = 'Неправильные данные в формате GIF';
  SGIFEncodeError   = 'Ошибка кодирования данных GIF';
  SGIFVersion       = 'Неверная версия GIF';
  SInvalidBitmap    = 'Неправильный формат изображения';
  SGridOutbound     = 'Выход за границы сетки';

{$ENDIF}

{$IFDEF GERMAN}

  SDetails         = 'Information';
  SLoadLibError    = 'Can''t load library "%s"';
  SFileNotExec     = 'Specified file is''t execute file, dynamic library or icon';
  SNotImplemented  = 'Function not implemented';
  SResNotFound     = 'Resource not found';
  SOutOfResources  = 'Resource overflow';
  SReadError       = 'Read error';

  SHashNotFound    = 'Hash-item with key = "%s" not found';
  SAllocError      = 'Memory allocation error';
  STooManyHash     = 'Too many items in hash-table';

  SLoadWinsockError = 'Unable to load Winsock';
  SWinsockError     = 'Winsocket error: %s No %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Unpossible to change GIF size';
  SNoGIFData        = 'No data to write GIF';
  SUnrecognizedGIFExt = 'Incorrect GIF extention: %.2x';
  SWrongGIFColors   = 'Incorrect colors number; must be power of 2';
  SBadGIFCodeSize   = 'Size of GIF code isn''t between 2 and 9';
  SGIFDecodeError   = 'Incorrect data in GIF format';
  SGIFEncodeError   = 'GIF format data encoding error';
  SGIFVersion       = 'Unknown GIF version';
  SInvalidBitmap    = 'Incorrect bitmap format';
  SGridOutbound     = 'Out of grid bound';

{$ENDIF}

{$IFDEF PORTUGUESE}

  SDetails         = 'Informaзгo';
  SLoadLibError    = 'Impossнvel carrega biblioteca "%s"';
  SFileNotExec     = 'Arquivo especificado nгo й um arquivo executбvel, biblioteca dinвmica ou нcone';
  SNotImplemented  = 'Funзгo nгo implementada';
  SResNotFound     = 'Recurso nгo encontrado';
  SOutOfResources  = 'Estouro de recurso';
  SReadError       = 'Erro de leitura';

  SHashNotFound    = 'Нtem Hash com chave = "%s" nгo encontrado';
  SAllocError      = 'Erro de alocaзгo de memуria';
  STooManyHash     = 'Нtens em excesso na tabela hash';

  SLoadWinsockError = 'Impossнvel carregar Winsock';
  SWinsockError     = 'Erro Winsocket : %s No %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Unpossible to change GIF size';
  SNoGIFData        = 'No data to write GIF';
  SUnrecognizedGIFExt = 'Incorrect GIF extention: %.2x';
  SWrongGIFColors   = 'Incorrect colors number; must be power of 2';
  SBadGIFCodeSize   = 'Size of GIF code isn''t between 2 and 9';
  SGIFDecodeError   = 'Incorrect data in GIF format';
  SGIFEncodeError   = 'GIF format data encoding error';
  SGIFVersion       = 'Unknown GIF version';
  SInvalidBitmap    = 'Incorrect bitmap format';
  SGridOutbound     = 'Out of grid bound';

{$ENDIF}

{$IFDEF FRENCH}

  SDetails         = 'Information';
  SLoadLibError    = 'Can''t load library "%s"';
  SFileNotExec     = 'Specified file is''t execute file, dynamic library or icon';
  SNotImplemented  = 'Function not implemented';
  SResNotFound     = 'Resource not found';
  SOutOfResources  = 'Resource overflow';
  SReadError       = 'Read error';

  SHashNotFound    = 'Hash-item with key = "%s" not found';
  SAllocError      = 'Memory allocation error';
  STooManyHash     = 'Too many items in hash-table';

  SLoadWinsockError = 'Unable to load Winsock';
  SWinsockError     = 'Winsocket error: %s No %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Unpossible to change GIF size';
  SNoGIFData        = 'No data to write GIF';
  SUnrecognizedGIFExt = 'Incorrect GIF extention: %.2x';
  SWrongGIFColors   = 'Incorrect colors number; must be power of 2';
  SBadGIFCodeSize   = 'Size of GIF code isn''t between 2 and 9';
  SGIFDecodeError   = 'Incorrect data in GIF format';
  SGIFEncodeError   = 'GIF format data encoding error';
  SGIFVersion       = 'Unknown GIF version';
  SInvalidBitmap    = 'Incorrect bitmap format';
  SGridOutbound     = 'Out of grid bound';

{$ENDIF}

{$IFDEF POLISH}

  SDetails         = 'Informacja';
  SLoadLibError    = 'Nie mo+na za¦adowaц biblioteki "%s"';
  SFileNotExec     = 'OkreЬlony plik nie jest plikiem wykonywalnym, bibliotek¦ ani ikon¦';
  SNotImplemented  = 'Niezaimplementowana funkcja';
  SResNotFound     = 'Nie znaleziono zasobu';
  SOutOfResources  = 'Przepe¦nione zasoby';
  SReadError       = 'B¦¦d odczytu';

  SHashNotFound    = 'Nie znaleziono pozycji z kluczem "%s"';
  SAllocError      = 'B¦¦d alokacji pamiъci';
  STooManyHash     = 'Zbyt wiele pozycji w tabeli rozproszonej';

  SLoadWinsockError = 'Nie mo+na za¦adowaц Winsocka';
  SWinsockError     = 'B¦¦d Winsocket: %s nr %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Unpossible to change GIF size';
  SNoGIFData        = 'No data to write GIF';
  SUnrecognizedGIFExt = 'Incorrect GIF extention: %.2x';
  SWrongGIFColors   = 'Incorrect colors number; must be power of 2';
  SBadGIFCodeSize   = 'Size of GIF code isn''t between 2 and 9';
  SGIFDecodeError   = 'Incorrect data in GIF format';
  SGIFEncodeError   = 'GIF format data encoding error';
  SGIFVersion       = 'Unknown GIF version';
  SInvalidBitmap    = 'Incorrect bitmap format';
  SGridOutbound     = 'Out of grid bound';

{$ENDIF}

{$IFDEF CZECH}

  SDetails         = 'Information';
  SLoadLibError    = 'Can''t load library "%s"';
  SFileNotExec     = 'Specified file is''t execute file, dynamic library or icon';
  SNotImplemented  = 'Function not implemented';
  SResNotFound     = 'Resource not found';
  SOutOfResources  = 'Resource overflow';
  SReadError       = 'Read error';

  SHashNotFound    = 'Hash-item with key = "%s" not found';
  SAllocError      = 'Memory allocation error';
  STooManyHash     = 'Too many items in hash-table';

  SLoadWinsockError = 'Unable to load Winsock';
  SWinsockError     = 'Winsocket error: %s No %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Unpossible to change GIF size';
  SNoGIFData        = 'No data to write GIF';
  SUnrecognizedGIFExt = 'Incorrect GIF extention: %.2x';
  SWrongGIFColors   = 'Incorrect colors number; must be power of 2';
  SBadGIFCodeSize   = 'Size of GIF code isn''t between 2 and 9';
  SGIFDecodeError   = 'Incorrect data in GIF format';
  SGIFEncodeError   = 'GIF format data encoding error';
  SGIFVersion       = 'Unknown GIF version';
  SInvalidBitmap    = 'Incorrect bitmap format';
  SGridOutbound     = 'Out of grid bound';

{$ENDIF}

{$IFDEF ITALIAN}

  SDetails         = 'Informazioni';
  SLoadLibError    = 'Impossibile caricare la libreria "%s"';
  SFileNotExec     = 'Il file non и un file eseguibile, libreria dinamica o icona';
  SNotImplemented  = 'Funzione non implementata';
  SResNotFound     = 'Risorsa non trovata';
  SOutOfResources  = 'Risorsa esaurita';
  SReadError       = 'Errore in lettura';

  SHashNotFound    = 'Hash-item con chiave = "%s" non trovato';
  SAllocError      = 'Errore di allocazione di memoria';
  STooManyHash     = 'Troppi elementi nella hash-table';

  SLoadWinsockError = 'Impossibile caricare Winsock';
  SWinsockError     = 'Errore Winsocket : %s No %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Unpossible to change GIF size';
  SNoGIFData        = 'No data to write GIF';
  SUnrecognizedGIFExt = 'Incorrect GIF extention: %.2x';
  SWrongGIFColors   = 'Incorrect colors number must be power of 2';
  SBadGIFCodeSize   = 'Size of GIF code isn''t between 2 and 9';
  SGIFDecodeError   = 'Incorrect data in GIF format';
  SGIFEncodeError   = 'GIF format data encoding error';
  SGIFVersion       = 'Unknown GIF version';
  SInvalidBitmap    = 'Incorrect bitmap format';
  SGridOutbound     = 'Out of grid bound';

{$ENDIF}

{$IFDEF SPANISH}

  SDetails         = 'Informaciуn';
  SLoadLibError    = 'No se puede cargar la librerнa "%s"';
  SFileNotExec     = 'El archivo especificado no es un ejecutable, librerнa dinбmica o icono';
  SNotImplemented  = 'Funciуn no implementada';
  SResNotFound     = 'Recurso no encontrado';
  SOutOfResources  = 'Desbordamiento del recurso';
  SReadError       = 'Error de lectura';

  SHashNotFound    = 'Elemento hash con clave = "%s" no encontrado';
  SAllocError      = 'Error de asignacion de memoria';
  STooManyHash     = 'Demasiados elementos en la tabla hash';

  SLoadWinsockError = 'Imposible cargar Winsock';
  SWinsockError     = 'Error de Winsocket: %s No %d.';

  SGIFImage         = 'Imagen GIF de CompuServe';
  SChangeGIFSize    = 'Imposible cambiar el tamaсo del GIF';
  SNoGIFData        = 'No hay datos para escribir GIF';
  SUnrecognizedGIFExt = 'Extensiуn incorrecta de GIF: %.2x';
  SWrongGIFColors   = 'Nъmero incorrecto de colores; debe ser una potencia de 2';
  SBadGIFCodeSize   = 'Tamaсo de cуdigo GIF no estб entre 2 y 9';
  SGIFDecodeError   = 'Datos incorrectos en formato GIF';
  SGIFEncodeError   = 'Error de codificaciуn de datos en formato GIF';
  SGIFVersion       = 'Versiуn desconocida de GIF';
  SInvalidBitmap    = 'Formato incorrecto de Bitmap';
  SGridOutbound     = 'Se excediу el lнmite de la rejilla';

{$ENDIF}

{$IFDEF HUNGARY}

  SDetails         = 'Informбciу';
  SLoadLibError    = 'Nem betцlthetх a "%s"';
  SFileNotExec     = 'A megadott fбjl nem vйgrehajthatу, DLL vagy Icon';
  SNotImplemented  = 'A funkciу nem megvalуsнtott';
  SResNotFound     = 'Az erхforrбs nem talбlhatу';
  SOutOfResources  = 'Erхforrбshiбny';
  SReadError       = 'Olvasбsi hiba';

  SHashNotFound    = 'Hash-elem a = "%s" kulcshoz nem talбlhatу';
  SAllocError      = 'Memуria-allokбciуs hiba';
  STooManyHash     = 'Tъl sok elem a hash-tбblбban';

  SLoadWinsockError = 'Winsock betцltйsi hiba';
  SWinsockError     = 'Winsocket hiba: %s szбmъ %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Nem lehet megvбltoztatni a GIF mйretйt';
  SNoGIFData        = 'Nincs GIF-be нrandу adat';
  SUnrecognizedGIFExt = 'Hibбs GIF kiterjesztйs: %.2x';
  SWrongGIFColors   = 'Hibбs szбmъ szнn, kettх hatvбnya kell';
  SBadGIFCodeSize   = 'GIF kуd nincs 2 йs 9 kцzцtt';
  SGIFDecodeError   = 'Йrvйnytelen adat a GIF formбtumban';
  SGIFEncodeError   = 'GIF formбtum-adat kуdolбsi hiba';
  SGIFVersion       = 'Ismeretlen GIF vбltozat';
  SInvalidBitmap    = 'Hibбs Bitmap formбtum';
  SGridOutbound     = 'Rбcshatбron kнvьl';

{$ENDIF}

implementation

end.
