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

  SDetails         = '����������';
  SLoadLibError    = '�� ���� ��������� ���������� = "%s"';
  SFileNotExec     = '��������� ���� �� �������� �����������, ������������ ����������� ��� �������';
  SNotImplemented  = '������� �� �����������';
  SResNotFound     = '������ �� ������';
  SOutOfResources  = '������������ ��������';
  SReadError       = '������ ������';

  SHashNotFound    = '������� ���� � ������ = "%s" �� ������';
  SAllocError      = '������ ������������� ������';
  STooManyHash     = '������� ����� ��������� � ����';

  SLoadWinsockError = '���������� ��������� Winsock';
  SWinsockError     = '������ Winsocket: %s ����� %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = '���������� �������� ������ GIF';
  SNoGIFData        = '��� ������ ��� ������ � GIF';
  SUnrecognizedGIFExt = '������������ ���������� GIF: %.2x';
  SWrongGIFColors   = '�������� ���������� ������; ������ ���� �������� 2';
  SBadGIFCodeSize   = '������ ���� GIF �� ����� 2 � 9';
  SGIFDecodeError   = '������������ ������ � ������� GIF';
  SGIFEncodeError   = '������ ����������� ������ GIF';
  SGIFVersion       = '�������� ������ GIF';
  SInvalidBitmap    = '������������ ������ �����������';
  SGridOutbound     = '����� �� ������� �����';

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

  SDetails         = 'Informa��o';
  SLoadLibError    = 'Imposs�vel carrega biblioteca "%s"';
  SFileNotExec     = 'Arquivo especificado n�o � um arquivo execut�vel, biblioteca din�mica ou �cone';
  SNotImplemented  = 'Fun��o n�o implementada';
  SResNotFound     = 'Recurso n�o encontrado';
  SOutOfResources  = 'Estouro de recurso';
  SReadError       = 'Erro de leitura';

  SHashNotFound    = '�tem Hash com chave = "%s" n�o encontrado';
  SAllocError      = 'Erro de aloca��o de mem�ria';
  STooManyHash     = '�tens em excesso na tabela hash';

  SLoadWinsockError = 'Imposs�vel carregar Winsock';
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
  SLoadLibError    = 'Nie mo+na za�adowa� biblioteki "%s"';
  SFileNotExec     = 'Okre�lony plik nie jest plikiem wykonywalnym, bibliotek� ani ikon�';
  SNotImplemented  = 'Niezaimplementowana funkcja';
  SResNotFound     = 'Nie znaleziono zasobu';
  SOutOfResources  = 'Przepe�nione zasoby';
  SReadError       = 'B��d odczytu';

  SHashNotFound    = 'Nie znaleziono pozycji z kluczem "%s"';
  SAllocError      = 'B��d alokacji pami�ci';
  STooManyHash     = 'Zbyt wiele pozycji w tabeli rozproszonej';

  SLoadWinsockError = 'Nie mo+na za�adowa� Winsocka';
  SWinsockError     = 'B��d Winsocket: %s nr %d.';

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
  SFileNotExec     = 'Il file non � un file eseguibile, libreria dinamica o icona';
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

  SDetails         = 'Informaci�n';
  SLoadLibError    = 'No se puede cargar la librer�a "%s"';
  SFileNotExec     = 'El archivo especificado no es un ejecutable, librer�a din�mica o icono';
  SNotImplemented  = 'Funci�n no implementada';
  SResNotFound     = 'Recurso no encontrado';
  SOutOfResources  = 'Desbordamiento del recurso';
  SReadError       = 'Error de lectura';

  SHashNotFound    = 'Elemento hash con clave = "%s" no encontrado';
  SAllocError      = 'Error de asignacion de memoria';
  STooManyHash     = 'Demasiados elementos en la tabla hash';

  SLoadWinsockError = 'Imposible cargar Winsock';
  SWinsockError     = 'Error de Winsocket: %s No %d.';

  SGIFImage         = 'Imagen GIF de CompuServe';
  SChangeGIFSize    = 'Imposible cambiar el tama�o del GIF';
  SNoGIFData        = 'No hay datos para escribir GIF';
  SUnrecognizedGIFExt = 'Extensi�n incorrecta de GIF: %.2x';
  SWrongGIFColors   = 'N�mero incorrecto de colores; debe ser una potencia de 2';
  SBadGIFCodeSize   = 'Tama�o de c�digo GIF no est� entre 2 y 9';
  SGIFDecodeError   = 'Datos incorrectos en formato GIF';
  SGIFEncodeError   = 'Error de codificaci�n de datos en formato GIF';
  SGIFVersion       = 'Versi�n desconocida de GIF';
  SInvalidBitmap    = 'Formato incorrecto de Bitmap';
  SGridOutbound     = 'Se excedi� el l�mite de la rejilla';

{$ENDIF}

{$IFDEF HUNGARY}

  SDetails         = 'Inform�ci�';
  SLoadLibError    = 'Nem bet�lthet� a "%s"';
  SFileNotExec     = 'A megadott f�jl nem v�grehajthat�, DLL vagy Icon';
  SNotImplemented  = 'A funkci� nem megval�s�tott';
  SResNotFound     = 'Az er�forr�s nem tal�lhat�';
  SOutOfResources  = 'Er�forr�shi�ny';
  SReadError       = 'Olvas�si hiba';

  SHashNotFound    = 'Hash-elem a = "%s" kulcshoz nem tal�lhat�';
  SAllocError      = 'Mem�ria-allok�ci�s hiba';
  STooManyHash     = 'T�l sok elem a hash-t�bl�ban';

  SLoadWinsockError = 'Winsock bet�lt�si hiba';
  SWinsockError     = 'Winsocket hiba: %s sz�m� %d.';

  SGIFImage         = 'CompuServe GIF Image';
  SChangeGIFSize    = 'Nem lehet megv�ltoztatni a GIF m�ret�t';
  SNoGIFData        = 'Nincs GIF-be �rand� adat';
  SUnrecognizedGIFExt = 'Hib�s GIF kiterjeszt�s: %.2x';
  SWrongGIFColors   = 'Hib�s sz�m� sz�n, kett� hatv�nya kell';
  SBadGIFCodeSize   = 'GIF k�d nincs 2 �s 9 k�z�tt';
  SGIFDecodeError   = '�rv�nytelen adat a GIF form�tumban';
  SGIFEncodeError   = 'GIF form�tum-adat k�dol�si hiba';
  SGIFVersion       = 'Ismeretlen GIF v�ltozat';
  SInvalidBitmap    = 'Hib�s Bitmap form�tum';
  SGridOutbound     = 'R�cshat�ron k�v�l';

{$ENDIF}

implementation

end.
