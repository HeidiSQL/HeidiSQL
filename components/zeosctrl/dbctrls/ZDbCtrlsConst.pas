{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{         Dbware controls resources and constants        }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZDbCtrlsConst;

interface

{$INCLUDE ..\Zeos.inc}

resourcestring

{$IFNDEF RUSSIAN} {$IFNDEF GERMAN} {$IFNDEF PORTUGUESE} 
{$IFNDEF FRENCH} {$IFNDEF POLISH} {$IFNDEF CZECH}
{$IFNDEF ITALIAN} {$IFNDEF SPANISH} {$IFNDEF HUNGARY}

  SFilterError      = 'Incorrect filter equation';
  SFilterCaption    = 'Set Filter';
  SFilterFields     = 'Filter Fields';
  SFieldCaption     = 'Field';
  SFieldHint        = 'Insert field name';
  SSetFilterHint    = 'Set filter';
  SClearFilterHint  = 'Clear filter';
  SFindBy           = 'Find by = ';
  SFindStep         = 'Find step by step';
  SFindOpt          = 'Find with optimization';
  SFindCaption      = 'Find Field';
  SFindItem         = '&Find';
  SFindNextItem     = 'Find &Next';
  SCancelItem       = '&Cancel';
  SSampleItem       = '&Text for search:';
  SStartItem        = '&Start position:';
  SForwardItem      = 'Forward';
  SBackwardItem     = 'Backward';
  SAllItem          = 'All';
  SSimilarItem      = 'Si&milar:';
  SAnyPartItem      = 'Any part field';
  SWholeFieldItem   = 'Whole field';
  SFromBeginItem    = 'From begining';
  SCaseCaption      = '&Case sensitive';
  SRegularCaption   = '&Regular expressions';
  SStatusCaption    = 'Search type';
  SNoMoreRecords    = 'No more records was found';
  SIncSearch        = '&Incremental search';
  SFindField        = '&Find Field ...';
  SIncludeField     = '&Include field';
  SExcludeField     = '&Exclude field';
  SSetFilter        = '&Set filter ...';
  SDropFilter       = 'D&rop filter ...';
  SSortAsc          = 'Sort &ascending';
  SSortDesc         = 'Sort &descending';
  SOperations       = '&Operations ...';

{$ENDIF} {$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}

{$IFDEF RUSSIAN}

  SFilterError      = 'Ошибка в выражении фильтра';
  SFilterCaption    = 'Установка фильтра';
  SFilterFields     = 'Фильтр по полям';
  SFieldCaption     = 'Поле';
  SFieldHint        = 'Вставка имени поля';
  SSetFilterHint    = 'Установка фильтра';
  SClearFilterHint  = 'Очистка фильтра';
  SFindBy           = 'Найти по = ';
  SFindStep         = 'Поиск пошагам';
  SFindOpt          = 'Поиск с оптимизацией';
  SFindCaption      = 'Найти поле';
  SFindItem         = '&Найти';
  SFindNextItem     = 'Найти &следующее';
  SCancelItem       = '&Отмена';
  SSampleItem       = '&Поиск текста:';
  SStartItem        = '&Начальная позиция:';
  SForwardItem      = 'Вперед';
  SBackwardItem     = 'Назад';
  SAllItem          = 'Все';
  SSimilarItem      = '&Шаблон:';
  SAnyPartItem      = 'С любой частью';
  SWholeFieldItem   = 'Поле целиком';
  SFromBeginItem    = 'С начала';
  SCaseCaption      = 'С &учетом регистра';
  SRegularCaption   = '&Регулярное выражение';
  SStatusCaption    = 'Тип поиска';
  SNoMoreRecords    = 'Больше полей не найдено';
  SIncSearch        = '&Инкрементальный поиск';
  SFindField        = '&Найти поле ...';
  SIncludeField     = 'Исключить &кроме поля';
  SExcludeField     = '&Исключить поле';
  SSetFilter        = '&Установить фильтр ...';
  SDropFilter       = '&Снять фильтр ...';
  SSortAsc          = 'Сортировка по&возрастанию';
  SSortDesc         = 'Сортировка по&убыванию';
  SOperations       = '&Операции ...';

{$ENDIF}

{$IFDEF GERMAN}

  SFilterError      = 'Incorrect filter equation';
  SFilterCaption    = 'Set Filter';
  SFilterFields     = 'Filter Fields';
  SFieldCaption     = 'Field';
  SFieldHint        = 'Insert field name';
  SSetFilterHint    = 'Set filter';
  SClearFilterHint  = 'Clear filter';
  SFindBy           = 'Find by = ';
  SFindStep         = 'Find step by step';
  SFindOpt          = 'Find with optimization';
  SFindCaption      = 'Find Field';
  SFindItem         = '&Find';
  SFindNextItem     = 'Find &Next';
  SCancelItem       = '&Cancel';
  SSampleItem       = '&Text for search:';
  SStartItem        = '&Start position:';
  SForwardItem      = 'Forward';
  SBackwardItem     = 'Backward';
  SAllItem          = 'All';
  SSimilarItem      = 'Si&milar:';
  SAnyPartItem      = 'Any part field';
  SWholeFieldItem   = 'Whole field';
  SFromBeginItem    = 'From begining';
  SCaseCaption      = '&Case sensitive';
  SRegularCaption   = '&Regular expressions';
  SStatusCaption    = 'Search type';
  SNoMoreRecords    = 'No more records was found';
  SIncSearch        = '&Incremental search';
  SFindField        = '&Find Field ...';
  SIncludeField     = '&Include field';
  SExcludeField     = '&Exclude field';
  SSetFilter        = '&Set filter ...';
  SDropFilter       = 'D&rop filter ...';
  SSortAsc          = 'Sort &ascending';
  SSortDesc         = 'Sort &descending';
  SOperations       = '&Operations ...';

{$ENDIF}

{$IFDEF PORTUGUESE}

  SFilterError      = 'Filtro de equaзгo incorreto';
  SFilterCaption    = 'Define Filtro';
  SFilterFields     = 'Campos Filtro';
  SFieldCaption     = 'Campo';
  SFieldHint        = 'Insira nome do campo';
  SSetFilterHint    = 'Define Filtro';
  SClearFilterHint  = 'Limpa Filtro';
  SFindBy           = 'Buscar por = ';
  SFindStep         = 'Buscar passo a passo';
  SFindOpt          = 'Buscar com otimizaзгo';
  SFindCaption      = 'Buscar Campo';
  SFindItem         = '&Buscar';
  SFindNextItem     = 'Buscar &Prуximo';
  SCancelItem       = '&Cancela';
  SSampleItem       = '&Texto a Buscar:';
  SStartItem        = '&Ponto Inicial:';
  SForwardItem      = 'Para Frente';
  SBackwardItem     = 'Para Trбs';
  SAllItem          = 'Tudo';
  SSimilarItem      = 'Si&milar:';
  SAnyPartItem      = 'Qualquer parte do campo';
  SWholeFieldItem   = 'Campo inteior';
  SFromBeginItem    = 'Do comeзo';
  SCaseCaption      = '&Sensнvel a capitalizaзгo';
  SRegularCaption   = '&Expresхes regulares';
  SStatusCaption    = 'Tipo da busca';
  SNoMoreRecords    = 'Sem mais registros';
  SIncSearch        = 'Busca &Incremental';
  SFindField        = 'Busca Cam&po ...';
  SIncludeField     = 'Inc&lui campo';
  SExcludeField     = 'E&xclui campo';
  SSetFilter        = '&Define Filtro ...';
  SDropFilter       = '&Apaga Filtro ...';
  SSortAsc          = '&Classificaзгo crescente';
  SSortDesc         = 'Classificaзгo &decrescente';
  SOperations       = '&Operaзхes ...';

{$ENDIF}

{$IFDEF FRENCH}

  SFilterError      = 'Incorrect filter equation';
  SFilterCaption    = 'Set Filter';
  SFilterFields     = 'Filter Fields';
  SFieldCaption     = 'Field';
  SFieldHint        = 'Insert field name';
  SSetFilterHint    = 'Set filter';
  SClearFilterHint  = 'Clear filter';
  SFindBy           = 'Find by = ';
  SFindStep         = 'Find step by step';
  SFindOpt          = 'Find with optimization';
  SFindCaption      = 'Find Field';
  SFindItem         = '&Find';
  SFindNextItem     = 'Find &Next';
  SCancelItem       = '&Cancel';
  SSampleItem       = '&Text for search:';
  SStartItem        = '&Start position:';
  SForwardItem      = 'Forward';
  SBackwardItem     = 'Backward';
  SAllItem          = 'All';
  SSimilarItem      = 'Si&milar:';
  SAnyPartItem      = 'Any part field';
  SWholeFieldItem   = 'Whole field';
  SFromBeginItem    = 'From begining';
  SCaseCaption      = '&Case sensitive';
  SRegularCaption   = '&Regular expressions';
  SStatusCaption    = 'Search type';
  SNoMoreRecords    = 'No more records was found';
  SIncSearch        = '&Incremental search';
  SFindField        = '&Find Field ...';
  SIncludeField     = '&Include field';
  SExcludeField     = '&Exclude field';
  SSetFilter        = '&Set filter ...';
  SDropFilter       = 'D&rop filter ...';
  SSortAsc          = 'Sort &ascending';
  SSortDesc         = 'Sort &descending';
  SOperations       = '&Operations ...';

{$ENDIF}

{$IFDEF POLISH}

  SFilterError      = 'B¦ъdna postaц filtru';
  SFilterCaption    = 'Ustaw filtr';
  SFilterFields     = 'Pola filtru';
  SFieldCaption     = 'Pole';
  SFieldHint        = 'Wstaw nazwy pєl';
  SSetFilterHint    = 'Ustaw filtr';
  SClearFilterHint  = 'WyczyЬц filtr';
  SFindBy           = 'Szukaj ... = ';
  SFindStep         = 'Krok po kroku';
  SFindOpt          = 'Z optymalizacj¦';
  SFindCaption      = 'Szukaj ...';
  SFindItem         = '&Szukaj';
  SFindNextItem     = 'Szukaj &nastъpne';
  SCancelItem       = '&Anuluj';
  SSampleItem       = 'Szukany &tekst:';
  SStartItem        = '&Kierunek:';
  SForwardItem      = 'Do przodu';
  SBackwardItem     = 'Do ty¦u';
  SAllItem          = 'Wszystko';
  SSimilarItem      = '&Podobieёstwo:';
  SAnyPartItem      = 'Jakakolwiek czъЬц';
  SWholeFieldItem   = 'Ca¦oЬц';
  SFromBeginItem    = 'Zaczynaj¦ce siъ od';
  SCaseCaption      = '&Wa+na wielkoЬц liter';
  SRegularCaption   = '&Zwyk¦e wyra+enie';
  SStatusCaption    = 'Rodzaj poszukiwaё';
  SNoMoreRecords    = 'Nie znaleziono rekordєw';
  SIncSearch        = 'Poszukiwanie &rosn¦ce';
  SFindField        = '&ZnajdЯ...';
  SIncludeField     = '&Do¦¦cz pole';
  SExcludeField     = '&Wyklucz pole';
  SSetFilter        = '&Ustaw filtr ...';
  SDropFilter       = 'U&suё filtr ...';
  SSortAsc          = 'Sortuj &rosn¦co';
  SSortDesc         = 'Sortuj &malej¦co';
  SOperations       = '&Operacje ...';

{$ENDIF}

{$IFDEF CZECH}

  SFilterError      = 'Incorrect filter equation';
  SFilterCaption    = 'Set Filter';
  SFilterFields     = 'Filter Fields';
  SFieldCaption     = 'Field';
  SFieldHint        = 'Insert field name';
  SSetFilterHint    = 'Set filter';
  SClearFilterHint  = 'Clear filter';
  SFindBy           = 'Find by = ';
  SFindStep         = 'Find step by step';
  SFindOpt          = 'Find with optimization';
  SFindCaption      = 'Find Field';
  SFindItem         = '&Find';
  SFindNextItem     = 'Find &Next';
  SCancelItem       = '&Cancel';
  SSampleItem       = '&Text for search:';
  SStartItem        = '&Start position:';
  SForwardItem      = 'Forward';
  SBackwardItem     = 'Backward';
  SAllItem          = 'All';
  SSimilarItem      = 'Si&milar:';
  SAnyPartItem      = 'Any part field';
  SWholeFieldItem   = 'Whole field';
  SFromBeginItem    = 'From begining';
  SCaseCaption      = '&Case sensitive';
  SRegularCaption   = '&Regular expressions';
  SStatusCaption    = 'Search type';
  SNoMoreRecords    = 'No more records was found';
  SIncSearch        = '&Incremental search';
  SFindField        = '&Find Field ...';
  SIncludeField     = '&Include field';
  SExcludeField     = '&Exclude field';
  SSetFilter        = '&Set filter ...';
  SDropFilter       = 'D&rop filter ...';
  SSortAsc          = 'Sort &ascending';
  SSortDesc         = 'Sort &descending';
  SOperations       = '&Operations ...';

{$ENDIF}

{$IFDEF ITALIAN}

  SFilterError      = 'Errore nella composizione del filtro';
  SFilterCaption    = 'Imposta il Filtro';
  SFilterFields     = 'Campi di Filtro';
  SFieldCaption     = 'Campo';
  SFieldHint        = 'Inserisci il nome del campo';
  SSetFilterHint    = 'Imposta il Filtro';
  SClearFilterHint  = 'Azzere la condizione di filtro';
  SFindBy           = 'Trova con = ';
  SFindStep         = 'Trova step by step';
  SFindOpt          = 'Trova con ottimizzazione';
  SFindCaption      = 'Trova il campo';
  SFindItem         = '&Trova';
  SFindNextItem     = 'Trova &Successivo';
  SCancelItem       = '&Cancella';
  SSampleItem       = 'T&esto da cercare:';
  SStartItem        = '&Posizione di inizio:';
  SForwardItem      = 'Avanti';
  SBackwardItem     = 'Indietro';
  SAllItem          = 'Tutti';
  SSimilarItem      = 'Si&mili:';
  SAnyPartItem      = 'Una parte del campo';
  SWholeFieldItem   = 'Tutto il campo';
  SFromBeginItem    = 'Dall'inizio';
  SCaseCaption      = '&Maiuscole/Minuscole';
  SRegularCaption   = '&Regular expressions';
  SStatusCaption    = 'Tipo Ricerca';
  SNoMoreRecords    = 'Fine records trovati';
  SIncSearch        = 'Ricerca &Incrementale';
  SFindField        = '&Trova campo ...';
  SIncludeField     = '&Includi il campo';
  SExcludeField     = '&Escludi il campo';
  SSetFilter        = '&Imposta filtro ...';
  SDropFilter       = 'Rimuo&vi filtro ...';
  SSortAsc          = 'Ordine &ascendente';
  SSortDesc         = 'Ordine &descendente';
  SOperations       = '&Operazioni ...';

{$ENDIF}

{$IFDEF SPANISH}

  SFilterError      = 'Ecuaciуn de Filtro Incorrecta';
  SFilterCaption    = 'Aplicar Filtro';
  SFilterFields     = 'Filtrar Campos';
  SFieldCaption     = 'Campo';
  SFieldHint        = 'Insertar Nombre de Campo';
  SSetFilterHint    = 'Aplicar Filtro';
  SClearFilterHint  = 'Quitar Filtro';
  SFindBy           = 'Buscar por = ';
  SFindStep         = 'Bъsqueda paso a paso';
  SFindOpt          = 'Bъsqueda con Optimizaciуn';
  SFindCaption      = 'Buscar Campo';
  SFindItem         = '&Buscar';
  SFindNextItem     = 'Buscar &Siguiente';
  SCancelItem       = '&Cancelar';
  SSampleItem       = 'Buscar &Texto:';
  SStartItem        = 'Posiciуn &Inicial:';
  SForwardItem      = 'Avanzar';
  SBackwardItem     = 'Retroceder';
  SAllItem          = 'Todo';
  SSimilarItem      = 'Simi&lar:';
  SAnyPartItem      = 'Cualquier Parte del Campo';
  SWholeFieldItem   = 'Campo Completo';
  SFromBeginItem    = 'Desde el Inicio';
  SCaseCaption      = '&Mayъsculas/Minъsculas';
  SRegularCaption   = '&Expresiones Regulares';
  SStatusCaption    = 'Tipo de Bъsqueda';
  SNoMoreRecords    = 'No se encontraron mбs registros';
  SIncSearch        = 'Bъsqueda I&ncremental';
  SFindField        = 'Buscar C&ampo ...';
  SIncludeField     = 'Inclui&r Campo';
  SExcludeField     = 'E&xcluir Campo';
  SSetFilter        = 'Aplicar &Filtro ...';
  SDropFilter       = '&Quitar Filtro ...';
  SSortAsc          = 'Orden Ascen&dente';
  SSortDesc         = '&Orden Descendente';
  SOperations       = 'O&peraciones ...';

{$ENDIF}

{$IFDEF HUNGARY}

  SFilterError      = 'Hibбs Filter egyenlet';
  SFilterCaption    = 'Filter beбllнtбs';
  SFilterFields     = 'Filter mezхk';
  SFieldCaption     = 'Mezх';
  SFieldHint        = 'Mezхnйv beszъrбs';
  SSetFilterHint    = 'Filter beбllнtбs';
  SClearFilterHint  = 'Filter tцrlйs';
  SFindBy           = 'Keress a = ';
  SFindStep         = 'Keresйs egyenkйnt';
  SFindOpt          = 'Optimalizбlt keresйs';
  SFindCaption      = 'Mezхkeresйs';
  SFindItem         = '&Keresйs';
  SFindNextItem     = 'Kц&vetkezх';
  SCancelItem       = '&Mйgsem';
  SSampleItem       = 'Keresendх &Szцveg:';
  SStartItem        = '&Kezdхpozнciу:';
  SForwardItem      = 'Elхre';
  SBackwardItem     = 'Vissza';
  SAllItem          = 'Mind';
  SSimilarItem      = '&Hasonlу:';
  SAnyPartItem      = 'Rйszmezх';
  SWholeFieldItem   = 'Teljes mezх';
  SFromBeginItem    = 'Elцlrхl';
  SCaseCaption      = '&Betы йrzйkenysйg';
  SRegularCaption   = '&Regulбris kifejezйsek';
  SStatusCaption    = 'Keresйstнpus';
  SNoMoreRecords    = 'Nincs tцbb talбlat';
  SIncSearch        = '&Nцvekmйnyes keresйs';
  SFindField        = '&Mezх keresйs ...';
  SIncludeField     = 'Mezх &beleйrt';
  SExcludeField     = 'Mezх &kizбrva';
  SSetFilter        = '&Filter beбllнtбsa ...';
  SDropFilter       = 'Filter &elvetйse ...';
  SSortAsc          = '&Nцvekvх rendezйs';
  SSortDesc         = '&Csцkkenх rendezйs';
  SOperations       = '&Mыveletek ...';

{$ENDIF}

implementation

end.
