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

  SFilterError      = '������ � ��������� �������';
  SFilterCaption    = '��������� �������';
  SFilterFields     = '������ �� �����';
  SFieldCaption     = '����';
  SFieldHint        = '������� ����� ����';
  SSetFilterHint    = '��������� �������';
  SClearFilterHint  = '������� �������';
  SFindBy           = '����� �� = ';
  SFindStep         = '����� �������';
  SFindOpt          = '����� � ������������';
  SFindCaption      = '����� ����';
  SFindItem         = '&�����';
  SFindNextItem     = '����� &���������';
  SCancelItem       = '&������';
  SSampleItem       = '&����� ������:';
  SStartItem        = '&��������� �������:';
  SForwardItem      = '������';
  SBackwardItem     = '�����';
  SAllItem          = '���';
  SSimilarItem      = '&������:';
  SAnyPartItem      = '� ����� ������';
  SWholeFieldItem   = '���� �������';
  SFromBeginItem    = '� ������';
  SCaseCaption      = '� &������ ��������';
  SRegularCaption   = '&���������� ���������';
  SStatusCaption    = '��� ������';
  SNoMoreRecords    = '������ ����� �� �������';
  SIncSearch        = '&��������������� �����';
  SFindField        = '&����� ���� ...';
  SIncludeField     = '��������� &����� ����';
  SExcludeField     = '&��������� ����';
  SSetFilter        = '&���������� ������ ...';
  SDropFilter       = '&����� ������ ...';
  SSortAsc          = '���������� ��&�����������';
  SSortDesc         = '���������� ��&��������';
  SOperations       = '&�������� ...';

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

  SFilterError      = 'Filtro de equa��o incorreto';
  SFilterCaption    = 'Define Filtro';
  SFilterFields     = 'Campos Filtro';
  SFieldCaption     = 'Campo';
  SFieldHint        = 'Insira nome do campo';
  SSetFilterHint    = 'Define Filtro';
  SClearFilterHint  = 'Limpa Filtro';
  SFindBy           = 'Buscar por = ';
  SFindStep         = 'Buscar passo a passo';
  SFindOpt          = 'Buscar com otimiza��o';
  SFindCaption      = 'Buscar Campo';
  SFindItem         = '&Buscar';
  SFindNextItem     = 'Buscar &Pr�ximo';
  SCancelItem       = '&Cancela';
  SSampleItem       = '&Texto a Buscar:';
  SStartItem        = '&Ponto Inicial:';
  SForwardItem      = 'Para Frente';
  SBackwardItem     = 'Para Tr�s';
  SAllItem          = 'Tudo';
  SSimilarItem      = 'Si&milar:';
  SAnyPartItem      = 'Qualquer parte do campo';
  SWholeFieldItem   = 'Campo inteior';
  SFromBeginItem    = 'Do come�o';
  SCaseCaption      = '&Sens�vel a capitaliza��o';
  SRegularCaption   = '&Expres�es regulares';
  SStatusCaption    = 'Tipo da busca';
  SNoMoreRecords    = 'Sem mais registros';
  SIncSearch        = 'Busca &Incremental';
  SFindField        = 'Busca Cam&po ...';
  SIncludeField     = 'Inc&lui campo';
  SExcludeField     = 'E&xclui campo';
  SSetFilter        = '&Define Filtro ...';
  SDropFilter       = '&Apaga Filtro ...';
  SSortAsc          = '&Classifica��o crescente';
  SSortDesc         = 'Classifica��o &decrescente';
  SOperations       = '&Opera��es ...';

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

  SFilterError      = 'B��dna posta� filtru';
  SFilterCaption    = 'Ustaw filtr';
  SFilterFields     = 'Pola filtru';
  SFieldCaption     = 'Pole';
  SFieldHint        = 'Wstaw nazwy p�l';
  SSetFilterHint    = 'Ustaw filtr';
  SClearFilterHint  = 'Wyczy�� filtr';
  SFindBy           = 'Szukaj ... = ';
  SFindStep         = 'Krok po kroku';
  SFindOpt          = 'Z optymalizacj�';
  SFindCaption      = 'Szukaj ...';
  SFindItem         = '&Szukaj';
  SFindNextItem     = 'Szukaj &nast�pne';
  SCancelItem       = '&Anuluj';
  SSampleItem       = 'Szukany &tekst:';
  SStartItem        = '&Kierunek:';
  SForwardItem      = 'Do przodu';
  SBackwardItem     = 'Do ty�u';
  SAllItem          = 'Wszystko';
  SSimilarItem      = '&Podobie�stwo:';
  SAnyPartItem      = 'Jakakolwiek cz���';
  SWholeFieldItem   = 'Ca�o��';
  SFromBeginItem    = 'Zaczynaj�ce si� od';
  SCaseCaption      = '&Wa+na wielko�� liter';
  SRegularCaption   = '&Zwyk�e wyra+enie';
  SStatusCaption    = 'Rodzaj poszukiwa�';
  SNoMoreRecords    = 'Nie znaleziono rekord�w';
  SIncSearch        = 'Poszukiwanie &rosn�ce';
  SFindField        = '&Znajd�...';
  SIncludeField     = '&Do��cz pole';
  SExcludeField     = '&Wyklucz pole';
  SSetFilter        = '&Ustaw filtr ...';
  SDropFilter       = 'U&su� filtr ...';
  SSortAsc          = 'Sortuj &rosn�co';
  SSortDesc         = 'Sortuj &malej�co';
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

  SFilterError      = 'Ecuaci�n de Filtro Incorrecta';
  SFilterCaption    = 'Aplicar Filtro';
  SFilterFields     = 'Filtrar Campos';
  SFieldCaption     = 'Campo';
  SFieldHint        = 'Insertar Nombre de Campo';
  SSetFilterHint    = 'Aplicar Filtro';
  SClearFilterHint  = 'Quitar Filtro';
  SFindBy           = 'Buscar por = ';
  SFindStep         = 'B�squeda paso a paso';
  SFindOpt          = 'B�squeda con Optimizaci�n';
  SFindCaption      = 'Buscar Campo';
  SFindItem         = '&Buscar';
  SFindNextItem     = 'Buscar &Siguiente';
  SCancelItem       = '&Cancelar';
  SSampleItem       = 'Buscar &Texto:';
  SStartItem        = 'Posici�n &Inicial:';
  SForwardItem      = 'Avanzar';
  SBackwardItem     = 'Retroceder';
  SAllItem          = 'Todo';
  SSimilarItem      = 'Simi&lar:';
  SAnyPartItem      = 'Cualquier Parte del Campo';
  SWholeFieldItem   = 'Campo Completo';
  SFromBeginItem    = 'Desde el Inicio';
  SCaseCaption      = '&May�sculas/Min�sculas';
  SRegularCaption   = '&Expresiones Regulares';
  SStatusCaption    = 'Tipo de B�squeda';
  SNoMoreRecords    = 'No se encontraron m�s registros';
  SIncSearch        = 'B�squeda I&ncremental';
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

  SFilterError      = 'Hib�s Filter egyenlet';
  SFilterCaption    = 'Filter be�ll�t�s';
  SFilterFields     = 'Filter mez�k';
  SFieldCaption     = 'Mez�';
  SFieldHint        = 'Mez�n�v besz�r�s';
  SSetFilterHint    = 'Filter be�ll�t�s';
  SClearFilterHint  = 'Filter t�rl�s';
  SFindBy           = 'Keress a = ';
  SFindStep         = 'Keres�s egyenk�nt';
  SFindOpt          = 'Optimaliz�lt keres�s';
  SFindCaption      = 'Mez�keres�s';
  SFindItem         = '&Keres�s';
  SFindNextItem     = 'K�&vetkez�';
  SCancelItem       = '&M�gsem';
  SSampleItem       = 'Keresend� &Sz�veg:';
  SStartItem        = '&Kezd�poz�ci�:';
  SForwardItem      = 'El�re';
  SBackwardItem     = 'Vissza';
  SAllItem          = 'Mind';
  SSimilarItem      = '&Hasonl�:';
  SAnyPartItem      = 'R�szmez�';
  SWholeFieldItem   = 'Teljes mez�';
  SFromBeginItem    = 'El�lr�l';
  SCaseCaption      = '&Bet� �rz�kenys�g';
  SRegularCaption   = '&Regul�ris kifejez�sek';
  SStatusCaption    = 'Keres�st�pus';
  SNoMoreRecords    = 'Nincs t�bb tal�lat';
  SIncSearch        = '&N�vekm�nyes keres�s';
  SFindField        = '&Mez� keres�s ...';
  SIncludeField     = 'Mez� &bele�rt';
  SExcludeField     = 'Mez� &kiz�rva';
  SSetFilter        = '&Filter be�ll�t�sa ...';
  SDropFilter       = 'Filter &elvet�se ...';
  SSortAsc          = '&N�vekv� rendez�s';
  SSortDesc         = '&Cs�kken� rendez�s';
  SOperations       = '&M�veletek ...';

{$ENDIF}

implementation

end.
