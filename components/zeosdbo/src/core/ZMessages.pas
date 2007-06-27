// ========================================================================= //
//                                                                           //
//                            Zeos Database Objects                          //
//               Copyright (c) 1999-2005 Zeos Development Group              //
//                                                                           //
// ========================================================================= //
//                                                                           //
// License Agreement:                                                        //
//                                                                           //
// This library is free software; you can redistribute it and/or modify it   //
// under the terms of the GNU Lesser General Public License as published by  //
// the Free Software Foundation; either version 2.1 of the License, or (at   //
// your option) any later version.                                           //
//                                                                           //
// This library is distributed in the hope that it will be useful, but       //
// WITHOUT ANY WARRANTY; without even the implied warranty of                //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser  //
// General Public License for more details.                                  //
//                                                                           //
// You should have received a copy of the GNU Lesser General Public License  //
// along with this library; if not, write to the Free Software Foundation,   //
// Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA             //
//                                                                           //
// The project web site is located on:                                       //
//    http://www.sourceforge.net/projects/zeoslib.                           //
//    http://www.zeoslib.sourceforge.net                                     //
//                                                                           //
//                                                    Zeos Development Group //
// ========================================================================= //


// ========================================================================= //
// ZMessages.pas                                                             //
// ========================================================================= //
//                                                                           //
// This unit contains all the messages that are output by ZEOS methods. One  //
// of the given language can be activated by setting the language in ->      //
// ZEOS.inc (e.g.: {$DEFINE GERMAN}). If no language is defined in ZEOS.inc  //
// the english strings will be used.                                         //
//                                                                           //
// Date         Rev.   Sgn   Comment                                         //
// ------------------------------------------------------------------------- //
// 03/05/2005    1.9   ms    german messagetexts added and corrected some    //
//                           english messages                                //
// 09/05/2005    1.9   ms    dutch and portuguese messagetexts added         //
// 23/06/2005    1.9   ms    added some new message strings (german and      //
//                           english)                                        //
// ========================================================================= //


unit ZMessages;

interface

{$I ZCore.inc}

resourcestring

// -> ms, 09/05/2005
{$IFDEF PORTUGUESE}
  SSQLError1 = 'Erro SQL: %s';
  SSQLError2 = 'Erro SQL: %s Código: %d';
  SSQLError3 = 'Erro SQL: %s Código: %d SQL: %s';
  SSQLError4 = 'Erro SQL: %s Código: %d Mensagem: %s';

  SListCapacityError = 'Capacidade da Lista fora do limite (%d)';
  SListCountError = 'Contagem da Lista fora do limite (%d)';
  SListIndexError = 'Índice da Lista fora do limite (%d)';

  SClonningIsNotSupported = 'Clonagem não é suportada por esta classe';
  SImmutableOpIsNotAllowed = 'A operação não é permitida para coleção imutável';
  SStackIsEmpty = 'Pilha está vazia';
  SVariableWasNotFound = 'Variável "%s" não foi encontrada';
  SFunctionWasNotFound = 'Function "%s" não foi encontrada';
  SInternalError = 'Erro interno';
  SSyntaxErrorNear = 'Erro de sintaxe próximo a "%s"';
  SSyntaxError = 'Erro de sintaxe';
  SUnknownSymbol = 'Símbolo desconhecido "%s"';
  SUnexpectedExprEnd = 'Final inesperado de expressão';
  SRightBraceExpected = ') esperado';
  SParametersError = 'Esperado %d parâmetros mas foi encontrado %d';
  SExpectedMoreParams = 'Esperado mais que 2 parâmetros';
  SInvalidVarByteArray = 'VarByte array inválido';
  SVariableAlreadyExists = 'Variável "%s" já existe';
  STypesMismatch = 'Tipos não combinam';
  SUnsupportedVariantType = 'Tipo variante não suportado';
  SUnsupportedOperation = 'Operação não suportado';

  STokenizerIsNotDefined = 'Tokenizer is not defined';
  SLibraryNotFound = 'Nenhuma biblioteca dinaêmica da lista %s foi encontrada';
  SEncodeDateIsNotSupported = 'Esta versão não suporta isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Esta versão não suporta supported isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Esta versão não suporta supported isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Esta versão não suporta isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Esta versão não suporta isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Esta versão não suporta isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'Não foi possível obter os dados do ResultSet';
  SRowBufferIsNotAssigned = 'Buffer da Linha não atribuído';
  SColumnIsNotAccessable = 'Coluna com índice %d não é acessível';
  SConvertionIsNotPossible = 'A conversão da coluna %d de %s para %s não é possível';
  SCanNotAccessBlobRecord = 'Não é possível acessar um registro BLOB na coluna %d com o tipo %s';
  SRowDataIsNotAvailable = 'Dados na Linha não disponíveis';
  SResolverIsNotSpecified = 'Resolver não foi especificado para este ResultSet';
  SResultsetIsAlreadyOpened = 'ResultSet já está aberto';
  SCanNotUpdateEmptyRow = 'Não é possível atualizar uma linha vazia';
  SCanNotUpdateDeletedRow = 'Não é possível atualizar uma linha apagada';
  SCanNotDeleteEmptyRow = 'Não é possível apagar uma linha vazia';
  SCannotUseCommit = 'Você não pode usar Commit no modo AutoCommit';
  SCannotUseRollBack = 'Você não pode usar Rollback no modo AutoCommit';
  SCanNotUpdateComplexQuery = 'Não é possível atualizar uma query complexa com mais de uma tabela';
  SCanNotUpdateThisQueryType = 'Não é possível atualizar este tipo de query';
  SDriverWasNotFound = 'O driver de banco de dados requisitado não foi encontrado';
  SCanNotConnectToServer = 'Não foi possível conectar ao servidor SQL';
  STableIsNotSpecified = 'Tabela não especificada';
  SLiveResultSetsAreNotSupported = 'Live query não é suportado por esta classe';
  SInvalidInputParameterCount = 'A contagem do parâmetro de entrada é menor que o esperado';
  SIsolationIsNotSupported = 'O nível de isolamento da Transação não é suportado';
  SColumnWasNotFound = 'Coluna com o nome "%s" não foi encontrada';
  SWrongTypeForBlobParameter = 'Tipo errado para parâmetro Blob';
  SIncorrectConnectionURL = 'Conexão incorreta URL: %s';
  SUnsupportedProtocol = 'Protocolo não suportado: %s';

  SConnectionIsNotOpened = 'Conexão ainda não está aberta.';
  SInvalidOpInAutoCommit = 'Operação inválida no modo AutoCommit.';
  SInvalidOpInNonAutoCommit = 'Operação inválida quando o modo AutoCommit é False.';

  SConnectionIsNotAssigned = 'Componente de conexão de banco de dados não atribuído';
  SQueryIsEmpty = 'Consulta SQL está vazia';
  SCanNotExecuteMoreQueries = 'Não é possível executar mais que uma query';
  SOperationIsNotAllowed1 = 'Operação não permitida no modo FORWARD ONLY';
  SOperationIsNotAllowed2 = 'Operação não permitida no modo READ ONLY';
  SOperationIsNotAllowed3 = 'Operação não permitida no modo %s';
  SOperationIsNotAllowed4 = 'Operação não permitida para DataSet fechado';
  SNoMoreRecords = 'Nenhum registro no ResultSet';
  SCanNotOpenResultSet = 'Não foi possível abrir o ResultSet';
  SCircularLink = 'Datasource possui um link circular';
  SBookmarkWasNotFound = 'Bookmark não foi encontrado';
  SIncorrectSearchFieldsNumber = 'Número incorreto de valores de campos de procura';
  SInvalidOperationInTrans = 'Operação inválida no modo de transação explícita';
  SIncorrectSymbol = 'Símbolo incorreto na lista de campos "%s".';
  SIncorrectToken = 'Sinal incorreto seguido por ":"';

  SSelectedTransactionIsolation = 'O nível selecionado do isolamento da transação não é suportado';
  SDriverNotSupported = 'Driver não suportado %s';
  SPattern2Long = 'Padrão é muito longo';
  SDriverNotCapableOutParameters = 'Driver não é capaz sem parâmetros';
  SStatementIsNotAllowed = 'Declaração não permitida';
  SStoredProcIsNotAllowed = 'A stored proc não é permitida';
  SCannotPerformOperation = 'Não é possível executar a operação num ResultSet fechado';
  SInvalidState = 'Estado inválido';
  SErrorConvertion = 'Erro de conversão';
  SDataTypeDoesNotSupported = 'Tipo de dado não suportado';
  SUnsupportedParameterType = 'Tipo de parâmetro não suportado';
  SUnsupportedDataType = 'Tipo de dado não suportado';
  SErrorConvertionField = 'Erro de conversão para do campo "%s" para SQLType "%s"';
  SBadOCI = 'Versão de OCI ruim [% s]. Isto requer 8.0.3 ou mais velho';
  SConnect2AsUser = 'Conecte "% s" como usuário "% s"';
  SUnknownError = 'Erro desconhecido';
  SFieldNotFound1 = 'Campo "%s" não foi encontrado';
  SFieldNotFound2 = 'Campo %d não foi encontrado';

  SLoginPromptFailure = 'Não foi possível encontrar o diálogo padrão de login. Por favor adicione DBLogDlg para a seção uses de seu arquivo principal.';

  SPropertyQuery = 'The Query may last a while on large databases!';
  SPropertyTables = 'You should limit it by Catalog and/or Schema.';
  SPropertyColumns = 'You should limit it by Catalog, Schema and/or TableName.';
  SPropertyProcedures = 'You should limit it by Catalog and/or Schema.';
  SPropertySequences = 'You should limit it by Catalog and/or Schema.';
  SPropertyExecute = 'Should the Query be executed anyway?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '&Close';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'Select SQL';
  SMenuLoad = 'Load';
  SMenuSave = 'Save';
  SButtonGenerate = '&Generate';
  SButtonCheck = 'C&heck';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = '&Cancel';
  STableAlias = 'T&able alias';
  SReplaceSQL = '&Replace SQL';
  SDialogOpenTitle = 'Open SQL File';
  SDialogSaveTitle = 'Save SQL File';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Open existing database';

  {$IFDEF FPC}
  SNotEditing = 'Dataset not in edit or insert mode';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  {$ENDIF}

{$ELSE}

{$IFDEF DUTCH}
  SSQLError1 = 'SQL Fout: %s';
  SSQLError2 = 'SQL Fout: %s Code: %d';
  SSQLError3 = 'SQL Fout: %s Code: %d SQL: %s';
  SSQLError4 = 'SQL Fout: %s Code: %d Bericht: %s';

  SListCapacityError = 'Lijst capaciteit buiten bereik (%d)';
  SListCountError = 'Lijst aantal buiten bereik (%d)';
  SListIndexError = 'Lijst index buiten bereik (%d)';

  SClonningIsNotSupported = 'Kloonen worden niet ondersteund in deze klasse';
  SImmutableOpIsNotAllowed = 'Deze operatie is niet ondersteund voor immutable collection';
  SStackIsEmpty = 'Stack is leeg';
  SVariableWasNotFound = 'Variabele "%s" niet gevonden';
  SFunctionWasNotFound = 'Functie "%s" niet gevonden';
  SInternalError = 'Interne fout';
  SSyntaxErrorNear = 'Syntaxis fout bij "%s"';
  SSyntaxError = 'Syntaxis fout';
  SUnknownSymbol = 'Onbekend symbool "%s"';
  SUnexpectedExprEnd = 'Onverwacht einde van de expressie';
  SRightBraceExpected = ') verwacht';
  SParametersError = 'Verwacht worden %d parameters maar er zijn er %d gevonden';
  SExpectedMoreParams = 'Meer dan 2 parameters werden verwacht';
  SInvalidVarByteArray = 'Ongeldig VarByte array';
  SVariableAlreadyExists = 'Variabele "%s" bestaat al';
  STypesMismatch = 'Types komen niet overeen';
  SUnsupportedVariantType = 'Niet ondersteund variant type';
  SUnsupportedOperation = 'Niet ondersteunde operatie';

  STokenizerIsNotDefined = 'Tokenizer is niet gedefinieerd';
  SLibraryNotFound = 'DLL van de lijst %s werd niet gevonden';
  SEncodeDateIsNotSupported = 'Deze versie ondersteund isc_encode_sql_date niet';
  SEncodeTimeIsNotSupported = 'Deze versie ondersteund isc_encode_sql_time niet';
  SEncodeTimestampIsNotSupported = 'Deze versie ondersteund isc_encode_sql_timestamp niet';
  SDecodeDateIsNotSupported = 'Deze versie ondersteund isc_decode_sql_date niet';
  SDecodeTimeIsNotSupported = 'Deze versie ondersteund isc_decode_sql_time niet';
  SDecodeTimestampIsNotSupported = 'Deze versie ondersteund isc_decode_sql_timestamp niet';

  SCanNotRetrieveResultSetData = 'Kan ResultSet data niet ophalen';
  SRowBufferIsNotAssigned = 'Row buffer is niet toegekend';
  SColumnIsNotAccessable = 'Kolom met index %d is not bereikbaar';
  SConvertionIsNotPossible = 'Conversie is niet mogelijk voor kolom %d van %s tot %s';
  SCanNotAccessBlobRecord = 'Kan het blob record in kolom %d met type %s niet benaderen';
  SRowDataIsNotAvailable = 'Rij data is niet beschikbaar';
  SResolverIsNotSpecified = 'Resolver is not gespecificeerd voor deze ResultSet';
  SResultsetIsAlreadyOpened = 'ResultSet is al geopend';
  SCanNotUpdateEmptyRow = 'Kan een lege rij niet updaten';
  SCanNotUpdateDeletedRow = 'Kan een verwijderde rij niet updaten';
  SCanNotDeleteEmptyRow = 'Kan een lege rij niet verwijderen';
  SCannotUseCommit = 'Commit in autocommit mode is niet mogelijk';
  SCannotUseRollBack = 'Rollback in autocommit mode is niet mogelijk';
  SCanNotUpdateComplexQuery = 'Kan een complex query met meerdere tabellen niet updaten';
  SCanNotUpdateThisQueryType = 'Kan dit query type niet updaten';
  SDriverWasNotFound = 'Gevraagde database driver is niet gevonden';
  SCanNotConnectToServer = 'Kan geen verbinding maken met de SQL server';
  STableIsNotSpecified = 'Table is not gespecificeerd';
  SLiveResultSetsAreNotSupported = 'Live query is niet ondersteund door deze klasse';
  SInvalidInputParameterCount = 'Input parameter aantal is lager dan verwacht';
  SIsolationIsNotSupported = 'Transact isolation level wordt niet ondersteund';
  SColumnWasNotFound = 'Kolom met naam "%s" bestaat niet';
  SWrongTypeForBlobParameter = 'Verkeerde type voor Blob parameter';
  SIncorrectConnectionURL = 'Ongeldige connectie URL: %s';
  SUnsupportedProtocol = 'Niet ondersteund protocol: %s';

  SConnectionIsNotOpened = 'Verbinding is niet niet gemaakt.';
  SInvalidOpInAutoCommit = 'Ongeldige operatie in AutoCommit mode.';
  SInvalidOpInNonAutoCommit = 'Ongeldige operatie in non AutoCommit mode.';

  SConnectionIsNotAssigned = 'Database connectie component is niet toegekend';
  SQueryIsEmpty = 'SQL Query is leeg';
  SCanNotExecuteMoreQueries = 'Kan niet meerdere queries uitvoeren';
  SOperationIsNotAllowed1 = 'Bewerking is niet toegestaan in FORWARD ONLY mode';
  SOperationIsNotAllowed2 = 'Bewerking is niet toegestaan in READ ONLY mode';
  SOperationIsNotAllowed3 = 'Bewerking is niet toegestaan in %s mode';
  SOperationIsNotAllowed4 = 'Bewerking is niet toegestaan voor gesloten dataset';
  SNoMoreRecords = 'Geen meer records in ResultSet aanwezig';
  SCanNotOpenResultSet = 'Kan een ResultSet niet openen';
  SCircularLink = 'Databron maakt een oneindige verbinding';
  SBookmarkWasNotFound = 'Bookmark niet gevonden';
  SIncorrectSearchFieldsNumber = 'Incorrect aantal zoekvelden';
  SInvalidOperationInTrans = 'Ongeldige operatie in explicit transaction mode';
  SIncorrectSymbol = 'Ongeldig symbool in veld lijst "%s".';
  SIncorrectToken = 'Ongeldig teken gevolgd door ":"';

  SSelectedTransactionIsolation = 'Geselecteerd transaction isolation level niet ondersteund';
  SDriverNotSupported = 'Driver niet ondersteund %s';
  SPattern2Long = 'Patroon is te lang';
  SDriverNotCapableOutParameters = 'Driver is not capable out parameters';
  SStatementIsNotAllowed = 'Statement is niet toegestaan';
  SStoredProcIsNotAllowed = 'De stored proc is niet toegestaan';
  SCannotPerformOperation = 'Kan operatie niet uitvoeren op een gesloten ResultSet';
  SInvalidState = 'Ongeldige state';
  SErrorConvertion = 'Conversiefout';
  SDataTypeDoesNotSupported = 'Data type is niet onderstuend';
  SUnsupportedParameterType = 'Niet ondersteund parameter type';
  SUnsupportedDataType = 'Niet ondersteund data type';
  SErrorConvertionField = 'Conversie fout voor veld "%s" naar SQLType "%s"';
  SBadOCI = 'Ongeschikte OCI version [%s]. Vereist is 8.0.3 of nieuwer';
  SConnect2AsUser = 'Verbinden met "%s" als gebruiker "%s"';
  SUnknownError = 'Onbekende fout';
  SFieldNotFound1 = 'Veld "%s" niet gevonden';
  SFieldNotFound2 = 'Veld %d niet gevonden';

  SLoginPromptFailure = 'Kan de standaard login promt niet vinden.  Voeg DBLogDlg toe aan de uses sectie.';

  SPropertyQuery = 'De Query kan enige tijd duren bij grote databases!';
  SPropertyTables = 'Limiet op Catalog en/of Schema is vereist.';
  SPropertyColumns = 'Limiet op Catalog, Schema en/of tablenaam is vereist.';
  SPropertyProcedures = 'Limiet op Catalog en/of Schema is vereist.';
  SPropertySequences = 'Limiet op Catalog en/of Schema is vereist.';
  SPropertyExecute = 'Dient de Query toch te worden uitgevoerd?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '&Close';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'Select SQL';
  SMenuLoad = 'Load';
  SMenuSave = 'Save';
  SButtonGenerate = '&Generate';
  SButtonCheck = 'C&heck';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = '&Cancel';
  STableAlias = 'T&able alias';
  SReplaceSQL = '&Replace SQL';
  SDialogOpenTitle = 'Open SQL File';
  SDialogSaveTitle = 'Save SQL File';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Open existing database';

  {$IFDEF FPC}
  SNotEditing = 'Dataset not in edit or insert mode';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  {$ENDIF}

{$ELSE}
// <- ms, 09/05/2005

// -> ms, 03/05/2005
{$IFDEF GERMAN}
  SSQLError1 = 'SQL Fehler: %s';
  SSQLError2 = 'SQL Fehler: %s Code: %d';
  SSQLError3 = 'SQL Fehler: %s Code: %d SQL: %s';
  SSQLError4 = 'SQL Fehler: %s Code: %d Meldung: %s';

  SListCapacityError = 'Die Listenkapazität übersteigt die definierte Grenze (%d)';
  SListCountError = 'Der Listenzähler ist außerhalb seiner definierten Grenzen (%d)';
  SListIndexError = 'Der Listenindex ist außerhalb der definierten Grenzen (%d)';

  SClonningIsNotSupported = 'Diese Klasse kann nicht geklont werden';
  SImmutableOpIsNotAllowed = 'Diese Operation ist bei nicht änderbaren Collections nicht erlaubt';
  SStackIsEmpty = 'Der Stack ist leer';
  SVariableWasNotFound = 'Die Variable "%s" wurde nicht gefunden';
  SFunctionWasNotFound = 'Die Funktion "%s" wurde nicht gefunden';
  SInternalError = 'Interner Fehler';
  SSyntaxErrorNear = 'Syntax Fehler bei "%s"';
  SSyntaxError = 'Syntax Fehler';
  SUnknownSymbol = 'Unbekanntes Symbol "%s"';
  SUnexpectedExprEnd = 'Unerwartetes Ende des Ausdrucks';
  SRightBraceExpected = ') erwartet';
  SParametersError = 'Es werden %d Parameter erwartet, aber nur %d Parameter gefunden';
  SExpectedMoreParams = 'Es werden mehr als zwei Parameter erwartet';
  SInvalidVarByteArray = 'Ungültiges VarByte Array';
  SVariableAlreadyExists = 'Die Variable "%s" existiert bereits';
  STypesMismatch = 'Inkompatible Typen';
  SUnsupportedVariantType = 'Nicht unterstützter Variant-Typ';
  SUnsupportedOperation = 'Nicht unterstützte Operation';

  STokenizerIsNotDefined = 'Tokenizer wurde nicht definiert';
  SLibraryNotFound = 'Es wurde keine der in %s gelisteten DLL''s gefunden';
  SEncodeDateIsNotSupported = 'Diese Version unterstützt "isc_encode_sql_date" nicht';
  SEncodeTimeIsNotSupported = 'Diese Version unterstützt "isc_encode_sql_time" nicht';
  SEncodeTimestampIsNotSupported = 'Diese Version unterstützt "isc_encode_sql_timestamp" nicht';
  SDecodeDateIsNotSupported = 'Diese Version unterstützt "isc_decode_sql_date" nicht';
  SDecodeTimeIsNotSupported = 'Diese Version unterstützt "isc_decode_sql_time" nicht';
  SDecodeTimestampIsNotSupported = 'Diese Version unterstützt "isc_decode_sql_timestamp" nicht';

  SCanNotRetrieveResultSetData = 'Die Ergebnismenge kann nicht ermittelt werden';
  SRowBufferIsNotAssigned = 'Der Zeilen-Buffer ist nicht zugewiesen';
  SColumnIsNotAccessable = 'Auf die Spalte (Tabellenfeld) mit dem Index %d kann nicht zugegriffen werden';
  SConvertionIsNotPossible = 'Eine Konvertierung der Spalte (Tabellenfeld) %d von %s bis %s kann nicht durchgeführt werden';
  SCanNotAccessBlobRecord = 'Auf den BLOB-Datensatz in Spalte (Tabellenfeld) %d vom Typ %s kann nicht zugegriffen werden';
  SRowDataIsNotAvailable = 'Die Zeilendaten (Datensatzdaten) sind nicht verfügbar';
  SResolverIsNotSpecified = 'Für diese Ergebnismenge wurde kein sog. "Resolver" angegeben';
  SResultsetIsAlreadyOpened = 'Die Ergebnismenge ist bereits geöffnet';
  SCanNotUpdateEmptyRow = 'Eine leere Datenzeile kann nicht aktualisiert werden';
  SCanNotUpdateDeletedRow = 'Eine gelöschte Datenzeile kann nicht aktualisiert werden';
  SCanNotDeleteEmptyRow = 'Eine leere Datenzeile kann nicht gelöscht werden';
  SCannotUseCommit = 'COMMIT kann im AUTOCOMMIT-Modus nicht verwendet werden';
  SCannotUseRollBack = 'ROLLBACK kann im AUTOCOMMIT-Modus nicht verwendet werden';
  SCanNotUpdateComplexQuery = 'Ein Query, dessen Ergebnismenge aus mehr als einer Tabelle stammt, kann nicht aktualisiert werden';
  SCanNotUpdateThisQueryType = 'Diese Art von Queries kann nicht aktualisiert werden';
  SDriverWasNotFound = 'Der angegebene Datenbanktreiber wurde nicht gefunden';
  SCanNotConnectToServer = 'Kann keine Verbindung zum SQL Server herstellen';
  STableIsNotSpecified = 'Tabelle ist nicht spezifiziert';
  SLiveResultSetsAreNotSupported = 'Ein "Live Query" wird von dieser Klasse nicht unterstützt';
  SInvalidInputParameterCount = 'Es wurden weniger Eingabeparameter angegeben, als erwartet';
  SIsolationIsNotSupported = 'Der gewählte Trasaktions-Isolationslevel wird nicht unterstützt';
  SColumnWasNotFound = 'Eine Tabellenspalte namens "%s" wurde nicht gefunden';
  SWrongTypeForBlobParameter = 'Falscher Typ für einen BLOB-Parameter';
  SIncorrectConnectionURL = 'Falsche Verbindungs-URL: %s';
  SUnsupportedProtocol = 'Nicht unterstütztes Protokoll: %s';

  SConnectionIsNotOpened = 'Die Verbindung zur Datenbank ist noch nicht hergestellt';
  SInvalidOpInAutoCommit = 'Ungültige Operation im AUTOCOMMIT-Modus';
  SInvalidOpInNonAutoCommit = 'Ungültige Operation außerhalb des AUTOCOMMIT-Modus';

  SConnectionIsNotAssigned = 'Die Datenbank-Verbindungskomponente ist nicht angegeben';
  SQueryIsEmpty = 'SQL Query leer';
  SCanNotExecuteMoreQueries = 'Mehr als ein Query kann nicht abgearbeitet werden';
  SOperationIsNotAllowed1 = 'Die Operation ist im FORWARD ONLY Modus nicht erlaubt';
  SOperationIsNotAllowed2 = 'Die Operation ist im READ ONLY Modus nicht erlaubt';
  SOperationIsNotAllowed3 = 'Die Operation ist im %s Modus nicht erlaubt';
  SOperationIsNotAllowed4 = 'Die Operation ist bei einem geschlossenen DataSet nicht erlaubt';
  SNoMoreRecords = 'Es gibt keine weiteren Datensätze in der Ergebnismenge';
  SCanNotOpenResultSet = 'Die Ergebnismenge kann nicht geöffnet werden';
  SCircularLink = 'Die DataSource hat einen zirkulären Verweis';
  SBookmarkWasNotFound = 'Das Lesezeichen (Bookmark) wurde nicht gefunden';
  SIncorrectSearchFieldsNumber = 'Die Anzahl der Suchfeldwerte ist nicht korrekt';
  SInvalidOperationInTrans = 'Ungültige Operatio im Zustand einer expliziten Transaktion';
  SIncorrectSymbol = 'Falsches Symbol in der Feldliste "%s".';
  SIncorrectToken = 'Falsches Token gefolgt von ":"';

  SSelectedTransactionIsolation = 'Der gewählte Transaktions-Isolationslevel wird nicht unterstützt';
  SDriverNotSupported = 'Der Treiber wird nicht unterstützt: %s';
  SPattern2Long = 'Das Muster (Pattern) ist zu lang';
  SDriverNotCapableOutParameters = 'Der Treiber beherrscht keine Parameter';
  SStatementIsNotAllowed = 'Diese Anweisung ist nicht erlaubt';
  SStoredProcIsNotAllowed = 'Diese Stored Procedure ist nicht erlaubt';
  SCannotPerformOperation = 'Auf eine geschlossene Ergebnismenge können keine Operationen ausgeführt werden';
  SInvalidState = 'Ungültiger Status';
  SErrorConvertion = 'Konvertierungsfehler';
  SDataTypeDoesNotSupported = 'Der Datentyp wird nicht unterstützt';
  SUnsupportedParameterType = 'Der Parametertyp wird nicht unterstützt';
  SUnsupportedDataType = 'Der Datentyp wird nicht unterstützt';
  SErrorConvertionField = 'Konvertierungsfehler bei Feld "%s" nach SQL-Typ "%s"';
  SBadOCI = 'Die OCI Version 8.0.3 (oder älter) wird benötigt! Aktuelle Version: %s';
  SConnect2AsUser = 'Verbinde zu "%s" als User "%s"';
  SUnknownError = 'Unbekannter Fehler';
  SFieldNotFound1 = 'Das Feld "%s" wurde nicht gefunden';
  SFieldNotFound2 = 'Das Feld %d wurde nicht gefunden';

  SLoginPromptFailure = 'Der Standard-Login-Dialog konnte nicht gefunden werden. Bitte DBLogDlg in die USES-Sektion der Haupt-Unit hinzufügen';

  SPropertyQuery = 'Die Abfrage kann bei großen Datenbanken eine Weile dauern!';
  SPropertyTables = 'Sie sollte durch die Angabe von Catalog und/oder Schema eingeschränkt werden.';
  SPropertyColumns = 'Sie sollte durch die Angabe von Catalog, Schema und/oder Tabellenname eingeschränkt werden.';
  SPropertyProcedures = 'Sie sollte durch die Angabe von Catalog und/oder Schema eingeschränkt werden.';
  SPropertySequences = 'Sie sollte durch die Angabe von Catalog und/oder Schema eingeschränkt werden.';
  SPropertyExecute = 'Soll die Abfrage trotzdem ausgeführt werden?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '&Schließen';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'SQL aus&wählen';
  SMenuLoad = 'Öffnen';
  SMenuSave = 'Speichern';
  SButtonGenerate = '&Generieren';
  SButtonCheck = 'Syntax &Prüfen';
  SButtonTest = 'Befehl &Testen';
  SButtonOk = '&OK';
  SButtonCancel = '&Abbruch';
  STableAlias = 'Tabllen-Alias';
  SReplaceSQL = 'SQL &ersetzen';
  SDialogOpenTitle = 'SQL Script öffnen';
  SDialogSaveTitle = 'SQL Script speichern';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Existierende Datenbank öffnen';

  {$IFDEF FPC}
  SNotEditing = 'Das DataSet ist nicht im "edit" oder "insert" Modus.';
  SFieldTypeMismatch = 'Der Typ für Feld ''%s'' stimmt nicht. Erwartet wird %s der Typ ist aber momentan %s';
  SFieldSizeMismatch = 'Die Größe des Feldes ''%s'' stimmt nicht. Erwartet wird  %d die Größe ist aber momentan %d';
  {$ENDIF}
  // <- ms, 03/05/2005
{$ELSE}
  // -> fduenas, 28/06/2005
{$IFDEF SPANISH} //Spanish translations
  SSQLError1 = 'Error SQL: %s';
  SSQLError2 = 'Error SQL: %s Código: %d';
  SSQLError3 = 'Error SQL: %s Código: %d SQL: %s';
  SSQLError4 = 'Error SQL: %s Código: %d Mensage: %s';

  SListCapacityError = 'List capacity fuera de límites (%d)';
  SListCountError = 'List count fuera de límites (%d)';
  SListIndexError = 'List index fuera de límites (%d)';

  SClonningIsNotSupported = 'La Clonación no está soportada por esta clase';
  SImmutableOpIsNotAllowed = 'Operación no permitida en colecciones no modificables';
  SStackIsEmpty = 'La Pila (Stack) está vacía';
  SVariableWasNotFound = 'Variable "%s" no encontrada';
  SFunctionWasNotFound = 'Función "%s" no encontrada';
  SInternalError = 'Error interno';
  SSyntaxErrorNear = 'Error de sintaxis cerca de "%s"';
  SSyntaxError = 'Error de sintaxis';
  SUnknownSymbol = 'Símbolo "%s" desconocido';
  SUnexpectedExprEnd = 'Fin de expresión inesperado';
  SRightBraceExpected = ') esperado';
  SParametersError = 'Se esperaban %d parámetros pero solo %d fueron encontrados';
  SExpectedMoreParams = 'Se esperaban más de dos parámetros';
  SInvalidVarByteArray = 'Arreglo VarByte inválido';
  SVariableAlreadyExists = 'La variable "%s" ya existe';
  STypesMismatch = 'Los Tipos no coinciden';
  SUnsupportedVariantType = 'Tipo de Variant no soportando';
  SUnsupportedOperation = 'Operación no soportada';

  STokenizerIsNotDefined = 'El objeto Tokenizer no está definido';
  SLibraryNotFound = 'Ninguna librería dinámica de la lista %s fue encontrada';
  SEncodeDateIsNotSupported = 'Esta versión no soporta isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Esta versión no soporta isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Esta versión no soporta isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Esta versión no soporta isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Esta versión no soporta isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Esta versión no soporta isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'No se pueden obtener datos del Resultset';
  SRowBufferIsNotAssigned = 'Buffer de línea no asignado';
  SColumnIsNotAccessable = 'La columna con índice %d no está accesible';
  SConvertionIsNotPossible = 'La conversión no es posible para la columna %d de %s a %s';
  SCanNotAccessBlobRecord = 'No se puede accesar al registro del blob en la columna %d con tipo %s';
  SRowDataIsNotAvailable = 'Datos de línea no disponibles';
  SResolverIsNotSpecified = 'El objeto Resolver no está especificado para este ResultSet';
  SResultsetIsAlreadyOpened = 'El Resultset ya está abierto';
  SCanNotUpdateEmptyRow = 'No se puede actualizar una línea vacía';
  SCanNotUpdateDeletedRow = 'No se puede actualizar una línea borrada';
  SCanNotDeleteEmptyRow = 'No se puede borrar una línea vacía';
  SCannotUseCommit = 'No se puede usar COMMIT en modo AUTOCOMMIT';
  SCannotUseRollBack = 'No se puede usar ROLLBACK en modo AUTOCOMMIT';
  SCanNotUpdateComplexQuery = 'No se puede actualizar una consulta compleja que haga referencia a más de una tabla';
  SCanNotUpdateThisQueryType = 'No se puede actualizar este tipo de consulta';
  SDriverWasNotFound = 'No se encontró el controlador de base de datos solicitado';
  SCanNotConnectToServer = 'No puede conectarse al servidor SQL';
  STableIsNotSpecified = 'La Tabla no está especificada';
  SLiveResultSetsAreNotSupported = 'La consulta actualizable no es soportada por esta clase';
  SInvalidInputParameterCount = 'El número de parámetros de tipo Input es menor al esperado';
  SIsolationIsNotSupported = 'Nivel de aislamiento de transacción no soportado';
  SColumnWasNotFound = 'Columna con nombre "%s" no encontrada';
  SWrongTypeForBlobParameter = 'Tipo incorrecto para el parámetro Blob';
  SIncorrectConnectionURL = 'URL de conexión incorrecta: %s';
  SUnsupportedProtocol = 'Protocolo no soportado: %s';

  SConnectionIsNotOpened = 'La conexión no ha sido abierta todavía';
  SInvalidOpInAutoCommit = 'Operación inválida en modo AutoCommit';
  SInvalidOpInNonAutoCommit = 'Operación inválida en modo No-AutoCommit';

  SConnectionIsNotAssigned = 'El componente de conexión a base de datos no está asigando';
  SQueryIsEmpty = 'La Consulta SQL está vacía';
  SCanNotExecuteMoreQueries = 'No se puede ejecutar más de una consulta';
  SOperationIsNotAllowed1 = 'Operación no permitida en modo FORWARD ONLY';
  SOperationIsNotAllowed2 = 'Operación no permitida en modo READ ONLY (Solo lectura)';
  SOperationIsNotAllowed3 = 'Operación no permitida en modo %s';
  SOperationIsNotAllowed4 = 'Operación no permitida en un dataset cerrado';
  SNoMoreRecords = 'No hay más registros en el Resultset';
  SCanNotOpenResultSet = 'No se puede abrir el Resultset';
  SCircularLink = 'Datasource hace una referencia cíclica';
  SBookmarkWasNotFound = 'Bookmark no encontrado';
  SIncorrectSearchFieldsNumber = 'Número incorrecto de valores de búsqueda';
  SInvalidOperationInTrans = 'Operación inválida en modo de transacción explícita';
  SIncorrectSymbol = 'Símbolo incorrecto en la lista de campos "%s".';
  SIncorrectToken = 'Token incorrecto seguido de ":"';

  SSelectedTransactionIsolation = 'El Nivel seleccionado de aislamiento de transacción no está soportado';
  SDriverNotSupported = 'Controlador %s no soportado';
  SPattern2Long = 'Patrón de búsqueda demasiado largo';
  SDriverNotCapableOutParameters = 'El controlador no tiene cualidades para manejar parámetros';
  SStatementIsNotAllowed = 'Sentencia no permitida';
  SStoredProcIsNotAllowed = 'El procedimiento alamacenado no está permitido';
  SCannotPerformOperation = 'No se puede efectuar la operación en un resultset cerrado';
  SInvalidState = 'Estado Inválido';
  SErrorConvertion = 'Error de conversión';
  SDataTypeDoesNotSupported = 'Tipo de datos no soportado';
  SUnsupportedParameterType = 'Tipo de parámetro no soportado';
  SUnsupportedDataType = 'Tipo de datos no soportado';
  SErrorConvertionField = 'Error de conversión del campo "%s" al Tipo SQL "%s"';
  SBadOCI = 'Versión de OCI [%s] no aceptable. Se requiere versión 8.0.3 o menor';
  SConnect2AsUser = 'Conectando a "%s" como usuario "%s"';
  SUnknownError = 'Error desconocido';
  SFieldNotFound1 = 'Campo "%s" no encontrado';
  SFieldNotFound2 = 'Campo %d no encontrado';

  SLoginPromptFailure = 'Cuadro de Diálogo por omisión para autenticación no encontrado.'+#10#13+
                        'Por favor agregue la unidad DBLogDlg a la sección uses de la unidad principal de su proyecto.';

  SPropertyQuery = '¡La Consulta puede tardar un poco en bases de datos extensas!';
  SPropertyTables = 'Debería limitarlas mediante Catalog y/o Schema.';
  SPropertyColumns = 'Debería limitarlas mediante Catalog, Schema y/o TableName.';
  SPropertyProcedures = 'Debería limitarlos mediante Catalog y/or Schema.';
  SPropertySequences = 'Debería limitarlos mediante Catalog y/or Schema.';
  SPropertyExecute = '¿Desea ejecutar la consulta de todos modos?';

  SFormTest = 'Prueba del Editor ZEOS SQL';
  SButtonClose = '&Cerrar';
  SFormEditor = 'Editor ZEOS SQL';
  STabSheetSelect = 'Seleccionar SQL';
  SMenuLoad = 'Cargar...';
  SMenuSave = 'Guardar...';
  SButtonGenerate = '&Generar';
  SButtonCheck = 'C&hecar';
  SButtonTest = 'Pro&bar';
  SButtonOk = '&Aceptar';
  SButtonCancel = '&Cancelar';
  STableAlias = 'A&lias de la tabla';
  SReplaceSQL = '&Reemplazar SQL';
  SDialogOpenTitle = 'Abrir archivo SQL';
  SDialogSaveTitle = 'Guardar archivo SQL';
  SSQLEditor = 'Editor SQL';
  SDatabaseDialog = 'Abrir base de datos existente';

  {$IFDEF FPC}
  SNotEditing = 'El Dataset no se encuentra en modo de edición o inserción';
  SFieldTypeMismatch = 'El Tipo de dato no coincide para el campo ''%s'', se espera: %s, actual: %s';
  SFieldSizeMismatch = 'El Tamaño de dato no coincide para el campo ''%s'', se espera: %d, actual: %d';
  {$ENDIF}

{$ELSE} // default: ENGLISH

  SSQLError1 = 'SQL Error: %s';
  SSQLError2 = 'SQL Error: %s Code: %d';
  SSQLError3 = 'SQL Error: %s Code: %d SQL: %s';
  SSQLError4 = 'SQL Error: %s Code: %d Message: %s';

  SListCapacityError = 'List capacity out of bounds (%d)';
  SListCountError = 'List count out of bounds (%d)';
  SListIndexError = 'List index out of bounds (%d)';

  SClonningIsNotSupported = 'Clonning is not supported by this class';
  SImmutableOpIsNotAllowed = 'The operation is not allowed on not changeable collections';
  SStackIsEmpty = 'Stack is empty';
  SVariableWasNotFound = 'Variable "%s" was not found';
  SFunctionWasNotFound = 'Function "%s" was not found';
  SInternalError = 'Internal error';
  SSyntaxErrorNear = 'Syntax error near "%s"';
  SSyntaxError = 'Syntax error';
  SUnknownSymbol = 'Unknown symbol "%s"';
  SUnexpectedExprEnd = 'Unexpected end of expression';
  SRightBraceExpected = ') expected';
  SParametersError = '%d parameters were expected but %d were found';
  SExpectedMoreParams = 'More than two parameters are expected';
  SInvalidVarByteArray = 'Invalid VarByte array';
  SVariableAlreadyExists = 'Variable "%s" already exists';
  STypesMismatch = 'Types mismatch';
  SUnsupportedVariantType = 'Unsupported variant type';
  SUnsupportedOperation = 'Unsupported operation';

  STokenizerIsNotDefined = 'Tokenizer is not defined';
  SLibraryNotFound = 'None of the dynamic libraries can be found: %s';
  SEncodeDateIsNotSupported = 'This version does not support isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'This version does not support isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'This version does not support isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'This version does not support isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'This version does not support isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'This version does not support isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'Cannot retrieve Resultset data';
  SRowBufferIsNotAssigned = 'Row buffer is not assigned';
  SColumnIsNotAccessable = 'Column with index %d is not accessable';
  SConvertionIsNotPossible = 'Convertion is not possible for column %d from %s to %s';
  SCanNotAccessBlobRecord = 'Cannot access blob record in column %d with type %s';
  SRowDataIsNotAvailable = 'Row data is not available';
  SResolverIsNotSpecified = 'Resolver is not specified for this ResultSet';
  SResultsetIsAlreadyOpened = 'Resultset is already open';
  SCanNotUpdateEmptyRow = 'Cannot update an empty row';
  SCanNotUpdateDeletedRow = 'Cannot update a deleted row';
  SCanNotDeleteEmptyRow = 'Cannot delete an empty row';
  SCannotUseCommit = 'You cannot use COMMIT in AUTOCOMMIT mode';
  SCannotUseRollBack = 'You cannot use ROLLBACK in AUTOCOMMIT mode';
  SCanNotUpdateComplexQuery = 'Cannot update a complex query with more then one table';
  SCanNotUpdateThisQueryType = 'Cannot update this query type';
  SDriverWasNotFound = 'Requested database driver was not found';
  SCanNotConnectToServer = 'Cannot connect to SQL server';
  STableIsNotSpecified = 'Table is not specified';
  SLiveResultSetsAreNotSupported = 'Live query is not supported by this class';
  SInvalidInputParameterCount = 'Input parameter count is less then expected';
  SIsolationIsNotSupported = 'Transaction isolation level is not supported';
  SColumnWasNotFound = 'Column with name "%s" was not found';
  SWrongTypeForBlobParameter = 'Wrong type for Blob parameter';
  SIncorrectConnectionURL = 'Incorrect connection URL: %s';
  SUnsupportedProtocol = 'Unsupported protocol: %s';

  SConnectionIsNotOpened = 'Connection is not opened yet';
  SInvalidOpInAutoCommit = 'Invalid operation in AutoCommit mode';
  SInvalidOpInNonAutoCommit = 'Invalid operation in non AutoCommit mode';

  SConnectionIsNotAssigned = 'Database connection component is not assigned';
  SQueryIsEmpty = 'SQL Query is empty';
  SCanNotExecuteMoreQueries = 'Cannot execute more then one query';
  SOperationIsNotAllowed1 = 'Operation is not allowed in FORWARD ONLY mode';
  SOperationIsNotAllowed2 = 'Operation is not allowed in READ ONLY mode';
  SOperationIsNotAllowed3 = 'Operation is not allowed in %s mode';
  SOperationIsNotAllowed4 = 'Operation is not allowed for closed dataset';
  SNoMoreRecords = 'No more records in the Resultset';
  SCanNotOpenResultSet = 'Can not open a Resultset';
  SCircularLink = 'Datasource makes a circular link';
  SBookmarkWasNotFound = 'Bookmark was not found';
  SIncorrectSearchFieldsNumber = 'Incorrect number of search field values';
  SInvalidOperationInTrans = 'Invalid operation in explicit transaction mode';
  SIncorrectSymbol = 'Incorrect symbol in field list "%s".';
  SIncorrectToken = 'Incorrect token followed by ":"';

  SSelectedTransactionIsolation = 'Selected transaction isolation level is not supported';
  SDriverNotSupported = 'Driver not supported %s';
  SPattern2Long = 'Pattern is too long';
  SDriverNotCapableOutParameters = 'Driver is not capable to handle parameters';
  SStatementIsNotAllowed = 'Statement is not allowed';
  SStoredProcIsNotAllowed = 'The stored proc is not allowed';
  SCannotPerformOperation = 'Can not perform operation on closed Resultset';
  SInvalidState = 'Invalid state';
  SErrorConvertion = 'Convertion error';
  SDataTypeDoesNotSupported = 'Data type is not supported';
  SUnsupportedParameterType = 'Unsupported parameter type';
  SUnsupportedDataType = 'Unsupported data type';
  SErrorConvertionField = 'Conversion error for field "%s" to SQLType "%s"';
  SBadOCI = 'Bad OCI version [%s]. Version 8.0.3 or older is required';
  SConnect2AsUser = 'Connect to "%s" as user "%s"';
  SUnknownError = 'Unknown error';
  SFieldNotFound1 = 'Field "%s" was not found';
  SFieldNotFound2 = 'Field %d was not found';

  SLoginPromptFailure = 'Can not find default login prompt dialog. Please add DBLogDlg to the uses section of your main file.';

  SPropertyQuery = 'The Query may last a while on large databases!';
  SPropertyTables = 'You should limit it by Catalog and/or Schema.';
  SPropertyColumns = 'You should limit it by Catalog, Schema and/or TableName.';
  SPropertyProcedures = 'You should limit it by Catalog and/or Schema.';
  SPropertySequences = 'You should limit it by Catalog and/or Schema.';
  SPropertyExecute = 'Should the Query be executed anyway?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '&Close';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'Select SQL';
  SMenuLoad = 'Load';
  SMenuSave = 'Save';
  SButtonGenerate = '&Generate';
  SButtonCheck = 'C&heck';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = '&Cancel';
  STableAlias = 'T&able alias';
  SReplaceSQL = '&Replace SQL';
  SDialogOpenTitle = 'Open SQL File';
  SDialogSaveTitle = 'Save SQL File';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Open existing database';

  {$IFDEF FPC}
  SNotEditing = 'Dataset not in edit or insert mode';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  {$ENDIF}

{$ENDIF} //SPANISH

{$ENDIF} // GERMAN

{$ENDIF} // DUTCH

{$ENDIF} // PORTUGUESE

implementation

end.
