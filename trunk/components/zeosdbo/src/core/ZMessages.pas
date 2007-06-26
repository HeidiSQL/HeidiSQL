{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                                                         }
{ This unit contains all the messages that are output by  }
{ ZEOS methods. One of the given language can be activated}
{ by setting the language in ->                           }
{ ZEOS.inc (e.g.: $DEFINE GERMAN).                        }
{ If no language is defined english will be used.         }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

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
  SUnsupportedOperation = 'Operação não suportada';

  STokenizerIsNotDefined = 'Sinalizador não definido';
  SLibraryNotFound = 'Nenhuma biblioteca dinâmica da lista %s foi encontrada';
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
  SUnsupportedByDriver    = 'Translate: Driver can not support this feature natively: [%s]';

  SConnectionIsNotOpened = 'Conexão ainda não está aberta.';
  SInvalidOpInAutoCommit = 'Operação inválida no modo AutoCommit.';
  SInvalidOpInNonAutoCommit = 'Operação inválida quando o modo AutoCommit é False.';
  SInvalidOpPrepare = 'Prepare transaction somente é possível após comandar Starttransaction';

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

  SPropertyQuery = 'A Query poderá demorar em bancos de dados volumosos!';
  SPropertyTables = 'Você deveria limitar por Catalogo e/ou Esquema.';
  SPropertyColumns = 'Você deveria limitar por Catalogo, Esquema e/ou Tabela.';
  SPropertyProcedures = 'Você deveria limitar por Catalogo e/ou Esquema.';
  SPropertySequences = 'Você deveria limitar por Catalogo e/ou Esquema..';
  SPropertyExecute = 'Executar a Query de qualquer maneira?';

  SFormTest = 'Teste Editor ZEOS SQL';
  SButtonClose = '&Fechar';
  SFormEditor = 'Editor ZEOS SQL';
  STabSheetSelect = 'SQL Select';
  SMenuLoad = 'Carregar';
  SMenuSave = 'Salvar';
  SButtonGenerate = '&Gerar';
  SButtonCheck = '&Verificar';
  SButtonTest = '&Testar';
  SButtonOk = '&OK';
  SButtonCancel = '&Cancelar';
  STableAlias = '&Alias Tabela';
  SReplaceSQL = '&Substituir SQL';
  SDialogOpenTitle = 'Abrir Arquivo SQL';
  SDialogSaveTitle = 'Salvar Arquivo SQL';
  SSQLEditor = 'Editor SQL';
  SDatabaseDialog = 'Abrir Banco de Dados existente';

  SUpdateSQLNoResult = 'SQL Update Refresh resultou num conjunto vazio';
  SUpdateSQLRefreshStatementcount ='Usar somente 1 declaraçao SQL para Update Refresh';
  {$IFDEF FPC}
  SNotEditing = 'Dataset não está em modo de edição ou inserção';
  SFieldTypeMismatch = 'Tipo inválido para o campo ''%s'', esperado: %s atual: %s';
  SFieldSizeMismatch = 'Tamanho Inválido para o campo ''%s'', esperado: %d atual: %d';
  {$ENDIF}

  SFailedtoInitPrepStmt   = 'A declaração preparada falhou ao inicializar'; 
  SFailedtoPrepareStmt    = 'A declaração falhou durante o processo de preparo'; 
  SFailedToBindAllValues  = 'A Aplicação falhou na tradução de todos os valores'; 
  SAttemptExecOnBadPrep   = 'Tentativa de executar uma declaração que não foi corretamente preparada'; 
  SBindingFailure         = 'Falha ao traduzir o conjunto de parâmetros'; 
  SPreparedStmtExecFailure = 'A declaração preparada falhou ao executar'; 
  SBoundVarStrIndexMissing = 'Índice de texto "%s" da variável de limite não existe'; 
  SBindVarOutOfRange      = 'Índice da variável de limite fora de alcance: %d';

  SRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  SMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  SUnKnownParamDataType = 'TRANSLATE: Unknown Param.DataType';

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
  SEncodeDateIsNotSupported = 'Deze versie ondersteunt isc_encode_sql_date niet';
  SEncodeTimeIsNotSupported = 'Deze versie ondersteunt isc_encode_sql_time niet';
  SEncodeTimestampIsNotSupported = 'Deze versie ondersteunt isc_encode_sql_timestamp niet';
  SDecodeDateIsNotSupported = 'Deze versie ondersteunt isc_decode_sql_date niet';
  SDecodeTimeIsNotSupported = 'Deze versie ondersteunt isc_decode_sql_time niet';
  SDecodeTimestampIsNotSupported = 'Deze versie ondersteunt isc_decode_sql_timestamp niet';

  SCanNotRetrieveResultSetData = 'Kan ResultSet data niet ophalen';
  SRowBufferIsNotAssigned = 'Row buffer is niet toegekend';
  SColumnIsNotAccessable = 'Kolom met index %d is niet bereikbaar';
  SConvertionIsNotPossible = 'Conversie is niet mogelijk voor kolom %d van %s tot %s';
  SCanNotAccessBlobRecord = 'Kan het blob record in kolom %d met type %s niet benaderen';
  SRowDataIsNotAvailable = 'Rij data is niet beschikbaar';
  SResolverIsNotSpecified = 'Resolver is niet gespecificeerd voor deze ResultSet';
  SResultsetIsAlreadyOpened = 'ResultSet is al geopend';
  SCanNotUpdateEmptyRow = 'Kan een lege rij niet updaten';
  SCanNotUpdateDeletedRow = 'Kan een verwijderde rij niet updaten';
  SCanNotDeleteEmptyRow = 'Kan een lege rij niet verwijderen';
  SCannotUseCommit = 'Commit in autocommit mode is niet mogelijk';
  SCannotUseRollBack = 'Rollback in autocommit mode is niet mogelijk';
  SCanNotUpdateComplexQuery = 'Kan een complexe query met meerdere tabellen niet updaten';
  SCanNotUpdateThisQueryType = 'Kan dit query type niet updaten';
  SDriverWasNotFound = 'Gevraagde database driver is niet gevonden';
  SCanNotConnectToServer = 'Kan geen verbinding maken met de SQL server';
  STableIsNotSpecified = 'Tabel is niet gespecifieerd';
  SLiveResultSetsAreNotSupported = 'Live query is niet ondersteund door deze klasse';
  SInvalidInputParameterCount = 'Input parameter aantal is lager dan verwacht';
  SIsolationIsNotSupported = 'Transactie isolatie niveau wordt niet ondersteund';
  SColumnWasNotFound = 'Kolom met naam "%s" bestaat niet';
  SWrongTypeForBlobParameter = 'Verkeerde type voor Blob parameter';
  SIncorrectConnectionURL = 'Ongeldige connectie URL: %s';
  SUnsupportedProtocol = 'Niet ondersteund protocol: %s';
  SUnsupportedByDriver    = 'De driver ondersteunt deze functie niet: [%s]';

  SConnectionIsNotOpened = 'Verbinding is niet gemaakt.';
  SInvalidOpInAutoCommit = 'Ongeldige operatie in AutoCommit mode.';
  SInvalidOpInNonAutoCommit = 'Ongeldige operatie in non AutoCommit mode.';
  SInvalidOpPrepare = 'Transactie voorbereiden is enkel mogelijk bij de eerste aanroep van Starttransaction!';

  SConnectionIsNotAssigned = 'Database connectie component is niet toegekend';
  SQueryIsEmpty = 'SQL Query is leeg';
  SCanNotExecuteMoreQueries = 'Kan niet meerdere queries uitvoeren';
  SOperationIsNotAllowed1 = 'Bewerking is niet toegestaan in FORWARD ONLY mode';
  SOperationIsNotAllowed2 = 'Bewerking is niet toegestaan in READ ONLY mode';
  SOperationIsNotAllowed3 = 'Bewerking is niet toegestaan in %s mode';
  SOperationIsNotAllowed4 = 'Bewerking is niet toegestaan voor gesloten dataset';
  SNoMoreRecords = 'Geen records meer aanwezig in ResultSet';
  SCanNotOpenResultSet = 'Kan een ResultSet niet openen';
  SCircularLink = 'Databron maakt een oneindige verbindingslus';
  SBookmarkWasNotFound = 'Bookmark niet gevonden';
  SIncorrectSearchFieldsNumber = 'Incorrect aantal zoekvelden';
  SInvalidOperationInTrans = 'Ongeldige operatie in explicit transaction mode';
  SIncorrectSymbol = 'Ongeldig symbool in veld lijst "%s".';
  SIncorrectToken = 'Ongeldig teken gevolgd door ":"';

  SSelectedTransactionIsolation = 'Geselecteerd transactie isolatie niveau niet ondersteund';
  SDriverNotSupported = 'Driver niet ondersteund %s';
  SPattern2Long = 'Patroon is te lang';
  SDriverNotCapableOutParameters = 'Driver ondersteunt geen out parameters';
  SStatementIsNotAllowed = 'Statement is niet toegestaan';
  SStoredProcIsNotAllowed = 'Stored procedures zijn niet toegestaan';
  SCannotPerformOperation = 'Kan operatie niet uitvoeren op een gesloten ResultSet';
  SInvalidState = 'Ongeldige status';
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

  SLoginPromptFailure = 'Kan de standaard login prompt niet vinden.  Voeg DBLogDlg toe aan de uses sectie.';

  SPropertyQuery = 'De Query kan enige tijd duren bij grote databases!';
  SPropertyTables = 'Limiet op Catalog en/of Schema is vereist.';
  SPropertyColumns = 'Limiet op Catalog, Schema en/of tablenaam is vereist.';
  SPropertyProcedures = 'Limiet op Catalog en/of Schema is vereist.';
  SPropertySequences = 'Limiet op Catalog en/of Schema is vereist.';
  SPropertyExecute = 'Dient de Query toch te worden uitgevoerd?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '&Sluiten';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'Select SQL';
  SMenuLoad = 'Laden';
  SMenuSave = 'Opslaan';
  SButtonGenerate = '&Genereren';
  SButtonCheck = 'C&heck';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = '&Annuleren';
  STableAlias = 'Tabel al&ias';
  SReplaceSQL = '&Vervang SQL';
  SDialogOpenTitle = 'SQL Bestand Openen';
  SDialogSaveTitle = 'SQL Bestand Opslaan';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Open bestaande database';

  SUpdateSQLNoResult = 'Der zuvor aktualisierte SQL liefert kein Resultset zurück';
  SUpdateSQLRefreshStatementcount ='Update Refresh SQL Statement count moet 1 zijn';

  {$IFDEF FPC}
  SNotEditing = 'Dataset is niet in edit of insert modus';
  SFieldTypeMismatch = 'Type mismatch voor veld ''%s'', verwacht: %s actueel: %s';
  SFieldSizeMismatch = 'Size mismatch voor veld ''%s'', verwacht: %d actueel: %d';
  {$ENDIF}

  SFailedtoInitPrepStmt   = 'Initialisatie van Prepared statement mislukt';
  SFailedtoPrepareStmt    = 'Statement mislukt tijdens prepare';
  SFailedToBindAllValues  = 'Pre-bind van allewaarden is mislukt';
  SAttemptExecOnBadPrep   = 'Poging om een statement uit te voeren voor een succesvolle prepare';
  SBindingFailure         = 'Binding van parameterset mislukt';
  SPreparedStmtExecFailure = 'Uitvoeren van Prepared statement mislukt';
  SBoundVarStrIndexMissing = 'Tekst index van bound variable bestaat niet: "%s"';
  SBindVarOutOfRange      = 'Bound variable index buiten bereik: %d';

  SRefreshRowOnlySupportedWithUpdateObject = 'De refreshrow methode is enkel ondersteund vooreen update object';
  SMustBeInBrowseMode = 'Bewerking is enkel toegestaan in dsBROWSE status';

  SUnKnownParamDataType = 'Param.DataType is onbekend';

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
  SUnsupportedByDriver    = 'Der Treiber unterstützt dieses Feature nicht von haus aus: [%s]';

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
  SInvalidOpPrepare = 'Transaktion vorzubereiten ist nur beim ersten Aufruf von Starttransaction möglich!';

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

  SUpdateSQLNoResult = 'Translate : Update Refresh SQL delivered no resultset';
  SUpdateSQLRefreshStatementcount ='Translate : Update Refresh SQL Statement count must be 1';

  {$IFDEF FPC}
  SNotEditing = 'Das DataSet ist nicht im "edit" oder "insert" Modus.';
  SFieldTypeMismatch = 'Der Typ für Feld ''%s'' stimmt nicht. Erwartet wird %s der Typ ist aber momentan %s';
  SFieldSizeMismatch = 'Die Größe des Feldes ''%s'' stimmt nicht. Erwartet wird  %d die Größe ist aber momentan %d';
  {$ENDIF}
  // <- ms, 03/05/2005

  SFailedtoInitPrepStmt   = 'Translate: Prepared statement failed to initialize';
  SFailedtoPrepareStmt    = 'Translate: Statement failed during prepare process';
  SFailedToBindAllValues  = 'Translate: Application failed to pre-bind all values';
  SAttemptExecOnBadPrep   = 'Translate: Attempt made to execute a statement before a successful preparation.';
  SBindingFailure         = 'Translate: Failed to bind parameter set';
  SPreparedStmtExecFailure = 'Translate: Prepared statement failed to execute';
  SBoundVarStrIndexMissing = 'Translate: Bound variable text index "%s" does not exist';
  SBindVarOutOfRange      = 'Translate: Bound variable index out of range: %d';

  SRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  SMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  SUnKnownParamDataType = 'TRANSLATE: Unknown Param.DataType';

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
  SUnsupportedByDriver    = 'Translate: Driver can not support this feature natively: [%s]';

  SConnectionIsNotOpened = 'La conexión no ha sido abierta todavía';
  SInvalidOpInAutoCommit = 'Operación inválida en modo AutoCommit';
  SInvalidOpInNonAutoCommit = 'Operación inválida en modo No-AutoCommit';
  SInvalidOpPrepare = 'Translate : Prepare transaction only possible on matching first(!) Starttransaction';

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

  SUpdateSQLNoResult = 'Translate : Update Refresh SQL delivered no resultset';
  SUpdateSQLRefreshStatementcount ='Translate : Update Refresh SQL Statement count must be 1';

  {$IFDEF FPC}
  SNotEditing = 'El Dataset no se encuentra en modo de edición o inserción';
  SFieldTypeMismatch = 'El Tipo de dato no coincide para el campo ''%s'', se espera: %s, actual: %s';
  SFieldSizeMismatch = 'El Tamaño de dato no coincide para el campo ''%s'', se espera: %d, actual: %d';
  {$ENDIF}

  SFailedtoInitPrepStmt   = 'Translate: Prepared statement failed to initialize';
  SFailedtoPrepareStmt    = 'Translate: Statement failed during prepare process';
  SFailedToBindAllValues  = 'Translate: Application failed to pre-bind all values';
  SAttemptExecOnBadPrep   = 'Translate: Attempt made to execute a statement before a successful preparation.';
  SBindingFailure         = 'Translate: Failed to bind parameter set';
  SPreparedStmtExecFailure = 'Translate: Prepared statement failed to execute';
  SBoundVarStrIndexMissing = 'Translate: Bound variable text index "%s" does not exist';
  SBindVarOutOfRange      = 'Translate: Bound variable index out of range: %d';

  SRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  SMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  SUnKnownParamDataType = 'TRANSLATE: Unknown Param.DataType';

{$ELSE}

{$IFDEF ROMANA}


 SSQLError1 = 'SQL Eroare: %s';
  SSQLError2 = 'SQL Eroare: %s Cod: %d';
  SSQLError3 = 'SQL Eroare: %s Cod: %d SQL: %s';
  SSQLError4 = 'SQL Eroare: %s Cod: %d Mesaj: %s';

  SListCapacityError = 'Capacitatea listei este în afara limitelor (%d)';
  SListCountError = 'Contorul listei este în afara limitelor (%d)';
  SListIndexError = 'Indexul listei este în afara limitelor (%d)';

  SClonningIsNotSupported = 'Clonning nu este suportat de aceastã clasã';
  SImmutableOpIsNotAllowed = 'Operaþia nu este permisã ori colecþia nu este modificabilã';
  SStackIsEmpty = 'Stiva este goalã';
  SVariableWasNotFound = 'Variabila "%s" nu a fost gãsitã';
  SFunctionWasNotFound = 'Funcþia "%s" nu a fost gãsitã';
  SInternalError = 'Eroare Internã';
  SSyntaxErrorNear = 'Eroare de sintaxã lângã "%s"';
  SSyntaxError = 'Eroare de sintaxã';
  SUnknownSymbol = 'Simbol necunoscut "%s"';
  SUnexpectedExprEnd = 'Final neaºteptat pentru expresie';
  SRightBraceExpected = ') aºteptat';
  SParametersError = 'parametrul %d a fost aºteptat dar %d a fost gãsit';
  SExpectedMoreParams = 'Mai nult de doi parametrii sunt aºteptaþi';
  SInvalidVarByteArray = 'Arie VarByte invalidã';
  SVariableAlreadyExists = 'Variabila "%s" deja existã';
  STypesMismatch = 'Tip nepotrivit';
  SUnsupportedVariantType = 'Tip variant neasteptat';
  SUnsupportedOperation = 'Operaþie nesuportatã';

  STokenizerIsNotDefined = 'Simbolistica nu este definitã';
  SLibraryNotFound = 'None of the dynamic libraries can be found: %s';
  SEncodeDateIsNotSupported = 'Aceastã versiune nu suportã isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Aceastã versiune nu suportã isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Aceastã versiune nu suportã isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Aceastã versiune nu suportã isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Aceastã versiune nu suportã isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Aceastã versiune nu suportã isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'Nu pot returna  Resultset data';
  SRowBufferIsNotAssigned = 'Row buffer nu este asignat';
  SColumnIsNotAccessable = 'Column with index %d nu este accesibil';
  SConvertionIsNotPossible = 'Conversia nu este posibilã pentru coloana %d din %s în %s';
  SCanNotAccessBlobRecord = 'Nu pot aceesa înregistrarea blob în coloana %d cu tipul %s';
  SRowDataIsNotAvailable = 'Row data nu este disponibil';
  SResolverIsNotSpecified = 'Resolver nu este specificat pentru acest ResultSet';
  SResultsetIsAlreadyOpened = 'Resultset este deja deschisã';
  SCanNotUpdateEmptyRow = 'Nu pot updata o înregistrare goalã';
  SCanNotUpdateDeletedRow = 'Nu pot updata o înregistrare ºtearsã';
  SCanNotDeleteEmptyRow = 'Nu pot ºterge o înregistrare goalã';
  SCannotUseCommit = 'Nu poþi folosi COMMIT în modul AUTOCOMMIT ';
  SCannotUseRollBack = 'Nu poþi folosi ROLLBACK în modul AUTOCOMMIT ';
  SCanNotUpdateComplexQuery = 'Nu pot updata un query complex cu mai mult de un tabel';
  SCanNotUpdateThisQueryType = 'Nu pot updata acest tip de query';
  SDriverWasNotFound = 'Driverul pentru baza de date nu a fost gãsit';
  SCanNotConnectToServer = 'Nu ma pot conecta la serverul SQL';
  STableIsNotSpecified = 'Tbelul nu este specificat';
  SLiveResultSetsAreNotSupported = 'Live query is not supported by this class';
  SInvalidInputParameterCount = 'Input parameter count is less then expected';
  SIsolationIsNotSupported = 'Transaction isolation level nu este suportat';
  SColumnWasNotFound = 'Coloana cu numele "%s" nu a fost fãsitã';
  SWrongTypeForBlobParameter = 'Tip greºit pentru parametru Blob';
  SIncorrectConnectionURL = 'Conexiune URL incorectã: %s';
  SUnsupportedProtocol = 'Protocol nesuportat: %s';
  SUnsupportedByDriver    = 'Driver nu poate suporta aceastã facilitate : [%s]';

  SConnectionIsNotOpened = 'Conexiune nu este deschisã incã';
  SInvalidOpInAutoCommit = 'Operaþie invalidã în modul AutoCommit';
  SInvalidOpInNonAutoCommit = 'Operaþie invalidã în modul non AutoCommit ';
  SInvalidOpPrepare = 'Prepare transaction only possible on matching first(!) Starttransaction';

  SConnectionIsNotAssigned = 'Nu este asignatã o componentã Database connection';
  SQueryIsEmpty = 'SQL Query este gol';
  SCanNotExecuteMoreQueries = 'Nu pot executa mai mult de un query';
  SOperationIsNotAllowed1 = 'Operaþia nu este permisã în modul FORWARD ONLY ';
  SOperationIsNotAllowed2 = 'Operaþia nu este permisã în modul READ ONLY';
  SOperationIsNotAllowed3 = 'Operaþia nu este permisã în modul %s ';
  SOperationIsNotAllowed4 = 'Operaþia nu este permisã pentru în dataset închis';
  SNoMoreRecords = 'Nu mai sunt înregistrãri în Resultset';
  SCanNotOpenResultSet = 'Nu pot deschide Resultset';
  SCircularLink = 'Datasource makes a circular link';
  SBookmarkWasNotFound = 'Bookmark nu a fost gãsit';
  SIncorrectSearchFieldsNumber = 'Numãr incorect of search field values';
  SInvalidOperationInTrans = 'Operaþie invalidã în modul explicit transaction';
  SIncorrectSymbol = 'Simbol incorect în lista de câmpuri  "%s".';
  SIncorrectToken = 'Incorect token dupã ":"';

  SSelectedTransactionIsolation = 'Selected transaction isolation level is not supported';
  SDriverNotSupported = 'Driver nesuportat %s';
  SPattern2Long = 'Pattern is too long';
  SDriverNotCapableOutParameters = 'Driver nu este capabil sã mânuie parametrii';
  SStatementIsNotAllowed = 'Statement nu sunt permise';
  SStoredProcIsNotAllowed = 'The stored proc nu sunt permise';
  SCannotPerformOperation = 'Nu se pot face operaþii cu Resultset închis';
  SInvalidState = 'Stare invalidã';
  SErrorConvertion = 'Eroare de conversie';
  SDataTypeDoesNotSupported = 'Tip de datã nesuportat';
  SUnsupportedParameterType = 'Tip parametru nesuportat';
  SUnsupportedDataType = 'Tip datã nesuportat';
  SErrorConvertionField = 'Eroare de conversie pentru câmpul "%s" în TipSQL "%s"';
  SBadOCI = 'Bad OCI version [%s]. Version 8.0.3 or older is required';
  SConnect2AsUser = 'Conectare la "%s" ca utlizator "%s"';
  SUnknownError = 'Eroare necunoscutã';
  SFieldNotFound1 = 'Câmpul "%s" nu a fost gãsit';
  SFieldNotFound2 = 'Câmpul %d nu a fost gãsit';

  SLoginPromptFailure = 'Nu gãsesc fereastra de dialog implicitã pentru login. Vã rog adãugaþi DBLogDlg în secþiunea uses.';

  SPropertyQuery = 'The Query may last a while on large databases!';
  SPropertyTables = 'You should limit it by Catalog and/or Schema.';
  SPropertyColumns = 'You should limit it by Catalog, Schema and/or TableName.';
  SPropertyProcedures = 'You should limit it by Catalog and/or Schema.';
  SPropertySequences = 'You should limit it by Catalog and/or Schema.';
  SPropertyExecute = 'Query va fi executatã oricum?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = 'În&chide';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'Select SQL';
  SMenuLoad = 'Deschide';
  SMenuSave = 'Salvare';
  SButtonGenerate = '&Generare';
  SButtonCheck = 'Verificare';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = 'Revo&care';
  STableAlias = 'T&able alias';
  SReplaceSQL = '&Replace SQL';
  SDialogOpenTitle = 'Deschide Fiºier SQL';
  SDialogSaveTitle = 'Salveazã Fiºier SQL';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Deschide bazã date existentã';

  SUpdateSQLNoResult = '"Update Refresh SQL" furnizat nu este un recordset';
  SUpdateSQLRefreshStatementcount ='Declaraþia "Update Refresh SQL" ca numãr trebuie sã fie una';

  {$IFDEF FPC}
  SNotEditing = 'Dataset nu este în modul de editare sau inserare';
  SFieldTypeMismatch = 'Tip nepotrivit pentru câmpul ''%s'', aºteptat: %s actual: %s';
  SFieldSizeMismatch = 'Dimensiune nepotrivitã pentru câmpul  ''%s'', aºteptat: %d actual: %d';
  {$ENDIF}

  SFailedtoInitPrepStmt   = 'Translate: Prepared statement failed to initialize';
  SFailedtoPrepareStmt    = 'Translate: Statement failed during prepare process';
  SFailedToBindAllValues  = 'Translate: Application failed to pre-bind all values';
  SAttemptExecOnBadPrep   = 'Translate: Attempt made to execute a statement before a successful preparation.';
  SBindingFailure         = 'Translate: Failed to bind parameter set';
  SPreparedStmtExecFailure = 'Translate: Prepared statement failed to execute';
  SBoundVarStrIndexMissing = 'Translate: Bound variable text index "%s" does not exist';
  SBindVarOutOfRange      = 'Translate: Bound variable index out of range: %d';

  SRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  SMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  SUnKnownParamDataType = 'TRANSLATE: Unknown Param.DataType';

  // <-- added by tohenk
  {$ELSE}
  {$IFDEF INDONESIAN}
  SSQLError1 = 'Kesalahan SQL: %s';
  SSQLError2 = 'Kesalahan SQL: %s Kode: %d';
  SSQLError3 = 'Kesalahan SQL: %s Kode: %d SQL: %s';
  SSQLError4 = 'Kesalahan SQL: %s Kode: %d Pesan: %s';

  SListCapacityError = 'Kapasitas List diluar jangkauan (%d)';
  SListCountError = 'Jumlah List diluar jangkauan (%d)';
  SListIndexError = 'Indeks List diluar jangkauan (%d)';

  SClonningIsNotSupported = 'Class ini tidak mendukung kloning';
  SImmutableOpIsNotAllowed = 'Operasi tidak diperkenankan pada koleksi yang tidak dapat diubah';
  SStackIsEmpty = 'Stack kosong';
  SVariableWasNotFound = 'Variabel "%s" tidak ada';
  SFunctionWasNotFound = 'Fungsi "%s" tidak ada';
  SInternalError = 'Kesalahan internal';
  SSyntaxErrorNear = 'Kesalahan Syntax di dekat "%s"';
  SSyntaxError = 'Kesalahan Syntax';
  SUnknownSymbol = 'Simbol tidak dikenali "%s"';
  SUnexpectedExprEnd = 'Tidak dibutuhkan, akhir dari ekspresi';
  SRightBraceExpected = ') dibutuhkan';
  SParametersError = '%d parameter dibutuhkan tapi terdapat %d parameter';
  SExpectedMoreParams = 'Dibutuhkan lebih dari dua parameter';
  SInvalidVarByteArray = 'array VarByte tidak valid';
  SVariableAlreadyExists = 'Variabel "%s" sudah ada';
  STypesMismatch = 'Tipe tidak sesuai';
  SUnsupportedVariantType = 'Tipe variant tidak didukung';
  SUnsupportedOperation = 'Operasi tidak didukung';

  STokenizerIsNotDefined = 'Tokenizer belum ditentukan';
  SLibraryNotFound = 'Tidak ada library ditemukan: %s';
  SEncodeDateIsNotSupported = 'Versi ini tidak mendukung isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Versi ini tidak mendukung isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Versi ini tidak mendukung isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Versi ini tidak mendukung isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Versi ini tidak mendukung isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Versi ini tidak mendukung isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'Tidak dapat mengambil data Resultset';
  SRowBufferIsNotAssigned = 'Row buffer tidak disediakan';
  SColumnIsNotAccessable = 'Kolom dengan indeks %d tidak dapat diakses';
  SConvertionIsNotPossible = 'Konversi tidak dimungkinkan pada kolom %d dari %s ke %s';
  SCanNotAccessBlobRecord = 'Tidak dapat mengakses rekord `blob` pada kolom %d dengan tipe %s';
  SRowDataIsNotAvailable = 'Data Row tidak tersedia';
  SResolverIsNotSpecified = 'Resolver belum ditentukan pada ResultSet ini';
  SResultsetIsAlreadyOpened = 'Resultset sudah terbuka';
  SCanNotUpdateEmptyRow = 'Tidak dapat meng-update row kosong';
  SCanNotUpdateDeletedRow = 'Tidak dapat meng-update row terhapus';
  SCanNotDeleteEmptyRow = 'Tidak dapat meng-hapus row kosong';
  SCannotUseCommit = 'COMMIT tidak dapat digunakan pada mode AUTOCOMMIT';
  SCannotUseRollBack = 'ROLLBACK tidak dapat digunakan pada mode AUTOCOMMIT';
  SCanNotUpdateComplexQuery = 'Tidak dapat meng-update query kompleks dengan lebih dari satu tabel';
  SCanNotUpdateThisQueryType = 'Tidak dapat meng-update query dengan tipe ini';
  SDriverWasNotFound = 'Driver database yang diminta tidak ada';
  SCanNotConnectToServer = 'Tidak dapat terhubung ke server SQL';
  STableIsNotSpecified = 'Tabel belum ditentukan';
  SLiveResultSetsAreNotSupported = 'Live query tidak didukung oleh Class ini';
  SInvalidInputParameterCount = 'Jumlah parameter Input kurang dari yang dibutuhkan';
  SIsolationIsNotSupported = 'Level Isolasi Transaksi tidak didukung';
  SColumnWasNotFound = 'Kolom dengan nama "%s" tidak ada';
  SWrongTypeForBlobParameter = 'Salah tipe untuk parameter Blob';
  SIncorrectConnectionURL = 'Salah koneksi URL: %s';
  SUnsupportedProtocol = 'Protokol tidak didukung: %s';
  SUnsupportedByDriver    = 'Driver tidak mendukung fitur: [%s]';

  SConnectionIsNotOpened = 'Koneksi belum dibuka';
  SInvalidOpInAutoCommit = 'Operasi tidak valid pada mode AUTOCOMMIT';
  SInvalidOpInNonAutoCommit = 'Operasi tidak valid pada mode non AUTOCOMMIT';
  SInvalidOpPrepare = 'Persiapan transaksi hanya mungkin pada (!) Starttransaction pertama';

  SConnectionIsNotAssigned = 'Komponen koneksi Database tidak ditentukan';
  SQueryIsEmpty = 'Query SQL kosong';
  SCanNotExecuteMoreQueries = 'Tidak dapat meng-eksekusi lebih dari satu query';
  SOperationIsNotAllowed1 = 'Operasi tidak diperkenankan pada mode FORWARD ONLY';
  SOperationIsNotAllowed2 = 'Operasi tidak diperkenankan pada mode READ ONLY';
  SOperationIsNotAllowed3 = 'Operasi tidak diperkenankan pada mode %s';
  SOperationIsNotAllowed4 = 'Operasi tidak diperkenankan pada dataset tertutup';
  SNoMoreRecords = 'Tidak ada rekord lagi pada Resultset';
  SCanNotOpenResultSet = 'Tidak dapat membuka Resultset';
  SCircularLink = 'Terjadi hubungan Datasource circular';
  SBookmarkWasNotFound = 'Bookmark tidak ada';
  SIncorrectSearchFieldsNumber = 'Salah jumlah nilai field pada pencarian';
  SInvalidOperationInTrans = 'Operasi tidak valid pada mode explicit transaction';
  SIncorrectSymbol = 'Simbol salah pada daftar field "%s".';
  SIncorrectToken = 'Token salah setelah ":"';

  SSelectedTransactionIsolation = 'Level Isolasi Transaksi terpilih tidak didukung';
  SDriverNotSupported = 'Driver tidak mendukung %s';
  SPattern2Long = 'Pola terlalu panjang';
  SDriverNotCapableOutParameters = 'Driver tidak mampu menangani parameter';
  SStatementIsNotAllowed = 'Statement tidak diperbolehkan';
  SStoredProcIsNotAllowed = 'StoredProc tidak diperbolehkan';
  SCannotPerformOperation = 'Tidak dapat melakukan operasi pada Resultset tertutup';
  SInvalidState = 'Sate tidak valid';
  SErrorConvertion = 'Kesalahan konversi';
  SDataTypeDoesNotSupported = 'Tipe Data tidak didukung';
  SUnsupportedParameterType = 'Tidak mendukung tipe parameter';
  SUnsupportedDataType = 'Tidak mendukung tipe data';
  SErrorConvertionField = 'Kesalahan konversi field "%s" ke Tipe SQL "%s"';
  SBadOCI = 'OCI version [%s] tidak sah. Dibutuhkan versi 8.0.3 atau terdahulu';
  SConnect2AsUser = 'Koneksi ke "%s" dengan user "%s"';
  SUnknownError = 'Kesalahan tidak diketahui';
  SFieldNotFound1 = 'Field "%s" tidak ada';
  SFieldNotFound2 = 'Field %d tidak ada';

  SLoginPromptFailure = 'Tidak ada dialog Login default. Silahkan tambahkan DBLogDlg ke klausula `uses` pada file utama.';

  SPropertyQuery = 'Query mungkin berlangsung lama pada database besar!';
  SPropertyTables = 'Batasi dengan Katalog data/atau Skema.';
  SPropertyColumns = 'Batasi dengan Katalog, Skema dan/atau Nama Tabel.';
  SPropertyProcedures = 'Batasi dengan Katalog dan/atau Skema.';
  SPropertySequences = 'Batasi dengan Katalog dan/atau Skema.';
  SPropertyExecute = 'Apakah Query jadi dieksekusi?';

  SFormTest = 'Tes Editor SQLZEOS';
  SButtonClose = '&Tutup';
  SFormEditor = 'Editor SQL ZEOS';
  STabSheetSelect = 'SQL Select';
  SMenuLoad = 'Ambil';
  SMenuSave = 'Simpan';
  SButtonGenerate = '&Generate';
  SButtonCheck = '&Cek';
  SButtonTest = 'T&es';
  SButtonOk = '&OK';
  SButtonCancel = '&Batal';
  STableAlias = 'Alias T&abel';
  SReplaceSQL = 'SQL &Replace';
  SDialogOpenTitle = 'Buka File SQL';
  SDialogSaveTitle = 'Simpan File SQL';
  SSQLEditor = 'Editor SQL';
  SDatabaseDialog = 'Buka database yang tersedia';

  SUpdateSQLNoResult = 'Tidak ada Resultset pada Update Refresh SQL';
  SUpdateSQLRefreshStatementcount ='Jumlah Statement pada Update Refresh SQL harus 1';

  {$IFDEF FPC}
  SNotEditing = 'Dataset tidak dalam mode edit atau sisip';
  SFieldTypeMismatch = 'Tipe tidak sesuai pada field ''%s'', seharusnya: %s aktual: %s';
  SFieldSizeMismatch = 'Ukuran tidak sesuai pada field ''%s'', seharusnya: %d aktual: %d';
  {$ENDIF}

  SFailedtoInitPrepStmt   = 'Gagal inisialisasi Prepared statement';
  SFailedtoPrepareStmt    = 'Statemen gagal sewaktu proses persiapan';
  SFailedToBindAllValues  = 'Aplikasi gagal dalam penggabungan (bind) pendahuluan semua nilai';
  SAttemptExecOnBadPrep   = 'Percobaan eksekusi statemen dilakukan sebelum persiapan berhasil.';
  SBindingFailure         = 'Gagal mem-gabungkan (bind) parameter';
  SPreparedStmtExecFailure = 'Prepared Statement gagal dieksekusi';
  SBoundVarStrIndexMissing = 'Teks variabel indeks "%s" tidak ada';
  SBindVarOutOfRange      = 'Variabel indeks diluar jangkauan: %d';

  SRefreshRowOnlySupportedWithUpdateObject = 'Metode RefreshRow hanya didukung oleh obyek Update';
  SMustBeInBrowseMode = 'Operasi hanya diperbolehkan pada status dsBrowse';

  SUnKnownParamDataType = 'Param.DataType tidak dikenal';

  // <--- end added by tohenk

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
  SUnsupportedByDriver    = 'Driver can not support this feature natively: [%s]';

  SConnectionIsNotOpened = 'Connection is not opened yet';
  SInvalidOpInAutoCommit = 'Invalid operation in AutoCommit mode';
  SInvalidOpInNonAutoCommit = 'Invalid operation in non AutoCommit mode';
  SInvalidOpPrepare = 'Prepare transaction only possible on matching first(!) Starttransaction';

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

  SUpdateSQLNoResult = 'Update Refresh SQL delivered no resultset';
  SUpdateSQLRefreshStatementcount ='Update Refresh SQL Statement count must be 1';

  {$IFDEF FPC}
  SNotEditing = 'Dataset not in edit or insert mode';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  {$ENDIF}

  SFailedtoInitPrepStmt   = 'Prepared statement failed to initialize';
  SFailedtoPrepareStmt    = 'Statement failed during prepare process';
  SFailedToBindAllValues  = 'Application failed to pre-bind all values';
  SAttemptExecOnBadPrep   = 'Attempt made to execute a statement before a successful preparation.';
  SBindingFailure         = 'Failed to bind parameter set';
  SPreparedStmtExecFailure = 'Prepared statement failed to execute';
  SBoundVarStrIndexMissing = 'Bound variable text index "%s" does not exist';
  SBindVarOutOfRange      = 'Bound variable index out of range: %d';

//FOS+ 07112006
  SRefreshRowOnlySupportedWithUpdateObject = 'The refreshrow method is only supported with an update object';
  SMustBeInBrowseMode = 'Operation is only allowed in dsBROWSE state';

  SUnKnownParamDataType = 'Unknown Param.DataType';

{$ENDIF}   // INDONESIAN <--- added by tohenk

{$ENDIF}   // ROMANA

{$ENDIF} //SPANISH

{$ENDIF} // GERMAN

{$ENDIF} // DUTCH

{$ENDIF} // PORTUGUESE

implementation

end.
