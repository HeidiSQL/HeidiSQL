unit SMCnst;

interface

{Japanese strings}
const
  strMessage = '印刷...';
  strSaveChanges = '変更を保存しますか?';
  strErrSaveChanges = 'データを保存できません! サーバへの接続を確認してください｡';
  strDeleteWarning = 'テーブルを削除します｡よろしいですか？ %s?';
  strEmptyWarning = 'Do you really want to empty a table %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('レコードの追加',
    'Insert record',
    'レコードの編集',
    'レコードの削除',
    '-',
    '印刷 ...',
    'エクスポート ...',
    '-',
    '変更の保存',
    '変更の破棄',
    '表示の更新',
    '-',
    'レコード選択/選択の解除',
       'レコードの選択',
       'すべてのレコードの選択',
       '-',
       'レコード選択の解除',
       'すべてのレコード選択の解除',
    '-',
    'レイアウトの保存',
    'レイアウト保存の呼び出し',
    '-',
    'セットアップ...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Title ';
  SgbData = ' Data ';
  STitleCaption = 'Caption:';
  STitleAlignment = 'Alignment:';
  STitleColor = 'Color:'; 
  STitleFont = 'Font:';
  SWidth = 'Width:';
  SWidthFix = 'characters';
  SAlignLeft = 'left';
  SAlignRight = 'right';
  SAlignCenter = 'center';

const //for SMDBNavigator
  SFirstRecord = '先頭';
  SPriorRecord = '前へ';
  SNextRecord = '次へ';
  SLastRecord = '最終';
  SInsertRecord = '挿入';
  SCopyRecord = 'コピー';
  SDeleteRecord = '削除';
  SEditRecord = '編集';
  SFilterRecord = 'フィルタ';
  SFindRecord = '検索';
  SPrintRecord = '印刷';
  SExportRecord = 'エクスポート';
  SPostEdit = '保存';
  SCancelEdit = 'キャンセル';
  SRefreshRecord = '更新';
  SChoice = '選択';
  SClear = '選択破棄';
  SDeleteRecordQuestion = 'レコードを削除しますか?';
  SDeleteMultipleRecordsQuestion = '選択されたレコードを削除しますか?';
  SRecordNotFound = 'レコードが見つかりません';

  SFirstName = '先頭';
  SPriorName = '前へ';
  SNextName = '次へ';
  SLastName = '最終';
  SInsertName = '挿入';
  SCopyName = 'コピー';
  SDeleteName = '削除';
  SEditName = '編集';
  SFilterName = 'フィルタ';
  SFindName = '検索';
  SPrintName = '印刷';
  SExportName = 'エクスポート';
  SPostName = '保存';
  SCancelName = 'キャンセル';
  SRefreshName = '更新';
  SChoiceName = '選択';
  SClearName = 'クリア';

  SRecNo = '#';
  SRecOf = ' of ';

const //for EditTyped
  etValidNumber = '数値';
  etValidInteger = '整数値';
  etValidDateTime = '日付/時刻';
  etValidDate = '日付';
  etValidTime = '時刻';
  etValid = '有効';
  etIsNot = 'is not a';
  etOutOfRange = '値 %s 範囲を超えています %s..%s';

implementation

end.
