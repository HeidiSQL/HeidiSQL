unit SMCnst;

interface

{Ukrainian strings}
const
  strMessage = '����...';
  strSaveChanges = '�� ������� �������� ������i ��i�� �� ������i?';
  strErrSaveChanges = '����i �� ������� ��������! �����i��� �''������� � �������� �� �������i��� �������� ������.';
  strDeleteWarning = '�� �i���� ������ �������� ������� %s?';
  strEmptyWarning = '�� �i���� ������ �������� ���i � ������i %s?';

const PopUpCaption: array [0..22] of string[33] =
   ('������ �����',
    '�������� �����',
    '����������� �����',
    '�������� �����',
    '-',
    '���� ...',
    '������� ������ ...',
    '-',
    '�������� ��i��',
    '�i��i���� ��i��',
    '�������� ����i',
    '-',
    '���i���/����� ������� � �����i�',
       '���i���� �����',
       '���i���� ��i ������',
       '-',
       '����� ������ �����',
       '����� ������ � ��i� ������',
    '-',
    '�������� ��������� �������',
    '³������� ��������� �������',
    '-',
    '������������...');


const //for TSMSetDBGridDialog
  SgbTitle = ' ��������� ';
  SgbData = ' ���i ';
  STitleCaption = '�����:';
  STitleAlignment = '���i��������:';
  STitleColor = '���� ����:';
  STitleFont = '�����:';
  SWidth = '������:';
  SWidthFix = '������i�';
  SAlignLeft = '��i��';
  SAlignRight = '������';
  SAlignCenter = '�� ������';

const //for TSMDBFilterDialog
  strEqual = '�i���';
  strNonEqual = '�� �i���';
  strNonMore = '�� �i����';
  strNonLess = '�� �����';
  strLessThan = '������ �i�';
  strLargeThan = '�i���� �i�';
  strExist = '�����';
  strNonExist = '�� �����';
  strIn = '� ������';
  strBetween = '�i�';

  strOR = '���';
  strAND = '��';

  strField = '����';
  strCondition = '�����';
  strValue = '��������';

  strAddCondition = ' ����i�� ��������� �����:';
  strSelection = ' ���i� �����i� ��i��� ��������� ����:';

  strAddToList = '������ �� ������';
  strEditInList = '�����������';
  strDeleteFromList = '�������� � ������';

  strTemplate = '������� �i����i�';
  strFLoadFrom = '��������� �...';
  strFSaveAs = '�������� ��..';
  strFDescription = '����';
  strFFileName = '��''� �����';
  strFCreate = '��������: %s';
  strFModify = '�����i������: %s';
  strFProtect = '������ �i� ����������';
  strFProtectErr = '���� �������� �i� ������!';

const //for SMDBNavigator
  SFirstRecord = '������ �����';
  SPriorRecord = '��������� �����';
  SNextRecord = '��������� �����';
  SLastRecord = '������� �����';
  SInsertRecord = '������� ������';
  SCopyRecord = '��������� ������';
  SDeleteRecord = '��������� ������';
  SEditRecord = '����������� ������';
  SFilterRecord = '�i��i� �����i�';
  SFindRecord = '����� ������';
  SPrintRecord = '���� �����i�';
  SExportRecord = '������� �����i�';
  SPostEdit = '���������� ��i�';
  SCancelEdit = '�i��i�� ��i�';
  SRefreshRecord = '��������� �����';
  SChoice = '���i� �����';
  SClear = '�������� ���i';
  SDeleteRecordQuestion = '�������� �����?';
  SDeleteMultipleRecordsQuestion = '�������� ��i ������i ������?';
  SRecordNotFound = '����� �� ��������';

  SFirstName = '�������';
  SPriorName = '�����.';
  SNextName = '������.';
  SLastName = '�i����';
  SInsertName = '�������';
  SCopyName = '���i������';
  SDeleteName = '���������';
  SEditName = '�����������';
  SFilterName = '�i����';
  SFindName = '�����';
  SPrintName = '����';
  SExportName = '�������';
  SPostName = '����������';
  SCancelName = '���������';
  SRefreshName = '���������';
  SChoiceName = '���i�';
  SClearName = '�����';

  SBtnOk = '&OK';
  SBtnCancel = '&�i��i��';
  SBtnLoad = '���������';
  SBtnSave = '��������';
  SBtnCopy = '���i�����';
  SBtnPaste = '��������';
  SBtnClear = '��������';

  SRecNo = '�';
  SRecOf = ' � ';

const //for EditTyped
  etValidNumber = '��������� ������';
  etValidInteger = '��������� �i��� ������';
  etValidDateTime = '��������� ����� �� �����';
  etValidDate = '��������� �����';
  etValidTime = '��������� �����';
  etValid = '���������';
  etIsNot = '�� �';
  etOutOfRange = '�������� %s �� ������ � �������i %s..%s';

  SApplyAll = '��������� ��i�';

implementation

end.
