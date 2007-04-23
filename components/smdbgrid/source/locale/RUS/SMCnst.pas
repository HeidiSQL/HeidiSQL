unit SMCnst;

interface

{Russian strings}
const
  strMessage = '������...';
  strSaveChanges = '�� ������ ��������� ��������� ��������� �� �������?';
  strErrSaveChanges = '������ �� ������� ���������! ��������� ���������� � �������� � ������������ ��������� ������.';
  strDeleteWarning = '�� ������������� ������ ������� ������� %s?';
  strEmptyWarning = '�� ������������� ������ ������� ������ �� ������� %s?';

const PopUpCaption: array [0..22] of string[33] =
   ('�������� ������',
    '�������� ������',
    '������������� ������',
    '������� ������',
    '-',
    '������ ...',
    '������� ������ ...',
    '-',
    '��������� ���������',
    '�������� ���������',
    '�������� ������',
    '-',
    '�������/���������� �������',
       '�������� ������',
       '�������� ��� ������',
       '-',
       '����������� ������',
       '����������� ��� ������',
    '-',
    '��������� ������������ �������',
    '������������ ������������ �������',
    '-',
    '���������...');


const //for TSMSetDBGridDialog
  SgbTitle = ' ��������� ';
  SgbData = ' ������ ';
  STitleCaption = '��������:';
  STitleAlignment = '������������:';
  STitleColor = '���� ����:'; 
  STitleFont = '�����:';
  SWidth = '������:';
  SWidthFix = '��������';
  SAlignLeft = '�����';
  SAlignRight = '������';
  SAlignCenter = '�� ������';

const //for TSMDBFilterDialog
  strEqual = '�����';
  strNonEqual = '�� �����';
  strNonMore = '�� �����';
  strNonLess = '�� �����';
  strLessThan = '������ ���';
  strLargeThan = '������ ���';
  strExist = '�����';
  strNonExist = '�� �����';
  strIn = '� ������';
  strBetween = '�����';

  strOR = '���';
  strAND = '�';

  strField = '����';
  strCondition = '�������';
  strValue = '��������';

  strAddCondition = ' ���������� �������������� �������:';
  strSelection = ' ����� ������� �� ��������� ��������:';

  strAddToList = '�������� � ������';
  strDeleteFromList = '������� �� ������';

  strTemplate = '������� ��������';
  strFLoadFrom = '��������� ��...';
  strFSaveAs = '��������� ���..';
  strFDescription = '��������';
  strFFileName = '��� �����';
  strFCreate = '������: %s';
  strFModify = '�������������: %s';
  strFProtect = '������ �� ����������';
  strFProtectErr = '���� ������� �� ������!';

const //for SMDBNavigator
  SFirstRecord = '������ ������';
  SPriorRecord = '���������� ������';
  SNextRecord = '��������� ������';
  SLastRecord = '��������� ������';
  SInsertRecord = '������� ������';
  SCopyRecord = '����������� ������';
  SDeleteRecord = '�������� ������';
  SEditRecord = '�������������� ������';
  SFilterRecord = '������� �������';
  SFindRecord = '����� ������';
  SPrintRecord = '������ �������';
  SExportRecord = '������� �������';
  SPostEdit = '���������� ���������';
  SCancelEdit = '������ ���������';
  SRefreshRecord = '���������� ������';
  SChoice = '����� ������';
  SClear = '������� ������';
  SDeleteRecordQuestion = '������� ������?';
  SDeleteMultipleRecordsQuestion = '������� ��� ��������� ������?';
  SRecordNotFound = '������ �� �������';

  SFirstName = '������';
  SPriorName = '����.';
  SNextName = '����.';
  SLastName = '�����';
  SInsertName = '�������';
  SCopyName = '�����������';
  SDeleteName = '��������';
  SEditName = '��������������';
  SFilterName = '������';
  SFindName = '�����';
  SPrintName = '������';
  SExportName = '�������';
  SPostName = '����������';
  SCancelName = '������';
  SRefreshName = '����������';
  SChoiceName = '�����';
  SClearName = '�����';

  SBtnOk = '&OK';
  SBtnCancel = '�&�����';
  SBtnLoad = '���������';
  SBtnSave = '���������';
  SBtnCopy = '����������';
  SBtnPaste = '��������';
  SBtnClear = '��������';

  SRecNo = '�';
  SRecOf = ' �� ';

const //for EditTyped
  etValidNumber = '���������� ������';
  etValidInteger = '���������� ����� ������';
  etValidDateTime = '���������� ����� � ��������';
  etValidDate = '���������� �����';
  etValidTime = '���������� ��������';
  etValid = '����������';
  etIsNot = '�� ��������';
  etOutOfRange = '�������� %s �� ����������� ��������� %s..%s';


implementation

end.
