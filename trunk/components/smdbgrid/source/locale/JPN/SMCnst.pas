unit SMCnst;

interface

{Japanese strings}
const
  strMessage = '���...';
  strSaveChanges = '�ύX��ۑ����܂���?';
  strErrSaveChanges = '�f�[�^��ۑ��ł��܂���! �T�[�o�ւ̐ڑ����m�F���Ă��������';
  strDeleteWarning = '�e�[�u�����폜���܂����낵���ł����H %s?';
  strEmptyWarning = 'Do you really want to empty a table %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('���R�[�h�̒ǉ�',
    'Insert record',
    '���R�[�h�̕ҏW',
    '���R�[�h�̍폜',
    '-',
    '��� ...',
    '�G�N�X�|�[�g ...',
    '-',
    '�ύX�̕ۑ�',
    '�ύX�̔j��',
    '�\���̍X�V',
    '-',
    '���R�[�h�I��/�I���̉���',
       '���R�[�h�̑I��',
       '���ׂẴ��R�[�h�̑I��',
       '-',
       '���R�[�h�I���̉���',
       '���ׂẴ��R�[�h�I���̉���',
    '-',
    '���C�A�E�g�̕ۑ�',
    '���C�A�E�g�ۑ��̌Ăяo��',
    '-',
    '�Z�b�g�A�b�v...');

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
  SFirstRecord = '�擪';
  SPriorRecord = '�O��';
  SNextRecord = '����';
  SLastRecord = '�ŏI';
  SInsertRecord = '�}��';
  SCopyRecord = '�R�s�[';
  SDeleteRecord = '�폜';
  SEditRecord = '�ҏW';
  SFilterRecord = '�t�B���^';
  SFindRecord = '����';
  SPrintRecord = '���';
  SExportRecord = '�G�N�X�|�[�g';
  SPostEdit = '�ۑ�';
  SCancelEdit = '�L�����Z��';
  SRefreshRecord = '�X�V';
  SChoice = '�I��';
  SClear = '�I��j��';
  SDeleteRecordQuestion = '���R�[�h���폜���܂���?';
  SDeleteMultipleRecordsQuestion = '�I�����ꂽ���R�[�h���폜���܂���?';
  SRecordNotFound = '���R�[�h��������܂���';

  SFirstName = '�擪';
  SPriorName = '�O��';
  SNextName = '����';
  SLastName = '�ŏI';
  SInsertName = '�}��';
  SCopyName = '�R�s�[';
  SDeleteName = '�폜';
  SEditName = '�ҏW';
  SFilterName = '�t�B���^';
  SFindName = '����';
  SPrintName = '���';
  SExportName = '�G�N�X�|�[�g';
  SPostName = '�ۑ�';
  SCancelName = '�L�����Z��';
  SRefreshName = '�X�V';
  SChoiceName = '�I��';
  SClearName = '�N���A';

  SRecNo = '#';
  SRecOf = ' of ';

const //for EditTyped
  etValidNumber = '���l';
  etValidInteger = '�����l';
  etValidDateTime = '���t/����';
  etValidDate = '���t';
  etValidTime = '����';
  etValid = '�L��';
  etIsNot = 'is not a';
  etOutOfRange = '�l %s �͈͂𒴂��Ă��܂� %s..%s';

implementation

end.
