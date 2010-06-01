unit GraphicStrings;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicColor.pas, released November 1, 1999.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Plei�a, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are Copyright
// (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
// This file is part of the image library GraphicEx.
//
// GraphicStrings contains the strings used in GraphicEx which could be localized.
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
// This is the russian version of GraphicStrings.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

resourcestring
  // image file descriptions
  gesAllImages = '��� �����������';
  gesRegistration = '������� ���������������� %s ����� (������).';
  
  gesBitmaps = 'Windows ������� �����';
  gesRLEBitmaps = 'Run length �������������� ������� ����� Windows';
  gesDIBs = '������� ����� Windows, ����������� �� ����������';
  gesIcons = '����������� Windows';
  gesMetaFiles = '��������� Windows';
  gesEnhancedMetaFiles = '����������� ��������� Windows';
  gesJPGImages = '����������� JPG';
  gesJPEGImages = '����������� JPEG';
  gesTruevision = '����������� Truevision';
  gesTIFF = '����������� ������� TIFF';
  gesMacTIFF =  '����������� TIFF ��� Macintosh';
  gesPCTIF = 'PC TIF �����������';
  gesGFIFax = 'GFI fax images';
  gesSGI = '����������� SGI';
  gesSGITrueColor = '������������ ����������� SGI';
  gesZSoft = '����������� ZSoft Paintbrush';
  gesZSoftWord = '������ ������ Word 5.x';
  gesAliasWaveFront = '����������� Alias/Wavefront';
  gesSGITrueColorAlpha = '������������ ����������� SGI � �����-�������';
  gesSGIMono = '׸���-����� ����������� SGI';
  gesPhotoshop = '����������� Photoshop';
  gesPortable = '����������� Portable map';
  gesPortablePixel = '����������� Portable pixel map';
  gesPortableGray = '����������� Portable gray map';
  gesPortableMono = '����������� Portable bitmap';
  gesAutoDesk = '����������� Autodesk';
  gesKodakPhotoCD = '����������� Kodak Photo-CD';
  gesCompuserve = '����������� CompuServe';
  gesHalo = '����������� Dr. Halo';
  gesPaintShopPro = '����������� Paintshop Pro';
  gesPortableNetworkGraphic = '����������� Portable network graphic (PNG)';

  // image specific error messages
  gesInvalidImage = '���������� ��������� �����������. ������������ ��� ���������������� ������ ����������� %s.';
  gesInvalidColorFormat = '������������ ������ ����� � ����� %s.';
  gesStreamReadError = '������ ������ �� ������ � ����� %s.';
  gesUnsupportedImage = '���������� ��������� �����������. ���������������� ������ ����������� %s.';
  gesUnsupportedFeature = '���������� ��������� �����������. %s �� �������������� ��� ������ %s.';
  gesInvalidCRC = '���������� ��������� �����������. ������ CRC ������� � ����� %s.';
  gesCompression = '���������� ��������� �����������. ������ ������ � ����� %s.';
  gesExtraCompressedData = '���������� ��������� �����������. �������������� ������ ������� � ����� %s.';
  gesInvalidPalette = '���������� ��������� �����������. ������������ ������� � ����� %s.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = '����� ������ ';
  gesPCDImageSize = '������� �����������, �������� �� Base16, Base4 or Base ';
  gesRLAPixelFormat = '������� �����������, �������� �� RGB and RGBA ';
  gesPSPFileType = '������ ������� �����, �������� �� 3� ��� 4� ';

  // errors which apply only to specific image types
  gesUnknownCriticalChunk = '���������� ��������� ����������� PNG. ���������� �����������, �� ����������� ������.';

  // color manager error messages
  gesIndexedNotSupported = '��������� ����� ���������������� � ��-���������������� ��������� ����������� �� ��������������.';
  gesConversionUnsupported = '�������� ��������� �� ��������������. �� �������� ����� ���������� �����.';
  gesInvalidSampleDepth = '������������ �������� �������. �������������� ������� � �����: 1, 2, 4, 8, or 16.';
  gesInvalidPixelDepth = '������� ����������� � ����� �� �������� � ������� �������� �����.';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
