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
// The initial developer of the original code is Dipl. Ing. Mike Lischke (PleiЯa, Germany, www.delphi-gems.com),
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
  gesAllImages = 'Все изображения';
  gesRegistration = 'Попытка харегистрировать %s файлы (дважды).';
  
  gesBitmaps = 'Windows битовые карты';
  gesRLEBitmaps = 'Run length закодированные битовые карты Windows';
  gesDIBs = 'Битовые карты Windows, независимые от устройства';
  gesIcons = 'Пиктограммы Windows';
  gesMetaFiles = 'Метафайлы Windows';
  gesEnhancedMetaFiles = 'Расширенные метафайлы Windows';
  gesJPGImages = 'Изображения JPG';
  gesJPEGImages = 'Изображения JPEG';
  gesTruevision = 'Изображения Truevision';
  gesTIFF = 'Изображения формата TIFF';
  gesMacTIFF =  'Изображения TIFF для Macintosh';
  gesPCTIF = 'PC TIF изображения';
  gesGFIFax = 'GFI fax images';
  gesSGI = 'Изображения SGI';
  gesSGITrueColor = 'Полноцветные изображения SGI';
  gesZSoft = 'Изображения ZSoft Paintbrush';
  gesZSoftWord = 'Снимки экрана Word 5.x';
  gesAliasWaveFront = 'Изображения Alias/Wavefront';
  gesSGITrueColorAlpha = 'Полноцветные изображения SGI с альфа-каналом';
  gesSGIMono = 'Чёрно-белые изображения SGI';
  gesPhotoshop = 'Изображения Photoshop';
  gesPortable = 'Изображения Portable map';
  gesPortablePixel = 'Изображения Portable pixel map';
  gesPortableGray = 'Изображения Portable gray map';
  gesPortableMono = 'Изображения Portable bitmap';
  gesAutoDesk = 'Изображения Autodesk';
  gesKodakPhotoCD = 'Изображения Kodak Photo-CD';
  gesCompuserve = 'Изображения CompuServe';
  gesHalo = 'Изображения Dr. Halo';
  gesPaintShopPro = 'Изображения Paintshop Pro';
  gesPortableNetworkGraphic = 'Изображения Portable network graphic (PNG)';

  // image specific error messages
  gesInvalidImage = 'Невозможно загружить изображение. Неправильный или неподдерживаемый формат изображения %s.';
  gesInvalidColorFormat = 'Неправильный формат цвета в файле %s.';
  gesStreamReadError = 'Ошибка чтения из потока в файле %s.';
  gesUnsupportedImage = 'Невозможно загружить изображение. Неподдерживаемый формат изображения %s.';
  gesUnsupportedFeature = 'Невозможно загружить изображение. %s не поддерживается для файлов %s.';
  gesInvalidCRC = 'Невозможно загружить изображение. Ошибка CRC найдена в файлы %s.';
  gesCompression = 'Невозможно загружить изображение. Ошибка сжатия в файле %s.';
  gesExtraCompressedData = 'Невозможно загружить изображение. Дополнительные данные найдены в файле %s.';
  gesInvalidPalette = 'Невозможно загружить изображение. Неправильная палитра в файле %s.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'Схема сжатия ';
  gesPCDImageSize = 'Размеры изображения, отличные от Base16, Base4 or Base ';
  gesRLAPixelFormat = 'Форматы изображений, отличные от RGB and RGBA ';
  gesPSPFileType = 'Версии формата файла, отличные от 3й или 4й ';

  // errors which apply only to specific image types
  gesUnknownCriticalChunk = 'Невозможно загрузить изображение PNG. Обнаружена неожиданная, но критическая ошибка.';

  // color manager error messages
  gesIndexedNotSupported = 'Конверсия между индексированными и не-индексированными форматами изображений не поддерживается.';
  gesConversionUnsupported = 'Цветовая конверсия не поддерживается. Не возможно найти правильный метод.';
  gesInvalidSampleDepth = 'Неправильная цветовая глубина. Поддерживается глубина в битах: 1, 2, 4, 8, or 16.';
  gesInvalidPixelDepth = 'Глубина изображения в битах не подходит к текущей цветовой схеме.';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
