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
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleißa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are Copyright
// (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
// This file is part of the image library GraphicEx.
//
// GraphicStrings contains the strings used in GraphicEx which could be localized.
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
// This is the german version of GraphicStrings.
//
//----------------------------------------------------------------------------------------------------------------------

interface
                   
{$I GraphicConfiguration.inc}

resourcestring
  // image file descriptions
  gesAllImages = 'Alle Bilder';
  gesRegistration = 'Das Format %s ist schon registriert.';

  gesBitmaps = 'Windows bitmaps';
  gesRLEBitmaps = 'Run length encoded Windows bitmaps';
  gesDIBs = 'Geräteunabhängige Windows bitmaps';
  gesEPS = 'Encapsulated Postscript Bilder';
  gesIcons = 'Windows icons';
  gesMetaFiles = 'Windows metafiles';
  gesEnhancedMetaFiles = 'Windows erweiterte metafiles';
  gesJPGImages = 'JPG Bilder';
  gesJPEGImages = 'JPEG Bilder';
  gesJPEImages = 'JPE Bilder';
  gesJFIFImages = 'JFIF Bilder';
  gesTruevision = 'Truevision Bilder';
  gesTIFF = 'Tagged image file format';
  gesMacTIFF =  'Macintosh TIFF Bilder';
  gesPCTIF = 'PC TIF Bilder';
  gesGFIFax = 'GFI FAX Dateien';
  gesSGI = 'SGI Bilder';
  gesSGITrueColor = 'SGI True Color Bilder';
  gesZSoft = 'ZSoft Paintbrush Bilder';
  gesZSoftWord = 'Word 5.x Snapschuss Bilder';
  gesAliasWaveFront = 'Alias/Wavefront Bilder';
  gesSGITrueColorAlpha = 'SGI True Color Bilder mit Transparenz';
  gesSGIMono = 'SGI schwarz/weiss Bilder';
  gesPhotoshop = 'Photoshop Bilder';
  gesPortable = 'Portable map Bilder';
  gesPortablePixel = 'Portable pixel map Bilder';
  gesPortableGray = 'Portable gray map Bilder';
  gesPortableMono = 'Portable bitmap Bilder';
  gesAutoDesk = 'Autodesk Bilder';
  gesKodakPhotoCD = 'Kodak Photo-CD Bilder';
  gesCompuserve = 'CompuServe Bilder';
  gesHalo = 'Dr. Halo Bilder';
  gesPaintShopPro = 'Paintshop Pro Bilder';
  gesPortableNetworkGraphic = 'Portable network graphic Bilder';

  // image specific error messages
  gesInvalidImage = 'Bild konnte nicht geladen werden. Ungültiges oder unerwartetes %s Bildformat.';
  gesInvalidColorFormat = 'Ungültiges Farbformat in %s Bild.';
  gesStreamReadError = 'Stream Lesefehler in %s Datei.';
  gesUnsupportedImage = 'Bild konnte nicht geladen werden. Nicht unterstütztes %s Bildformat.';
  gesUnsupportedFeature = 'Bild konnte nicht geladen werden. %s nicht unterstützt für %s Dateien.';
  gesInvalidCRC = 'Bild konnte nicht geladen werden. Ein CRC Fehler ist in der %s Datei aufgetreten.';
  gesCompression = 'Bild konnte nicht geladen werden. Kompressionsfehler in %s Datei gefunden.';
  gesExtraCompressedData = 'Bild konnte nicht geladen werden. Zuviele komprimierte Daten in %s Datei gefunden.';
  gesInvalidPalette = 'Bild konnte nicht geladen werden. Palette in %s Datei ist ungültig.';
  gesUnknownCriticalChunk = 'PNG Bild konnte nicht geladen werden. Unerwarteter, aber als kritisch markierter Chunk gefunden.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'Das Kompressionsformat ist';
  gesRLAPixelFormat = 'Andere Bildformat, als RGB und RGBA werden';
  gesPSPFileType = 'Andere Dateiversionen als 3 or 4 werden';

  // color manager error messages
  gesIndexedNotSupported = 'Konvertierung zwischen indizierten und nicht-indizierten Formaten wird nicht unterstützt.';
  gesConversionUnsupported = 'Farbkonvertierung schlug fehl. Es konnte keine passende Konvertierungsmethode gefunden werden.';
  gesInvalidSampleDepth = 'Farbtiefe ist nicht gültig. Bits pro Sample muss 1, 2, 4, 8 oder 16 sein.';
  gesInvalidPixelDepth = 'Sampleanzahl pro Pixel korrespondiert nicht zum angegebenen Farbschema.';
  gesInvalidSubSampling = 'Subsampling Wert ist ungültig. Erlaubt sind 1, 2 und 4.';
  gesVerticalSubSamplingError = 'Der vertikale Subsampling Wert muss kleiner oder gleich dem horizontalen Wert sein.';

  // progress strings
  gesPreparing = 'Vorbereitung...';
  gesLoadingData = 'Daten werden geladen...';
  gesUpsampling = 'Upsampling...';
  gesTransfering = 'Übertragung...';

  // compression errors
  gesLZ77Error = 'LZ77 Dekompressionsfehler.';
  gesJPEGEOI = 'JPEG Dekompressionsfehler. Unerwartetes Ende der Eingabedaten.';
  gesJPEGStripSize = 'Unpassende JPEG Strip oder Tile Größe.';
  gesJPEGComponentCount = 'Unpassende JPEG Komponentenanzahl';
  gesJPEGDataPrecision = 'Unpassende JPEG Datengenauigkeit.';
  gesJPEGSamplingFactors = 'Unpassende JPEG Samplingfaktoren.';
  gesJPEGBogusTableField = 'Falsches JPEG Tabellenfeld gefunden.';
  gesJPEGFractionalLine = 'Unvollständige JPEG Bildzeilen werden nicht understützt.';

  // miscellaneous
  gesWarning = 'Warnung';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
