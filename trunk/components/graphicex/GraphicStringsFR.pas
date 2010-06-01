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
// This is the french version of GraphicStrings.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

resourcestring
  // image file descriptions
  gesAllImages = 'Toutes les images';
  gesRegistration = 'Tentative de re-enregistrement des fichiers %s.';

  gesBitmaps = 'Bitmaps Windows';
  gesRLEBitmaps = 'Bitmaps Windows (Run length encoded)';
  gesDIBs = 'Bitmaps Windows (Device independant)';
  gesEPS = 'Images Postscript Encapsulées';
  gesIcons = 'Icone Windows';
  gesMetaFiles = 'Metafiles Windows';
  gesEnhancedMetaFiles = 'Metafiles Windows améliorés';
  gesJPGImages = 'Images JPG';
  gesJPEGImages = 'Images JPEG';
  gesJPEImages = 'Images JPE images';
  gesJFIFImages = 'Images JFIF images';
  gesTruevision = 'Images Truevision';
  gesTIFF = 'Images Tagged image file format';
  gesMacTIFF =  'Images TIFF Macintosh';
  gesPCTIF = 'Images PC TIF';
  gesGFIFax = 'Images GFI fax';
  gesSGI = 'Images SGI';
  gesSGITrueColor = 'Images SGI true color';
  gesZSoft = 'Images ZSoft Paintbrush';
  gesZSoftWord = 'Capture d''ecrant Word 5.x';
  gesAliasWaveFront = 'Images Alias/Wavefront';
  gesSGITrueColorAlpha = 'Images SGI true color avec canal alpha';
  gesSGIMono = 'Images SGI noir/blanc';
  gesPhotoshop = 'Images Photoshop';
  gesPortable = 'Images Portable map';
  gesPortablePixel = 'Images Portable pixel map';
  gesPortableGray = 'Images Portable gray map';
  gesPortableMono = 'Images Portable bitmap';
  gesAutoDesk = 'Images Autodesk';
  gesKodakPhotoCD = 'Images Kodak Photo-CD';
  gesCompuserve = 'Images CompuServe';
  gesHalo = 'Images Dr. Halo';
  gesPaintShopPro = 'Images Paintshop Pro';
  gesPortableNetworkGraphic = 'Images Portable network graphic';

  // image specific error messages
  gesInvalidImage = 'Ne peux pas charger l''image. Format de fichier %s invalide ou inattendue.';
  gesInvalidColorFormat = 'Format de couleur invalide dans le fichier %s.';
  gesStreamReadError = 'Erreur de lecture de flux dans le fichier %s.';
  gesUnsupportedImage = 'Ne peux pas charger l''image. Format de fichier %s non supporté.';
  gesUnsupportedFeature = 'Ne peux pas charger l''image. %s pas supporté par les fichiers %s.';
  gesInvalidCRC = 'Ne peux pas charger l''image. Erreur de CRC dans le fichier %s.';
  gesCompression = 'Ne peux pas charger l''image. Erreur de compression dans le fichier %s.';
  gesExtraCompressedData = 'Ne peux pas charger l''image. Surplus de données compressé trouvé dans le fichier %s.';
  gesInvalidPalette = 'Ne peux pas charger l''image. La palette du fichier %s est invalide.';
  gesUnknownCriticalChunk = 'Ne peux pas charger l''image PNG. Morceau inattendue, mais critique détecté.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'Le procédé de compression n''est';
  gesRLAPixelFormat = 'Les format d''images différents de RGB ou RGBA ne sont';
  gesPSPFileType = 'Les fichiers de version différents de 3 ou 4 ne sont';

  // color manager error messages
  gesIndexedNotSupported = 'La conversion entre les formats de pixels indexé et non-indexé n''est pas supportée.';
  gesConversionUnsupported = 'la conversion des couleurs a échoué. Méthode approprié non trouvé.';
  gesInvalidSampleDepth = 'Profondeur des couleurs invalide. Elle doit être de 1, 2, 4, 8, or 16 bits par échantillon.';
  gesInvalidPixelDepth = 'La profondeur des pixels de l''échantillon ne correspond pas au format des couleurs.';
  gesInvalidSubSampling = 'Valeur du sous échantillon est invalide. Les valeurs correctes sont 1, 2 et 4.';
  gesVerticalSubSamplingError = 'La valeur du sous échantillon vertical doit être <= à la valeur du sous échantillon horizontal.';

  // progress strings
  gesPreparing = 'Préparation...';
  gesLoadingData = 'Chargement des données...';
  gesUpsampling = 'Upsampling...';
  gesTransfering = 'Transfert...';

  // compression errors
  gesLZ77Error = 'Erreur de décompressionLZ77.';
  gesJPEGEOI = 'Erreur de décompression JPEG. Fin inattendue des entrées.';
  gesJPEGStripSize = 'Traille strip/tile incorrecte.';
  gesJPEGComponentCount = 'Nombre d''élément JPEG incorrecte.';
  gesJPEGDataPrecision = 'Précision des données JPEG incorrecte.';
  gesJPEGSamplingFactors = 'Echantillon JPEG invalides.';
  gesJPEGBogusTableField = 'Champs de la table JPEG fantôme.';
  gesJPEGFractionalLine = 'Fractional JPEG scanline non supportée.';

  // miscellaneous
  gesWarning = 'Attention';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
