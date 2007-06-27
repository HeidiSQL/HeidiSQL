 TEDBImage 1.4 and TQREDBImage 1.0 (Enhaced TDBImage and TQRDBImage):
  by Sebastián Mayorá - Argentina - DelphiHelper@yahoo.com.ar

TQREDBimage was designed for printing image files. It has the same features that TEDBImage has.

»It works like TDBImage except :
   - It can manage .ico .bmp .wmf .emf .jpg .jpeg. Without a line of code !!!
   - Can copy to clipboard .bmp .wmf .jpg .jpeg	
   - Event OnLoadCustomImage is fired when the image type is unknown, so you can load "any" type of images (gif, tiff, png,....)

»What you can do is:
  - Copy, Cut and paste from clipboard. No code needed.
  - LoadFromFile and SaveToFile (New in v1.3)
  - Load "any" type of TGraphic using OnLoadCustomImage event:
	If you need OTHER kind of graphics (such GIF, TIFF, etc)
   	then you should write something like this in OnLoadCustomImage Event:

    	procedure TForm1.EDBImage1LoadCustomImage(var B: TGraphic; Stream: TStream);
    	begin  
      	   B := TXXX.create;		{XXX is your class of Graphic (TGifImage, TTiffImage, etc).}
           B.LoadFromStream( Stream );
    	end;  				//That is ALL.!!!



»Please mail me for: - Bugs
                    - Suggestinons
                    - say Hello.
                    - Comments
                    - etc...

»New in this version. 1.4
- Optimized LoadPicture;
- Some bugs fixed
- Added LoadFromFile and SavetoFile methods

»Known Issues
- If TEDBIMage is within TDBCtrlGrid report errors. I don't know why yet.
- Tested with D6, D5 and D4.
- OnLoadCustomImage  tested with TGIFImage (from RXLib)


Next release:
- Full test in all supported versions of delphi (D4, D5, D6).
- Option to autoconvert images to JPG (or other) and store compressed (to save space)

Thanks to: Mohsen Rahmani, Mr. Hong, René Simon and Dayne for their help. 

THIS IS FREEWARE - USE AT YOUR OWN RISK, ETC, ETC    
