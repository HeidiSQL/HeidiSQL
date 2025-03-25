Converting Delphi units and forms to Lazarus

U = .pas unit file
F = .dfm form file
UF = both

* U add {$mode delphi}{$H+} between unit and interface
* U remove unit prefixes, like System.Classes => Classes, Vcl.Graphics => Graphics
* U rename {$R *.dfm} to {$R *.lfm}
* U TComboBoxEx exists? use comboex unit
* U replace TSynEdit.GetWordAtCursor => TSynEdit.GetWordAtRowCol(SynEdit.CaretXY)
* F rename .dfm file extension to .lfm
* F if form was designed in > 96 PPI: add "DesignTimePPI = xyz"
* (F prefer using Lazarus IDE on 100% DPI?)
* F decrease Constraints.Min/MaxWidth as these get scaled too much
* F remove all explicit Font properties, mainly Color, Height and Name, causing too small text on Linux
* UF replace TSynMemo => TSynEdit
* UF replace TVirtualStringTree => TLazVirtualStringTree (+ use laz.VirtualTrees unit)
* UF replace TVirtualStringTree|Columns => TLazVirtualStringTree|Header.Columns
* UF replace TButtonedEdit => TEditButton (+ use EditBtn unit)
* UF if TButton has Images + ImageIndex: replace TButton => TSpeedButton (note: ModalResult not supported!)
* UF if TButton has Style + DropDownMenu: replace TButton => TSpeedButton with ImageIndex 108 and let OnClick activate the TPopupMenu
* UF replace TLinkLabel => TLabel and let OnClick do with its .Hint what OnLinkClick did with a Link, and set Font.Style.fsUnderline
* U open unit, accept removal of unknown properties
* U press Shift+F11 to add unit+form to project


Later:
* TUpDown has minimum Min value of -32768 and maximum Max of 32767. Remove and activate TEdit.NumbersOnly
* New keyboard hotkey editor with checkboxes for Shift Alt Ctrl + comboBox for key
