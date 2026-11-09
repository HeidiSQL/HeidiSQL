# Lazarus HighDPI / VirtualTree (per-monitor DPI): findings 2026-11

Verified with a local test project `dpi` and an instrumented copy `dpi-test`
(logs the font and PPI values to a text file).

- On `WM_DPICHANGED` the LCL (`TCustomForm.WMDPIChanged` →
  `AutoAdjustLayout(lapAutoAdjustForDPI,...)` → `ScaleFontsPPI`) scales every
  control's `Font` automatically, incl. the VirtualTree's `Font` via
  ParentFont (at 175%: `Height=-21, PixelsPerInch=168`). No action needed for
  the Font *property*.
- **Node text bug:** VT paints nodes into an offscreen `TBitmap`
  (`NodeBitmap`) whose canvas font PPI = `Screen.PixelsPerInch` (primary
  monitor, 96). `Canvas.Font := Font` → `TFont.Assign` converts via `Size` when
  PPIs differ → font downconverted to 96 dpi (`H=-12`) → text renders unscaled
  even though `Tree.Font` is correct. Exists in Lazarus 4.8 **and** 5.0rc1 on
  win32 (5.0's `SetCanvasScaleFactor`/`GetCanvasScaleFactor` are no-ops on
  win32, only implemented for gtk3/cocoa/qt5). Reported as
  https://gitlab.com/freepascal.org/lazarus/lazarus/-/work_items/42702, fixed
  upstream in 4ca245a455, locally by patch 0011.
- Application-side workaround (form's `OnChangeBounds`, on `PixelsPerInch`
  change): `VT.ParentFont := False; VT.Font.PixelsPerInch := Screen.PixelsPerInch;
  VT.Font.Height := -MulDiv(Form.Font.Size, PixelsPerInch, 72);`
  The `ParentFont := False` is essential: merely assigning `Font` clears
  ParentFont internally (`TControl.FontChanged`), so the parent font is no
  longer copied on the next DPI change and the font goes stale (didn't shrink
  back to 100%).
- Node heights are NOT scaled by VT on DPI change (`NodeHeight[Node]` stays,
  e.g. 18; `DefaultNodeHeight` is only scaled if its "stored" flag is set and
  `AutoScale()` mis-measures it, e.g. 52 at 168). Fixed manually per node in
  `OnChangeBounds` (cf. `FixVT` in `source/apphelpers.pas`).
- Themed elements (check boxes etc.): `ThemeServices.DrawElement` always uses
  the system-DPI theme handle; true-size parts are not stretched to the rect.
  See patch 0012 in `lazarus-patches/README.md`.
- Header font: see "Open item" in `lazarus-patches/README.md`.
