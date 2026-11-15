# Local Lazarus patches

Patches against the installed Lazarus sources, one directory
per Lazarus release (currently `4.8/`, FPC 3.2.2). FPC patches live in
`../fpc-patches/`. Format: git diff relative to the Lazarus root, applied in
file-name order; check with `git -C <lazarus-dir> apply --check <patch>`
(or `-R` to verify an applied patch). Never edit installed sources only:
always keep the patch file here.

After a Lazarus upgrade: verify `lazbuild --version`, create
the new version directory, re-apply what is still needed (see the notes
below), and do a clean heidisql build.

## Patches with open upstream status (4.8)

### 0011-virtualtreeview-buffer-font-ppi

Upstream commit 4ca245a455 (Lazarus main, issue
https://gitlab.com/freepascal.org/lazarus/lazarus/-/work_items/42702) fixed
the VirtualTree node-text buffer-PPI bug. 0011 covers the same spot (via
`Font.PixelsPerInch`, verified equivalent) plus an extra header-bitmap hunk
(`FHeaderBitmap.Canvas.Font.PixelsPerInch`) that is not fixed upstream.
Keep 0011 unchanged while staying on 4.8. When upgrading to Lazarus 5.0
(contains 4ca245a): drop the NodeBitmap hunk and check whether the header
hunk is still needed. Background: `../hidpi.md`.

### 0012-themes-drawelement-for-ppi

VT's `PaintCheckImage` sized themed check boxes via
`GetDetailSizeForPPI(Font.PixelsPerInch)` but drew them with
`ThemeServices.DrawElement`, i.e. the system-DPI theme handle, so Windows drew
the 13px glyph on monitors whose DPI differs from the primary one. 0012 adds
`TThemeServices.DrawElementForPPI` (win32 override uses `GetThemeForPPI`) and
uses it in VT. Reported with this patch as
https://gitlab.com/freepascal.org/lazarus/lazarus/-/work_items/42717
(grids.pas has the same gap, not patched). Depends on the MetaDarkStyle
`IsTheme(hTheme, teButton)` line (`../../source/metadarkstyle/HEIDISQL-NOTES.md`),
else check boxes render light in dark mode on such monitors.

Upstream (Lazarus main, 2026-11-11): cc6e88ec30 adds
`ThemeServices.DrawElementForPPI` from this patch (same signature), d8815d6cb0
uses it in VT (same call as 0012) and also fixes grids.pas, TCheckboxThemed,
TCustomCheckCombo, TCheckListBox and TChartListbox. Not in `fixes_5` as of
2026-11-14, so 5.0 does not contain it unless backported. Keep 0012 while on
4.8; when upgrading, drop it if the release contains both commits.

## Upstream fix without local patch: VT node heights on DPI change

VT does not scale `DefaultNodeHeight` / `NodeHeight[Node]` on a DPI change.
Fixed upstream in Lazarus main for
https://gitlab.com/freepascal.org/lazarus/lazarus/-/work_items/42716 by
405b441432 (new `TBaseVirtualTree.DoAutoAdjustLayout` scaling default and
per-node heights, header font PPI in `PaintHeader`, `ScaleFontsPPI` for the
header font). Related: 43fd7bce86 fixes list item heights in TCheckListBox,
TColorBox, TColorListBox, TChartListbox. Both deliberately not backported to
`fixes_5` (maintainer is unsure about non-Windows widgetsets), so neither 4.8
nor 5.x contain them; earliest release would be 6.0 unless merged into fixes
later (status 2026-11-15).

405b441432 as committed crashes HeidiSQL at startup: its loop uses
`GetFirst`/`GetNext`, which call `InitChildren` and fire `OnInitChildren`
already from `TCustomForm.AfterConstruction` (AV in
`treeQueryHelpersInitChildren`, no connection yet). With
`GetFirstNoInit`/`GetNextNoInit` instead, tested on 4.8 together with
d8815d6cb0 and 43fd7bce86 and with Heidi's workaround disabled: no crash, node
heights and header scale correctly between 100% and 175% monitors. Open
point as well: `SetNodeHeight` ignores trees with `toReadOnly`, so their node
heights stay unscaled.

No local patch: Heidi keeps its workaround in
`TExtForm.FixTreeFontsOnDPIChange` (`source/extra_controls.pas`, recalculates
`DefaultNodeHeight`, resets `NodeHeight` of all initialized nodes, syncs the
header font). When upgrading Lazarus: check whether the release contains
405b441432 with the NoInit iteration; if so, test removing the workaround.

Performance (measured 2026-11-14, Lazarus 4.8, Win64, 20 columns): the DPI
change itself is O(n) already without any NodeHeight fix, about 120 ms per
1 mio nodes. Cause: `TVTHeader.AutoAdjustLayout` sets `col.Width` per column
→ `TVirtualTreeColumn.SetWidth` → `TBaseVirtualTree.DoColumnResize`, which
walks all nodes just to reset `vsHeightMeasured` on multiline nodes, so one
full traversal per column. `GetFirstInitialized`/`GetNextInitialized` also
visit every node, so limiting a NodeHeight reset to initialized nodes saves
almost nothing. Resetting `NodeHeight` of all nodes costs about 150 ms per
1 mio nodes. Possible upstream improvement: set the column widths inside
`Columns.BeginUpdate`/`EndUpdate` and call `DoColumnResize` once, or skip the
loop when `toMultiline` is off.

## Open item: VT header font PPI (issue #2413)

`TVTHeader.Font` is a TPersistent font that neither LCL DPI scaling nor
`TVTHeader.ChangeScale` keeps at the monitor PPI (startup on high-DPI and
lazily created frames affected). Heidi works this around in
`TExtForm.FixTreeFontsOnDPIChange` (`source/extra_controls.pas`: sets
`VT.Header.Font.PixelsPerInch/Height`), the render side is covered by 0011's
header hunk. A VT-side fix attempt (flipping `TVTHeader.ParentFont` default to
True) failed on 4.8: the CM_PARENTFONTCHANGED forward in the tree is guarded
by `not csLoading`, so the header font never resyncs during startup scaling;
that guard's purpose is unanalyzed.

When upgrading to Lazarus 5.0: re-check whether the header font still renders
unscaled at startup on high-DPI monitors (local dpi-test program, lazy-frame
repro via unit2). If
still broken, create a new patch (e.g. `0013-virtualtreeview-header-parentfont`,
header font sync incl. removing the csLoading guard) and then drop the
`VT.Header.Font.*` lines from `TExtForm.FixTreeFontsOnDPIChange` so the
concern lives in one place only.
