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
