# MetaDarkStyle snapshot: HeidiSQL notes

`src/` is a copy of the upstream files (https://github.com/zamtmn/metadarkstyle,
`src/`, current as of merge 57b8544). Not vendored: `CustomDark.*`, `dsgn/`,
`udarkstyleschemesadditional.pas`. Keep it as close to upstream as possible:
prefer fixes outside the snapshot, keep unavoidable local changes minimal and
offer them upstream.

## Refreshing the snapshot

Copy the files from upstream `src/` (EOL: upstream stores LF; keep CRLF in the
working tree, autocrlf is set), then re-apply open local deviations, if any.

## Local deviations

None (2026-11): `src/` matches upstream 57b8544.

## Merged upstream

Per-DPI theme handles: https://github.com/zamtmn/metadarkstyle/pull/95
(merged as 57b8544).
- Dark main menu band at per-monitor DPI > 96. Since Lazarus 3.0 the LCL draws
  the menu with per-DPI theme handles (`TWin32ThemeServices.ThemeForPPI`), which
  `DrawThemeBackgroundDark` / `DrawThemeTextDark` did not recognize. `IsTheme`
  also compares against `ThemeForPPI[Element, PPI]` for each monitor's PPI.
  Earlier attempts with an LCL patch and an `OpenThemeDataForDpi` intercept were
  dropped as unnecessary.
- The `teButton` branch of `DrawThemeBackgroundDark` uses `IsTheme`, needed by
  Lazarus patch 0012 (`DrawElementForPPI`, Lazarus issue
  https://gitlab.com/freepascal.org/lazarus/lazarus/-/work_items/42717); a no-op
  with an unpatched LCL.

Statusbar clipping (psText clipped via `TextRect`, `OnDrawPanel` routed through
the paint DC): https://github.com/zamtmn/metadarkstyle/pull/94, reported as
https://github.com/zamtmn/metadarkstyle/issues/93.

## Requirements

Exes need a comctl32 v6 manifest (Lazarus "Use XP manifest", `{$R *.res}` in the
lpr), else `SetWindowSubclass` fails silently with process exit code 127.
