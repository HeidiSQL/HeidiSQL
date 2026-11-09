# MetaDarkStyle snapshot: HeidiSQL notes

`src/` is a copy of the upstream files (https://github.com/zamtmn/metadarkstyle,
`src/`, current as of merge 8bb9d32). Keep it as close to upstream as possible:
prefer fixes outside the snapshot, keep unavoidable local changes minimal and
offer them upstream.

## Refreshing the snapshot

Copy the files from upstream `src/` (EOL: upstream stores LF; keep CRLF in the
working tree, autocrlf is set), then re-apply the open local deviations below.

## Local deviation (2026-11): per-DPI theme handles

Offered upstream as https://github.com/zamtmn/metadarkstyle/pull/95 (branch
`fix-menu-bar-per-monitor-dpi`). `uwin32widgetsetdark.pas` is exactly upstream
8bb9d32 + that PR's diff. Once merged, refresh the snapshot and the deviation is
gone.

- Commit 1: dark main menu band at per-monitor DPI > 96. Since Lazarus 3.0 the
  LCL draws the menu with per-DPI theme handles
  (`TWin32ThemeServices.ThemeForPPI`), which `DrawThemeBackgroundDark` /
  `DrawThemeTextDark` did not recognize. New `IsTheme` also compares against
  `ThemeForPPI[Element, PPI]` for each monitor's PPI. Earlier attempts with an
  LCL patch and an `OpenThemeDataForDpi` intercept were dropped as unnecessary.
- Commit 2: the `teButton` branch of `DrawThemeBackgroundDark` uses `IsTheme`,
  needed by Lazarus patch 0012 (`DrawElementForPPI`, Lazarus issue
  https://gitlab.com/freepascal.org/lazarus/lazarus/-/work_items/42717); a no-op
  with an unpatched LCL.

## Merged upstream

Statusbar clipping (psText clipped via `TextRect`, `OnDrawPanel` routed through
the paint DC): https://github.com/zamtmn/metadarkstyle/pull/94, reported as
https://github.com/zamtmn/metadarkstyle/issues/93.

## Requirements

Exes need a comctl32 v6 manifest (Lazarus "Use XP manifest", `{$R *.res}` in the
lpr), else `SetWindowSubclass` fails silently with process exit code 127.
