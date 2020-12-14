//**************************************************************************************************
//
// Vcl.Styles.Utils.inc
// file for the VCL Styles Utils
// https://github.com/RRUZ/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
//
// Portions created by Mahdi Safsafi [SMP3]   e-mail SMP@LIVE.FR
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2020 Rodrigo Ruz V.
// All Rights Reserved.
//
//************************************************************************************************

{$DEFINE USE_Vcl.Styles.Hooks}
{$DEFINE HOOK_UXTHEME}
{$DEFINE HOOK_TDateTimePicker}
{$DEFINE HOOK_TProgressBar}
{$DEFINE LimitStylesToMainApplicationThread}
{.$DEFINE HOOK_VirtualShell}

{ Feature toggles for supported UxThemes - see Vcl.Styles.UxTheme }

{$DEFINE HOOK_Button}
{$DEFINE HOOK_AllButtons}
{$DEFINE HOOK_Scrollbar}
{$DEFINE HOOK_TaskDialog}
{$DEFINE HOOK_ProgressBar}
{$DEFINE HOOK_DateTimePicker}
{$DEFINE HOOK_TreeView}
{$DEFINE HOOK_ListView}
{$DEFINE HOOK_ListBox}
{$DEFINE HOOK_ComboBox}

{$DEFINE HOOK_Spin}
{$DEFINE HOOK_EDIT}
{$DEFINE HOOK_Rebar}
{$DEFINE HOOK_ToolBar}
{$DEFINE HOOK_Menu}
{$DEFINE HOOK_TrackBar}
{$DEFINE HOOK_ToolTip}
{$DEFINE HOOK_Tab}
// Introduced in Windows Vista
{$DEFINE HOOK_CommandModule}
{$DEFINE HOOK_SearchBox}
{$DEFINE HOOK_AddressBand}
{$DEFINE HOOK_PreviewPane}
{$DEFINE HOOK_TRYHARDER}
{$DEFINE HOOK_BREADCRUMBAR}
{$DEFINE HOOK_InfoBar}
// Introduced in Windows 8
{$DEFINE HOOK_Navigation}
