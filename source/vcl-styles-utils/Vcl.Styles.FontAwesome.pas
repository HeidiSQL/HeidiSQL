//**************************************************************************************************
//
// Unit Vcl.Styles.FontAwesome
// unit for the VCL Styles Utils
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
// The Original Code is Vcl.Styles.FontAwesome.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2020 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Vcl.Styles.FontAwesome;

interface

{$IF CompilerVersion >= 30.0}

 {$DEFINE WinXCtrls}
{$ENDIF}

uses
  Winapi.GDIPOBJ,
  Winapi.GDIPAPI,
  Winapi.Windows,
  System.Classes,
  {$IFDEF WinXCtrls}
  Vcl.WinXCtrls,
  {$ENDIF}
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Graphics;


{$R AwesomeFont.RES}

//http://fortawesome.github.io/Font-Awesome/cheatsheet/
//http://prettyprinter.de/index.php

//version 4.7.0
const
  fa_glass = $F000;
  fa_music = $F001;
  fa_search = $F002;
  fa_envelope_o = $F003;
  fa_heart = $F004;
  fa_star = $F005;
  fa_star_o = $F006;
  fa_user = $F007;
  fa_film = $F008;
  fa_th_large = $F009;
  fa_th = $F00A;
  fa_th_list = $F00B;
  fa_check = $F00C;
  fa_remove = $F00D;
  fa_search_plus = $F00E;
  fa_search_minus = $F010;
  fa_power_off = $F011;
  fa_signal = $F012;
  fa_gear = $F013;
  fa_trash_o = $F014;
  fa_home = $F015;
  fa_file_o = $F016;
  fa_clock_o = $F017;
  fa_road = $F018;
  fa_download = $F019;
  fa_arrow_circle_o_down = $F01A;
  fa_arrow_circle_o_up = $F01B;
  fa_inbox = $F01C;
  fa_play_circle_o = $F01D;
  fa_rotate_right = $F01E;
  fa_refresh = $F021;
  fa_list_alt = $F022;
  fa_lock = $F023;
  fa_flag = $F024;
  fa_headphones = $F025;
  fa_volume_off = $F026;
  fa_volume_down = $F027;
  fa_volume_up = $F028;
  fa_qrcode = $F029;
  fa_barcode = $F02A;
  fa_tag = $F02B;
  fa_tags = $F02C;
  fa_book = $F02D;
  fa_bookmark = $F02E;
  fa_print = $F02F;
  fa_camera = $F030;
  fa_font = $F031;
  fa_bold = $F032;
  fa_italic = $F033;
  fa_text_height = $F034;
  fa_text_width = $F035;
  fa_align_left = $F036;
  fa_align_center = $F037;
  fa_align_right = $F038;
  fa_align_justify = $F039;
  fa_list = $F03A;
  fa_dedent = $F03B;
  fa_indent = $F03C;
  fa_video_camera = $F03D;
  fa_photo = $F03E;
  fa_pencil = $F040;
  fa_map_marker = $F041;
  fa_adjust = $F042;
  fa_tint = $F043;
  fa_edit = $F044;
  fa_share_square_o = $F045;
  fa_check_square_o = $F046;
  fa_arrows = $F047;
  fa_step_backward = $F048;
  fa_fast_backward = $F049;
  fa_backward = $F04A;
  fa_play = $F04B;
  fa_pause = $F04C;
  fa_stop = $F04D;
  fa_forward = $F04E;
  fa_fast_forward = $F050;
  fa_step_forward = $F051;
  fa_eject = $F052;
  fa_chevron_left = $F053;
  fa_chevron_right = $F054;
  fa_plus_circle = $F055;
  fa_minus_circle = $F056;
  fa_times_circle = $F057;
  fa_check_circle = $F058;
  fa_question_circle = $F059;
  fa_info_circle = $F05A;
  fa_crosshairs = $F05B;
  fa_times_circle_o = $F05C;
  fa_check_circle_o = $F05D;
  fa_ban = $F05E;
  fa_arrow_left = $F060;
  fa_arrow_right = $F061;
  fa_arrow_up = $F062;
  fa_arrow_down = $F063;
  fa_mail_forward = $F064;
  fa_expand = $F065;
  fa_compress = $F066;
  fa_plus = $F067;
  fa_minus = $F068;
  fa_asterisk = $F069;
  fa_exclamation_circle = $F06A;
  fa_gift = $F06B;
  fa_leaf = $F06C;
  fa_fire = $F06D;
  fa_eye = $F06E;
  fa_eye_slash = $F070;
  fa_warning = $F071;
  fa_plane = $F072;
  fa_calendar = $F073;
  fa_random = $F074;
  fa_comment = $F075;
  fa_magnet = $F076;
  fa_chevron_up = $F077;
  fa_chevron_down = $F078;
  fa_retweet = $F079;
  fa_shopping_cart = $F07A;
  fa_folder = $F07B;
  fa_folder_open = $F07C;
  fa_arrows_v = $F07D;
  fa_arrows_h = $F07E;
  fa_bar_chart_o = $F080;
  fa_twitter_square = $F081;
  fa_facebook_square = $F082;
  fa_camera_retro = $F083;
  fa_key = $F084;
  fa_gears = $F085;
  fa_comments = $F086;
  fa_commenting  = $F27a;
  fa_commenting_o = $f27b;
  fa_thumbs_o_up = $F087;
  fa_thumbs_o_down = $F088;
  fa_star_half = $F089;
  fa_heart_o = $F08A;
  fa_sign_out = $F08B;
  fa_linkedin_square = $F08C;
  fa_thumb_tack = $F08D;
  fa_external_link = $F08E;
  fa_sign_in = $F090;
  fa_trophy = $F091;
  fa_github_square = $F092;
  fa_upload = $F093;
  fa_lemon_o = $F094;
  fa_phone = $F095;
  fa_square_o = $F096;
  fa_bookmark_o = $F097;
  fa_phone_square = $F098;
  fa_twitter = $F099;
  fa_facebook_f = $F09A;
  fa_github = $F09B;
  fa_unlock = $F09C;
  fa_credit_card = $F09D;
  fa_rss = $F09E;
  fa_hdd_o = $F0A0;
  fa_bullhorn = $F0A1;
  fa_bell = $F0F3;
  fa_certificate = $F0A3;
  fa_hand_o_right = $F0A4;
  fa_hand_o_left = $F0A5;
  fa_hand_o_up = $F0A6;
  fa_hand_o_down = $F0A7;
  fa_arrow_circle_left = $F0A8;
  fa_arrow_circle_right = $F0A9;
  fa_arrow_circle_up = $F0AA;
  fa_arrow_circle_down = $F0AB;
  fa_globe = $F0AC;
  fa_wrench = $F0AD;
  fa_tasks = $F0AE;
  fa_filter = $F0B0;
  fa_clone = $F24D;
  fa_briefcase = $F0B1;
  fa_arrows_alt = $F0B2;
  fa_group = $F0C0;
  fa_chain = $F0C1;
  fa_cloud = $F0C2;
  fa_flask = $F0C3;
  fa_cut = $F0C4;
  fa_copy = $F0C5;
  fa_paperclip = $F0C6;
  fa_save = $F0C7;
  fa_square = $F0C8;
  fa_navicon = $F0C9;
  fa_list_ul = $F0CA;
  fa_list_ol = $F0CB;
  fa_strikethrough = $F0CC;
  fa_underline = $F0CD;
  fa_table = $F0CE;
  fa_magic = $F0D0;
  fa_truck = $F0D1;
  fa_pinterest = $F0D2;
  fa_pinterest_square = $F0D3;
  fa_google_plus_square = $F0D4;
  fa_google_plus = $F0D5;
  fa_money = $F0D6;
  fa_caret_down = $F0D7;
  fa_caret_up = $F0D8;
  fa_caret_left = $F0D9;
  fa_caret_right = $F0DA;
  fa_columns = $F0DB;
  fa_unsorted = $F0DC;
  fa_sort_down = $F0DD;
  fa_sort_up = $F0DE;
  fa_envelope = $F0E0;
  fa_linkedin = $F0E1;
  fa_rotate_left = $F0E2;
  fa_legal = $F0E3;
  fa_dashboard = $F0E4;
  fa_comment_o = $F0E5;
  fa_comments_o = $F0E6;
  fa_flash = $F0E7;
  fa_sitemap = $F0E8;
  fa_umbrella = $F0E9;
  fa_paste = $F0EA;
  fa_lightbulb_o = $F0EB;
  fa_exchange = $F0EC;
  fa_cloud_download = $F0ED;
  fa_cloud_upload = $F0EE;
  fa_user_md = $F0F0;
  fa_stethoscope = $F0F1;
  fa_suitcase = $F0F2;
  fa_bell_o = $F0A2;
  fa_coffee = $F0F4;
  fa_cutlery = $F0F5;
  fa_file_text_o = $F0F6;
  fa_building_o = $F0F7;
  fa_hospital_o = $F0F8;
  fa_ambulance = $F0F9;
  fa_medkit = $F0FA;
  fa_fighter_jet = $F0FB;
  fa_beer = $F0FC;
  fa_h_square = $F0FD;
  fa_plus_square = $F0FE;
  fa_angle_double_left = $F100;
  fa_angle_double_right = $F101;
  fa_angle_double_up = $F102;
  fa_angle_double_down = $F103;
  fa_angle_left = $F104;
  fa_angle_right = $F105;
  fa_angle_up = $F106;
  fa_angle_down = $F107;
  fa_desktop = $F108;
  fa_mouse_pointer = $F245;
  fa_laptop = $F109;
  fa_tablet = $F10A;
  fa_mobile_phone = $F10B;
  fa_circle_o = $F10C;
  fa_quote_left = $F10D;
  fa_quote_right = $F10E;
  fa_spinner = $F110;
  fa_circle = $F111;
  fa_mail_reply = $F112;
  fa_github_alt = $F113;
  fa_folder_o = $F114;
  fa_folder_open_o = $F115;
  fa_smile_o = $F118;
  fa_frown_o = $F119;
  fa_meh_o = $F11A;
  fa_gamepad = $F11B;
  fa_keyboard_o = $F11C;
  fa_flag_o = $F11D;
  fa_flag_checkered = $F11E;
  fa_terminal = $F120;
  fa_code = $F121;
  fa_mail_reply_all = $F122;
  fa_star_half_empty = $F123;
  fa_location_arrow = $F124;
  fa_crop = $F125;
  fa_code_fork = $F126;
  fa_unlink = $F127;
  fa_question = $F128;
  fa_info = $F129;
  fa_exclamation = $F12A;
  fa_superscript = $F12B;
  fa_subscript = $F12C;
  fa_eraser = $F12D;
  fa_puzzle_piece = $F12E;
  fa_microphone = $F130;
  fa_microphone_slash = $F131;
  fa_shield = $F132;
  fa_calendar_o = $F133;
  fa_fire_extinguisher = $F134;
  fa_rocket = $F135;
  fa_maxcdn = $F136;
  fa_chevron_circle_left = $F137;
  fa_chevron_circle_right = $F138;
  fa_chevron_circle_up = $F139;
  fa_chevron_circle_down = $F13A;
  fa_html5 = $F13B;
  fa_css3 = $F13C;
  fa_anchor = $F13D;
  fa_unlock_alt = $F13E;
  fa_bullseye = $F140;
  fa_ellipsis_h = $F141;
  fa_ellipsis_v = $F142;
  fa_rss_square = $F143;
  fa_play_circle = $F144;
  fa_ticket = $F145;
  fa_minus_square = $F146;
  fa_minus_square_o = $F147;
  fa_level_up = $F148;
  fa_level_down = $F149;
  fa_check_square = $F14A;
  fa_pencil_square = $F14B;
  fa_external_link_square = $F14C;
  fa_share_square = $F14D;
  fa_compass = $F14E;
  fa_toggle_down = $F150;
  fa_toggle_up = $F151;
  fa_toggle_right = $F152;
  fa_euro = $F153;
  fa_gbp = $F154;
  fa_dollar = $F155;
  fa_rupee = $F156;
  fa_cny = $F157;
  fa_ruble = $F158;
  fa_won = $F159;
  fa_bitcoin = $F15A;
  fa_file = $F15B;
  fa_file_text = $F15C;
  fa_sort_alpha_asc = $F15D;
  fa_sort_alpha_desc = $F15E;
  fa_sort_amount_asc = $F160;
  fa_sort_amount_desc = $F161;
  fa_sort_numeric_asc = $F162;
  fa_sort_numeric_desc = $F163;
  fa_thumbs_up = $F164;
  fa_thumbs_down = $F165;
  fa_youtube_square = $F166;
  fa_youtube = $F167;
  fa_xing = $F168;
  fa_xing_square = $F169;
  fa_youtube_play = $F16A;
  fa_dropbox = $F16B;
  fa_stack_overflow = $F16C;
  fa_instagram = $F16D;
  fa_flickr = $F16E;
  fa_adn = $F170;
  fa_bitbucket = $F171;
  fa_bitbucket_square = $F172;
  fa_tumblr = $F173;
  fa_tumblr_square = $F174;
  fa_long_arrow_down = $F175;
  fa_long_arrow_up = $F176;
  fa_long_arrow_left = $F177;
  fa_long_arrow_right = $F178;
  fa_apple = $F179;
  fa_windows = $F17A;
  fa_window_maximize = $f2d0;
  fa_android = $F17B;
  fa_linux = $F17C;
  fa_dribbble = $F17D;
  fa_skype = $F17E;
  fa_foursquare = $F180;
  fa_trello = $F181;
  fa_female = $F182;
  fa_male = $F183;
  fa_gittip = $F184;
  fa_sun_o = $F185;
  fa_moon_o = $F186;
  fa_archive = $F187;
  fa_bug = $F188;
  fa_vk = $F189;
  fa_weibo = $F18A;
  fa_renren = $F18B;
  fa_pagelines = $F18C;
  fa_stack_exchange = $F18D;
  fa_arrow_circle_o_right = $F18E;
  fa_arrow_circle_o_left = $F190;
  fa_toggle_left = $F191;
  fa_dot_circle_o = $F192;
  fa_wheelchair = $F193;
  fa_vimeo_square = $F194;
  fa_turkish_lira = $F195;
  fa_plus_square_o = $F196;
  fa_space_shuttle = $F197;
  fa_slack = $F198;
  fa_envelope_square = $F199;
  fa_wordpress = $F19A;
  fa_openid = $F19B;
  fa_institution = $F19C;
  fa_mortar_board = $F19D;
  fa_yahoo = $F19E;
  fa_google = $F1A0;
  fa_reddit = $F1A1;
  fa_reddit_square = $F1A2;
  fa_stumbleupon_circle = $F1A3;
  fa_stumbleupon = $F1A4;
  fa_delicious = $F1A5;
  fa_digg = $F1A6;
  fa_pied_piper = $F1A7;
  fa_pied_piper_alt = $F1A8;
  fa_drupal = $F1A9;
  fa_joomla = $F1AA;
  fa_language = $F1AB;
  fa_fax = $F1AC;
  fa_building = $F1AD;
  fa_child = $F1AE;
  fa_paw = $F1B0;
  fa_spoon = $F1B1;
  fa_cube = $F1B2;
  fa_cubes = $F1B3;
  fa_behance = $F1B4;
  fa_behance_square = $F1B5;
  fa_steam = $F1B6;
  fa_steam_square = $F1B7;
  fa_recycle = $F1B8;
  fa_automobile = $F1B9;
  fa_cab = $F1BA;
  fa_tree = $F1BB;
  fa_spotify = $F1BC;
  fa_deviantart = $F1BD;
  fa_soundcloud = $F1BE;
  fa_database = $F1C0;
  fa_file_pdf_o = $F1C1;
  fa_file_word_o = $F1C2;
  fa_file_excel_o = $F1C3;
  fa_file_powerpoint_o = $F1C4;
  fa_file_photo_o  = $F1C5;
  fa_file_zip_o = $F1C6;
  fa_file_sound_o = $F1C7;
  fa_file_movie_o = $F1C8;
  fa_file_code_o = $F1C9;
  fa_vine = $F1CA;
  fa_codepen = $F1CB;
  fa_jsfiddle = $F1CC;
  fa_life_bouy = $F1CD;
  fa_circle_o_notch = $F1CE;
  fa_ra = $F1D0;
  fa_ge = $F1D1;
  fa_git_square = $F1D2;
  fa_git = $F1D3;
  fa_hacker_news = $F1D4;
  fa_tencent_weibo = $F1D5;
  fa_qq = $F1D6;
  fa_wechat = $F1D7;
  fa_send = $F1D8;
  fa_send_o = $F1D9;
  fa_history = $F1DA;
  fa_genderless = $F1DB;
  fa_header = $F1DC;
  fa_paragraph = $F1DD;
  fa_sliders = $F1DE;
  fa_share_alt = $F1E0;
  fa_share_alt_square = $F1E1;
  fa_bomb = $F1E2;
  fa_soccer_ball_o = $F1E3;
  fa_tty = $F1E4;
  fa_binoculars = $F1E5;
  fa_plug = $F1E6;
  fa_slideshare = $F1E7;
  fa_twitch = $F1E8;
  fa_yelp = $F1E9;
  fa_newspaper_o = $F1EA;
  fa_wifi = $F1EB;
  fa_calculator = $F1EC;
  fa_paypal = $F1ED;
  fa_google_wallet = $F1EE;
  fa_cc_visa = $F1F0;
  fa_cc_mastercard = $F1F1;
  fa_cc_discover = $F1F2;
  fa_cc_amex = $F1F3;
  fa_cc_paypal = $F1F4;
  fa_cc_stripe = $F1F5;
  fa_bell_slash = $F1F6;
  fa_bell_slash_o = $F1F7;
  fa_trash = $F1F8;
  fa_copyright = $F1F9;
  fa_at = $F1FA;
  fa_eyedropper = $F1FB;
  fa_paint_brush = $F1FC;
  fa_birthday_cake = $F1FD;
  fa_area_chart = $F1FE;
  fa_pie_chart = $F200;
  fa_line_chart = $F201;
  fa_lastfm = $F202;
  fa_lastfm_square = $F203;
  fa_toggle_off = $F204;
  fa_toggle_on = $F205;
  fa_bicycle = $F206;
  fa_bus = $F207;
  fa_ioxhost = $F208;
  fa_angellist = $F209;
  fa_cc = $F20A;
  fa_shekel = $F20B;
  fa_meanpath = $F20C;
  fa_buysellads = $F20D;
  fa_connectdevelop = $F20E;
  fa_dashcube = $F210;
  fa_forumbee = $F211;
  fa_leanpub = $F212;
  fa_sellsy = $F213;
  fa_shirtsinbulk = $F214;
  fa_simplybuilt = $F215;
  fa_skyatlas = $F216;
  fa_cart_plus = $F217;
  fa_cart_arrow_down = $F218;
  fa_diamond = $F219;
  fa_ship = $F21A;
  fa_user_secret = $F21B;
  fa_motorcycle = $F21C;
  fa_street_view = $F21D;
  fa_heartbeat = $F21E;
  fa_venus = $F221;
  fa_mars = $F222;
  fa_mercury = $F223;
  fa_transgender = $F224;
  fa_transgender_alt = $F225;
  fa_venus_double = $F226;
  fa_mars_double = $F227;
  fa_venus_mars = $F228;
  fa_mars_stroke = $F229;
  fa_mars_stroke_v = $F22A;
  fa_mars_stroke_h = $F22B;
  fa_neuter = $F22C;
  fa_facebook_official = $F230;
  fa_pinterest_p = $F231;
  fa_whatsapp = $F232;
  fa_server = $F233;
  fa_user_plus = $F234;
  fa_user_times = $F235;
  fa_hotel = $F236;
  fa_viacoin = $F237;
  fa_train = $F238;
  fa_subway = $F239;
  fa_medium = $F23A;

type
  //http://fortawesome.github.io/Font-Awesome/cheatsheet/
  TFontAwesome = class
  private
    FPrivateFontCollection : TGPPrivateFontCollection;
    procedure LoadFontFromResource;
  public
    constructor Create;
    Destructor Destroy; override;
    procedure DrawChar(DC: HDC; const AChar: Char; DestRect: TRect; AColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft); overload;
    procedure DrawChar(DC: HDC; const AChar: Char; DestRect: TRect; AFontHeight : Integer; AColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft); overload;

    procedure DrawChar(DC: HDC; const ACode: Word; DestRect: TRect; AColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft); overload;
    procedure DrawChar(DC: HDC; const ACode: Word; DestRect: TRect; AFontHeight : Integer; AColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft); overload;

    function  GetIcon(const ACode: Word; Width, Height : Integer; AColor, ABackColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft) : HICON; overload;
    function  GetIcon(const ACode: Word; Width, Height, CharX, CharY : Integer; AColor, ABackColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft) : HICON; overload;
  end;
  {$IFDEF WinXCtrls}
  TFontAwesomeAnimated = class(TCustomActivityIndicator)
  private
    FFontAwesomeCode : Word;
    FColor, FBackColor : TColor;
    procedure SetFontAwesomeCode(const Value: Word);
    procedure SetColor(const Value: TColor);
    procedure SetBackColor(const Value: TColor);
  protected
    procedure ReloadFrames; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Anchors;
    property Animate;
    property FrameDelay;
    property IndicatorColor;
    property IndicatorSize;
    property IndicatorType;
    property Color : TColor read FColor write SetColor;
    property BackColor : TColor read FBackColor write SetBackColor;
    property FontAwesomeCode : Word read FFontAwesomeCode write SetFontAwesomeCode;
  end;
  {$ENDIF}

var
  FontAwesome :  TFontAwesome;


implementation

uses
 Winapi.Messages,
 System.SysUtils,
 System.Math,
 Vcl.ExtCtrls,
 Vcl.Forms,
 Vcl.ImgList,
 Vcl.Themes,
 Vcl.Styles.Utils.Graphics;

{ TFontLoader }

constructor TFontAwesome.Create;
begin
  inherited;
//  FFontHandle := 0;
//  FDefaultQuality := ANTIALIASED_QUALITY;
  FPrivateFontCollection := nil;
  LoadFontFromResource();
end;

destructor TFontAwesome.Destroy;
begin
//  if (FFontHandle <> 0) then
//    RemoveFontMemResourceEx(FFontHandle);

  if (FPrivateFontCollection <> nil) then
   FPrivateFontCollection.Free;
  inherited;
end;

procedure TFontAwesome.LoadFontFromResource;
var
 LStream : TResourceStream;
 LStatus : TStatus;
 cFonts: DWord;
begin
  LStream := TResourceStream.Create(HInstance, 'fontawesome', RT_RCDATA);
  try
    FPrivateFontCollection := TGPPrivateFontCollection.Create;

    // We HAVE to do this to register the font to the system (Weird .NET bug !)
    cFonts:= 0;
    AddFontMemResourceEx(LStream.Memory, Cardinal(LStream.Size), nil, @cFonts);

    LStatus := FPrivateFontCollection.AddMemoryFont(LStream.Memory, LStream.Size);
    if (LStatus <> Status.Ok) then
       RaiseLastOSError();
  finally
    LStream.Free;
  end;
end;


function TFontAwesome.GetIcon(const ACode: Word; Width, Height, CharX,
  CharY: Integer; AColor, ABackColor: TColor; Orientation: Integer;
  ImageAlignment: TImageAlignment): HICON;
var
  LIconInfo: TIconInfo;
  LBitmap, LMask: TBitmap;
  NewIcon: HICON;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.PixelFormat := pf32bit;
    LBitmap.Canvas.Brush.Color := ABackColor;
    LBitmap.SetSize(Width, Height);
    //LBitmap.Canvas.FillRect(Rect(0, 0, LBitmap.Width, LBitmap.Height));
    //Bitmap32_SetAlphaAndColor(LBitmap, 255, clFuchsia);

    //DrawChar(LBitmap.Canvas.Handle, ACode, Rect(0, 0, LBitmap.Width, LBitmap.Height), AColor, Orientation, ImageAlignment);
    DrawChar(LBitmap.Canvas.Handle, ACode, Rect(0, 0, Width, Height), CharY, AColor, Orientation, ImageAlignment);
    Bitmap32_SetAlphaExceptColor(LBitmap, 255, ABackColor);
    LBitmap.AlphaFormat := afDefined;

    LMask := TBitmap.Create;
    try
      //LMask.Handle:=CreateBitmap(LBitmap.Width, LBitmap.Height, 1, 1, 0);
      LMask.PixelFormat := pf1bit;
      LMask.SetSize(Width, Height);

      LIconInfo.fIcon := True;
      LIconInfo.xHotspot := Width;
      LIconInfo.yHotspot := Height;
      LIconInfo.hbmMask := LMask.Handle;
      LIconInfo.hbmColor := LBitmap.Handle;

      NewIcon := CreateIconIndirect(LIconInfo);
      Result  := NewIcon;
    finally
      LMask.Free;
    end;
  finally
    LBitmap.Free;
  end;
end;


function TFontAwesome.GetIcon(const ACode: Word; Width, Height: Integer; AColor, ABackColor: TColor; Orientation: Integer = 0; ImageAlignment: TImageAlignment = iaLeft): HICON;
begin
  Result := GetIcon(ACode, Width, Height, Width, Height, AColor, ABackColor, Orientation, ImageAlignment);
end;

procedure TFontAwesome.DrawChar(DC: HDC; const ACode: Word; DestRect: TRect; AColor: TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft);
begin
  DrawChar(DC, Chr(ACode), DestRect, AColor, Orientation, ImageAlignment);
end;


procedure TFontAwesome.DrawChar(DC: HDC; const AChar: Char; DestRect: TRect; AColor: TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft);
begin
  DrawChar(DC, AChar, DestRect, DestRect.Height, AColor, Orientation, ImageAlignment);
end;

procedure TFontAwesome.DrawChar(DC: HDC; const ACode: Word; DestRect: TRect;
  AFontHeight: Integer; AColor: TColor; Orientation: Integer;
  ImageAlignment: TImageAlignment);
begin
  DrawChar(DC, Chr(ACode), DestRect, AFontHeight, AColor, Orientation, ImageAlignment);
end;

procedure TFontAwesome.DrawChar(DC: HDC; const AChar: Char; DestRect: TRect;
  AFontHeight: Integer; AColor: TColor; Orientation: Integer;
  ImageAlignment: TImageAlignment);
var
 LFont : TGPFont;
 LGPGraphics : TGPGraphics;
 LBrush : TGPSolidBrush;
 LColor : Cardinal;
 LGPStringFormat : TGPStringFormat;
 LRect: TGPRectF;
begin
  LGPGraphics := TGPGraphics.Create(DC);
  try
    LFont := TGPFont.Create('FontAwesome', AFontHeight, FontStyleRegular, UnitPixel, FPrivateFontCollection);
    try
      LColor     := ColorToRGB(AColor);
      LRect := MakeRect(DestRect.Left * 1.0, DestRect.Top * 1.0, DestRect.Width * 1.0, DestRect.Height * 1.0);

      LBrush := TGPSolidBrush.Create(MakeColor(255, GetRValue(LColor), GetGValue(LColor), GetBValue(LColor)));
      try
        LGPStringFormat := TGPStringFormat.Create();
        try
          LGPStringFormat.SetAlignment(StringAlignmentCenter);
          //LGPStringFormat.SetLineAlignment(TStringAlignment.StringAlignmentCenter);
          LGPGraphics.DrawString(AChar, -1, LFont, LRect, LGPStringFormat, LBrush);
        finally
          LGPStringFormat.Free;
        end;
      finally
       LBrush.Free;
      end;
    finally
      LFont.Free;
    end;
  finally
    LGPGraphics.Free;
  end;
end;

//var
//  LogFont: TLogFont;
//  AFont  : HFONT;
//  pOldFont: HGDIOBJ;
//  LColorRef: COLORREF;
//  OldMode: integer;
//  uFormat : Cardinal;
//begin
//  if FFontHandle = 0 then exit;
//
//  ZeroMemory(@LogFont, SizeOf(LogFont));
//  LogFont.lfHeight := DestRect.Height;
//  LogFont.lfWidth := 0;
//  LogFont.lfEscapement := Orientation * 10;
//  LogFont.lfOrientation := LogFont.lfEscapement;
//  LogFont.lfWeight := FW_NORMAL;
//  LogFont.lfItalic := 0;
//  LogFont.lfUnderline := 0;
//  LogFont.lfStrikeOut := 0;
//  LogFont.lfCharSet := DEFAULT_CHARSET;
//  LogFont.lfOutPrecision := OUT_OUTLINE_PRECIS;//OUT_STROKE_PRECIS;
//  LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
//  LogFont.lfQuality := FDefaultQuality;
//  LogFont.lfPitchAndFamily := DEFAULT_PITCH;
//  LogFont.lfFaceName := 'FontAwesome';
//
//  LColorRef := ColorToRGB(AColor);
//
//  AFont := CreateFontIndirect(LogFont);
//  if AFont <> 0 then
//    try
//      LColorRef := SetTextColor(DC, LColorRef);
//      pOldFont := SelectObject(DC, AFont);
//      try
//        OldMode := SetBkMode(DC, TRANSPARENT);
//        uFormat := DT_SINGLELINE;
//
//        case ImageAlignment of
//         iaLeft   : uFormat := uFormat or DT_LEFT;
//         iaRight  : uFormat := uFormat or DT_RIGHT;
//         iaCenter : uFormat := uFormat or DT_CENTER;
//         iaTop    : uFormat := uFormat or DT_TOP;
//         iaBottom : uFormat := uFormat or DT_BOTTOM;
//        end;
//
//        uFormat := uFormat or DT_NOCLIP;
//
//        Winapi.Windows.DrawText(DC, AChar, 1, DestRect, uFormat);
//        SetBkMode(DC, OldMode);
//        SelectObject(DC, LColorRef);
//      finally
//        if pOldFont <> 0 then
//          SelectObject(DC, pOldFont);
//      end;
//    finally
//      DeleteObject(AFont);
//    end;
//end;


{$IFDEF WinXCtrls}
{ TFontAwesomeAnimated }
type
  TCustomActivityIndicatorShadow = class(TCustomControl)
  private
    FAnimate: Boolean;
    FIndicatorColor: TActivityIndicatorColor;
    FIndicatorSize: TActivityIndicatorSize;
    FIndicatorType: TActivityIndicatorType;
    FFrameDelay: Word;
    FFrameIndex: Integer;
    FTimer: TTimer;
    FFrameList: TImageList;
    FFrameCount: Integer;
    FFrameSize: Integer;
    FFrameBitmap: TBitmap;
    FLoadedFrames: Boolean;
  end;

procedure DrawParentImage(Control: TControl; DC: HDC; InvalidateParent: Boolean = False);
var
  SaveIndex: Integer;
  P: TPoint;
begin
  if Control.Parent = nil then
    Exit;
  SaveIndex := SaveDC(DC);
  GetViewportOrgEx(DC, P);

  SetViewportOrgEx(DC, P.X - Control.Left, P.Y - Control.Top, nil);
  IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);

  Control.Parent.Perform(WM_ERASEBKGND, DC, 0);
  Control.Parent.Perform(WM_PRINTCLIENT, DC, prf_Client);

  RestoreDC(DC, SaveIndex);

  if InvalidateParent then
  begin
    if not (Control.Parent is TCustomControl) and not (Control.Parent is TCustomForm) and
       not (csDesigning in Control.ComponentState) then
    begin
      Control.Parent.Invalidate;
    end;
  end;
end;

constructor TFontAwesomeAnimated.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFontAwesomeCode := fa_spinner;
  FColor := StyleServices.GetSystemColor(clBtnText);
  FBackColor := StyleServices.GetSystemColor(clBtnFace);
end;

procedure RotateBitmap(ABitmap: TBitmap; Degs: Integer; Resize: Boolean; ABackColor: TColor = clNone);
var
  LGPBitmap: TGPBitmap;
  LMatrix: TGPMatrix;
  C, S: Single;
  LSize: TSize;
  LGPGraphics: TGPGraphics;
begin
  LGPBitmap := TGPBitmap.Create(ABitmap.Handle, ABitmap.Palette);
  try
    LMatrix := TGPMatrix.Create;
    try
      LMatrix.RotateAt(Degs, MakePoint(0.5 * ABitmap.Width, 0.5 * ABitmap.Height));
      if Resize then
      begin
        C := Cos(DegToRad(Degs));
        S := Sin(DegToRad(Degs));
        LSize.cx := Round(ABitmap.Width * Abs(C) + ABitmap.Height * Abs(S));
        LSize.cy := Round(ABitmap.Width * Abs(S) + ABitmap.Height * Abs(C));
        ABitmap.Width := LSize.cx;
        ABitmap.Height := LSize.cy;
      end;

      LGPGraphics := TGPGraphics.Create(ABitmap.Canvas.Handle);
      try
        LGPGraphics.Clear(ColorRefToARGB(ColorToRGB(ABackColor)));
        LGPGraphics.SetTransform(LMatrix);
        LGPGraphics.DrawImage(LGPBitmap, (Cardinal(ABitmap.Width) - LGPBitmap.GetWidth) div 2,
          (Cardinal(ABitmap.Height) - LGPBitmap.GetHeight) div 2);
      finally
        LGPGraphics.Free;
      end;

    finally
      LMatrix.Free;
    end;
  finally
    LGPBitmap.Free;
  end;
end;

procedure TFontAwesomeAnimated.ReloadFrames;
var
  i, LFrameSize, LAngleDelta, LAngle : Integer;
  LBitmap : TBitmap;
begin
  TCustomActivityIndicatorShadow(Self).FFrameSize  := 48;
  LFrameSize := TCustomActivityIndicatorShadow(Self).FFrameSize;
  TCustomActivityIndicatorShadow(Self).FFrameCount := 24; //optimize
  TCustomActivityIndicatorShadow(Self).FFrameBitmap.SetSize(LFrameSize, LFrameSize);
  LAngleDelta := 360 div TCustomActivityIndicatorShadow(Self).FFrameCount;

  //TCustomActivityIndicatorShadow(Self).FFrameList.ColorDepth := cd24Bit;
  TCustomActivityIndicatorShadow(Self).FFrameList.Width := LFrameSize;
  TCustomActivityIndicatorShadow(Self).FFrameList.Height := LFrameSize;

  TCustomActivityIndicatorShadow(Self).FFrameList.Clear;
  LAngle := 0;
  for i := 0 to TCustomActivityIndicatorShadow(Self).FFrameCount - 1 do
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.PixelFormat := pf32bit;
      LBitmap.AlphaFormat := afDefined;
      LBitmap.SetSize(LFrameSize, LFrameSize);
      Bitmap32_SetAlphaAndColor(LBitmap, 0, FBackColor);
      Bitmap32_SetAlpha(LBitmap, 0);
      FontAwesome.DrawChar(LBitmap.Canvas.Handle, Char(FFontAwesomeCode), Rect(0, 0, LFrameSize, LFrameSize), FColor, 0, TImageAlignment.iaCenter);
      if (LAngle > 0) then
        RotateBitmap(LBitmap, LAngle, False, FBackColor);
      Inc(LAngle, LAngleDelta);
     TCustomActivityIndicatorShadow(Self).FFrameList.Add(LBitmap, nil);
    finally
      LBitmap.Free;
    end;
  end;
  TCustomActivityIndicatorShadow(Self).FLoadedFrames := True;
end;

procedure TFontAwesomeAnimated.Resize;
begin
  SetBounds(Left, Top, TCustomActivityIndicatorShadow(Self).FFrameSize, TCustomActivityIndicatorShadow(Self).FFrameSize);
end;

procedure TFontAwesomeAnimated.SetBackColor(const Value: TColor);
var
  SaveAnimate: Boolean;
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    SaveAnimate := Animate;
    Animate := False;
    ReloadFrames;
    Animate := SaveAnimate;
  end;
end;

procedure TFontAwesomeAnimated.SetColor(const Value: TColor);
var
  SaveAnimate: Boolean;
begin
  if FColor <> Value then
  begin
    FColor := Value;
    SaveAnimate := Animate;
    Animate := False;
    ReloadFrames;
    Animate := SaveAnimate;
  end;
end;

procedure TFontAwesomeAnimated.SetFontAwesomeCode(const Value: Word);
var
  SaveAnimate: Boolean;
begin
  if FFontAwesomeCode <> Value then
  begin
    FFontAwesomeCode := Value;
    SaveAnimate := Animate;
    Animate := False;
    ReloadFrames;
    Animate := SaveAnimate;
  end;
end;
{$ENDIF}

initialization
  FontAwesome := TFontAwesome.Create;
finalization
  FontAwesome.Free;
end.
