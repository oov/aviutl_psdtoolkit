#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "ovbase.h"
#include "ovutil/str.h"
#include "ovutil/win32.h"

#include "aviutl.h"
#include "cache.h"
#include "error_ptk.h"
#include "find.h"
#include "ipc.h"
#include "luafuncs.h"
#include "luastr.h"
#include "popupmenu.h"
#include "ptklayer.h"
#include "setting.h"
#include "speak.h"
#include "version.h"

static HMODULE g_bridge = NULL;
static HWND g_ptk_window = 0;
enum {
  WM_PTK_UPDATE_CURRENT_PROJECT_PATH = WM_APP + 1,
  WM_PTK_RECEIVE_UPDATE_EDITING_IMAGE_STATE = WM_APP + 2,
  WM_PTK_RECEIVE_EXPORT_FAVIEW_SLIDER = WM_APP + 3,
  WM_PTK_RECEIVE_EXPORT_LAYER_NAMES = WM_APP + 4,
};

NODISCARD static error bridge_dll_init(void) {
  struct wstr tmp = {0};
  error err = get_module_file_name(get_hinstance(), &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  size_t fnpos = 0;
  err = extract_file_name(&tmp, &fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp.ptr[fnpos] = L'\0';
  tmp.len = fnpos;
  err = scat(&tmp, L"script\\PSDToolKit\\PSDToolKitBridge.dll");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  g_bridge = LoadLibraryW(tmp.ptr);
  if (!g_bridge) {
    HRESULT const hr = HRESULT_FROM_WIN32(GetLastError());
    if (hr == HRESULT_FROM_WIN32(ERROR_MOD_NOT_FOUND)) {
      err = err(err_type_ptk, err_ptk_bridge_cannot_load);
    } else {
      err = errhr(hr);
    }
    goto cleanup;
  }
  typedef void (*init)(lua_CFunction impl);
  init fn = (init)(void *)GetProcAddress(g_bridge, "init");
  if (!fn) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  fn(luaopen_PSDToolKitBridge);

cleanup:
  if (efailed(err)) {
    if (g_bridge) {
      FreeLibrary(g_bridge);
      g_bridge = NULL;
    }
  }
  ereport(sfree(&tmp));
  return err;
}

static void bridge_dll_exit(void) {
  if (g_bridge) {
    FreeLibrary(g_bridge);
    g_bridge = NULL;
  }
}

NODISCARD static error clear_files(void) {
  error err = ipc_clear_files(g_ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD static error update_current_project_path(void) {
  struct wstr tmp = {0};
  struct str utf8 = {0};
  error err = aviutl_get_project_path(&tmp);
  if (efailed(err)) {
    if (!eis(err, err_type_ptk, err_ptk_project_is_not_open) &&
        !eis(err, err_type_ptk, err_ptk_project_has_not_yet_been_saved)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = scpy(&g_current_project_path, tmp.ptr ? tmp.ptr : L"");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = to_utf8(&g_current_project_path, &utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = ipc_update_current_project_path(g_ipc, &utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&utf8));
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error show_gui(void) {
  uint64_t w = 0;
  error err = ipc_show_gui(g_ipc, &w);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  g_ptk_window = (HANDLE)w;
  SetForegroundWindow(g_ptk_window);
cleanup:
  return err;
}

static void ipc_on_update_editing_image_state_callback(void *const userdata,
                                                       struct ipc_update_editing_image_state_params *const params) {
  HWND const window = userdata;
  struct ipc_update_editing_image_state_params *p = NULL;
  error err = mem(&p, 1, sizeof(struct ipc_update_editing_image_state_params));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *p = *params;
  *params = (struct ipc_update_editing_image_state_params){0};
cleanup:
  if (efailed(err)) {
    PostMessageW(window, WM_PTK_RECEIVE_UPDATE_EDITING_IMAGE_STATE, 0, (LPARAM)err);
  } else {
    PostMessageW(window, WM_PTK_RECEIVE_UPDATE_EDITING_IMAGE_STATE, (WPARAM)p, 0);
  }
}

static void ipc_on_export_faview_slider_callback(void *const userdata,
                                                 struct ipc_export_faview_slider_params *const params) {
  HWND const window = userdata;
  struct ipc_export_faview_slider_params *p = NULL;
  error err = mem(&p, 1, sizeof(struct ipc_export_faview_slider_params));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *p = *params;
  *params = (struct ipc_export_faview_slider_params){0};
cleanup:
  if (efailed(err)) {
    PostMessageW(window, WM_PTK_RECEIVE_EXPORT_FAVIEW_SLIDER, 0, (LPARAM)err);
  } else {
    PostMessageW(window, WM_PTK_RECEIVE_EXPORT_FAVIEW_SLIDER, (WPARAM)p, 0);
  }
}
static void ipc_on_export_layer_names_callback(void *const userdata,
                                               struct ipc_export_layer_names_params *const params) {
  HWND const window = userdata;
  struct ipc_export_layer_names_params *p = NULL;
  error err = mem(&p, 1, sizeof(struct ipc_export_layer_names_params));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *p = *params;
  *params = (struct ipc_export_layer_names_params){0};
cleanup:
  if (efailed(err)) {
    PostMessageW(window, WM_PTK_RECEIVE_EXPORT_LAYER_NAMES, 0, (LPARAM)err);
  } else {
    PostMessageW(window, WM_PTK_RECEIVE_EXPORT_LAYER_NAMES, (WPARAM)p, 0);
  }
}

NODISCARD static error setup_ipc(FILTER *const fp) {
  struct wstr tmp = {0};
  struct wstr tmp2 = {0};
  error err = get_module_file_name(get_hinstance(), &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  size_t fnpos = 0;
  err = extract_file_name(&tmp, &fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp.ptr[fnpos] = L'\0';
  tmp.len = fnpos;
  err = scpym(&tmp2, L"\"", tmp.ptr, L"script\\PSDToolKit\\PSDToolKit.exe\"");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = ipc_init(&g_ipc,
                 &(struct ipc_options){
                     .cmdline = tmp2,
                     .userdata = fp->hwnd,
                     .on_update_editing_image_state = ipc_on_update_editing_image_state_callback,
                     .on_export_faview_slider = ipc_on_export_faview_slider_callback,
                     .on_export_layer_names = ipc_on_export_layer_names_callback,
                 });
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp2));
  ereport(sfree(&tmp));
  return err;
}

struct wav_item {
  AVI_FILE_HANDLE afh;
  size_t samples;
};

NODISCARD static error
wav_open(void *const userdata, struct str const *const filepath, int const freq, int const ch, void **const h) {
  FILTER *const fp = userdata;
  struct wav_item *wi = NULL;

  error err = mem(&wi, 1, sizeof(struct wav_item));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  FILE_INFO fi = {0};
  wi->afh = fp->exfunc->avi_file_open(filepath->ptr, &fi, AVI_FILE_OPEN_FLAG_AUDIO_ONLY);
  if (!wi->afh) {
    err = errg(err_fail);
    goto cleanup;
  }
  if (fi.audio_rate != freq || fi.audio_ch != ch) {
    wi->samples = (size_t)fp->exfunc->avi_file_set_audio_sample_rate(wi->afh, freq, ch);
  } else {
    wi->samples = (size_t)fi.audio_n;
  }
  *h = wi;
cleanup:
  if (efailed(err)) {
    ereport(mem_free(&wi));
  }
  return err;
}

NODISCARD
static error wav_read(void *const userdata,
                      void *const h,
                      size_t const offset,
                      size_t const length,
                      int16_t *const buf,
                      size_t *const samples) {
  FILTER *const fp = userdata;
  struct wav_item *const wi = h;
  if (offset >= wi->samples) {
    *samples = 0;
    goto cleanup;
  }
  size_t const real_samples = offset + length >= wi->samples ? wi->samples - offset : length;
  if (real_samples < 8) {
    // avi_file_read_audio_sample seems to crash if you request a very small number of samples.
    // It should be almost inaudible, so treat it as zero.
    *samples = 0;
    goto cleanup;
  }
  *samples = (size_t)fp->exfunc->avi_file_read_audio_sample(wi->afh, (int)offset, (int)real_samples, buf);
cleanup:
  return eok();
}

static void wav_close(void *const userdata, void *const h) {
  FILTER *const fp = userdata;
  struct wav_item *wi = h;
  fp->exfunc->avi_file_close(wi->afh);
  ereport(mem_free(&wi));
}

NODISCARD static error build_error_message(error e, wchar_t const *const main_message, struct wstr *const dest) {
  struct wstr tmp = {0};
  struct wstr msg = {0};
  error err = eok();
  if (e == NULL) {
    err = scpy(dest, main_message);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    goto cleanup;
  }
  err = error_to_string(e, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpym(&msg, main_message, L"\r\n\r\n", tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(dest, msg.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&msg));
  ereport(sfree(&tmp));
  return err;
}

static void error_message_box(error e, HWND const window, wchar_t const *const msg) {
  struct wstr errmsg = {0};
  error err = build_error_message(e, msg, &errmsg);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  message_box(window, errmsg.ptr, L"PSDToolKit " VERSION_WIDE, MB_ICONERROR);

cleanup:
  ereport(sfree(&errmsg));
  if (efailed(err)) {
    ereportmsg(err, &native_unmanaged(NSTR("エラーダイアログの表示に失敗しました。")));
  }
  efree(&e);
}

#define ERRMSG_INIT L"PSDToolKit の初期化中にエラーが発生しました。"
static BOOL filter_init(FILTER *const fp) {
  aviutl_set_pointers(fp, NULL);
  error err = aviutl_init();
  if (efailed(err)) {
    wchar_t const *msg = NULL;
    if (eis(err, err_type_ptk, err_ptk_arch_is_not_64bit)) {
      msg = ERRMSG_INIT L"\r\n\r\n"
                        L"PSDToolKit を使うには 64bit 版の Windows が必要です。";
      efree(&err);
    } else if (eis(err, err_type_ptk, err_ptk_unsupported_aviutl_version)) {
      msg = ERRMSG_INIT L"\r\n\r\n"
                        L"PSDToolKit を使うには AviUtl version 1.00 以降が必要です。";
      efree(&err);
    } else if (eis(err, err_type_ptk, err_ptk_exedit_not_found)) {
      msg = ERRMSG_INIT L"\r\n\r\n"
                        L"拡張編集プラグインが見つかりません。";
      efree(&err);
    } else if (eis(err, err_type_ptk, err_ptk_exedit_not_found_in_same_dir)) {
      msg = ERRMSG_INIT L"\r\n\r\n"
                        L"インストール状態が正しくありません。\r\n"
                        L"付属のドキュメントに従ってインストールしてください。";
      efree(&err);
    } else if (eis(err, err_type_ptk, err_ptk_unsupported_exedit_version)) {
      msg = ERRMSG_INIT L"\r\n\r\n"
                        L"PSDToolKit を使うには 拡張編集 version 0.92 以降が必要です。";
      efree(&err);
    } else {
      msg = ERRMSG_INIT;
    }
    error_message_box(err, fp->hwnd, msg);
    return FALSE;
  }
  err = cache_init();
  if (efailed(err)) {
    error_message_box(err, fp->hwnd, ERRMSG_INIT);
    return FALSE;
  }
  err = speak_init(
      &(struct speak_callbacks){
          .userdata = fp,
          .open = wav_open,
          .read = wav_read,
          .close = wav_close,
      },
      &g_speak);
  if (efailed(err)) {
    error_message_box(err, fp->hwnd, ERRMSG_INIT);
    return FALSE;
  }
  err = setup_ipc(fp);
  if (efailed(err)) {
    error_message_box(err, fp->hwnd, ERRMSG_INIT);
    return FALSE;
  }
  err = bridge_dll_init();
  if (efailed(err)) {
    wchar_t const *msg = NULL;
    if (eis(err, err_type_ptk, err_ptk_bridge_cannot_load)) {
      msg = ERRMSG_INIT L"\r\n\r\n"
                        L"PSDToolKitBridge.dll の読み込みに失敗しました。\r\n"
                        L"インストール状態が正しくありません。\r\n"
                        L"付属のドキュメントに従ってインストールしてください。";
      efree(&err);
    } else {
      msg = ERRMSG_INIT;
    }
    error_message_box(err, fp->hwnd, msg);
    return FALSE;
  }
  err = aviutl_add_menu_item(&wstr_unmanaged_const(L"ウィンドウを表示"), fp->hwnd, 1, 'W', ADD_MENU_ITEM_FLAG_KEY_CTRL);
  if (efailed(err)) {
    ereportmsg(errg(err_fail), &native_unmanaged(NSTR("メニュー項目「ウィンドウを表示」の登録に失敗しました。")));
  }
  err = aviutl_add_menu_item(&wstr_unmanaged_const(L"環境設定"), fp->hwnd, 2, 0, 0);
  if (efailed(err)) {
    ereportmsg(errg(err_fail), &native_unmanaged(NSTR("メニュー項目「環境設定」の登録に失敗しました。")));
  }
  return TRUE;
}

static void ipc_friendly_error_message_box(error e, HWND const window, wchar_t const *msg) {
  if (eis(e, err_type_ptk, err_ptk_ipc_target_not_found)) {
    efree(&e);
    msg = L"PSDToolKit.exe が見つかりませんでした。\r\n\r\n"
          L"インストールファイルが破損している可能性があります。\r\n"
          L"PSDToolKit を再インストールしてみてください。";
  } else if (eis(e, err_type_ptk, err_ptk_ipc_target_access_denied)) {
    efree(&e);
    msg = L"PSDToolKit.exe へのアクセスが拒否されました。\r\n\r\n"
          L"このエラーはアンチウィルスソフトが PSDToolKit.exe を\r\n"
          L"誤ブロックしているのが原因で発生することがあります。\r\n\r\n"
          L"アンチウィルスソフトがプログラム実行を阻害していないか確認してください。";
  }

  error_message_box(e, window, msg);
}

static BOOL filter_exit(FILTER *const fp) {
  (void)fp;
  cache_exit();
  if (g_ipc) {
    ipc_exit(&g_ipc);
  }
  if (g_speak) {
    speak_exit(&g_speak);
  }
  bridge_dll_exit();
  ereport(sfree(&g_current_project_path));
  if (aviutl_initalized()) {
    ereportmsg(aviutl_exit(), &native_unmanaged(NSTR("終了処理に失敗しました。")));
  }
  return TRUE;
}

static BOOL wndproc_command(HWND window, WPARAM wparam) {
  error err = eok();
  switch (wparam) {
  case 1:
    err = show_gui();
    break;
  case 2:
    err = show_setting();
    break;
  default:
    err = errg(err_unexpected);
    break;
  }
  if (efailed(err)) {
    ipc_friendly_error_message_box(err, window, L"エラーが発生しました。");
  }
  return TRUE;
}

static void reset(HWND window) {
  error err = clear_files();
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = update_current_project_path();
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  if (efailed(err)) {
    ipc_friendly_error_message_box(err, window, L"エラーが発生しました。");
  }
}

static void wndproc_update_current_project_path(HWND window) {
  error err = update_current_project_path();
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  if (efailed(err)) {
    ipc_friendly_error_message_box(err, window, L"エラーが発生しました。");
  }
}

static void wndproc_receive_update_editing_image_state(struct ipc_update_editing_image_state_params *params,
                                                       error err) {
  struct wstr file_path = {0};
  struct wstr state = {0};
  struct wstr text = {0};
  struct wstr old_file_path = {0};
  struct wstr tmp = {0};
  struct wstr tmp2 = {0};
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = from_utf8(&params->file_path_utf8, &file_path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = from_utf8(&params->state_utf8, &state);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  HWND wnd = 0;
  HWND edit_control = 0;
  err = find_exedit_multi_line_text(&wstr_unmanaged_const(L"ptkf=\""), &wnd, &edit_control);
  if (efailed(err)) {
    if (eisg(err, err_not_found)) {
      efree(&err);
      err = err(err_type_ptk, err_ptk_target_psd_file_object_not_found);
      goto cleanup;
    }
    err = ethru(err);
    goto cleanup;
  }
  err = get_window_text(edit_control, &text);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  size_t pos = 0, len = 0;
  err = luastr_find_assign(&text, &wstr_unmanaged_const(L"ptkf"), &pos, &len);
  if (efailed(err)) {
    if (eisg(err, err_not_found)) {
      efree(&err);
      err = err(err_type_ptk, err_ptk_target_variable_not_found);
      goto cleanup;
    }
    err = ethru(err);
    goto cleanup;
  }
  err = luastr_decode(&(struct wstr const){.ptr = text.ptr + pos + 5, .len = len - 5}, &old_file_path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luastr_encode(&file_path, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sncpy(&tmp2, text.ptr, pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scatm(&tmp2, L"ptkf=", tmp.ptr, text.ptr + pos + len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(&text, tmp2.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luastr_find_assign(&text, &wstr_unmanaged_const(L"ptkl"), &pos, &len);
  if (efailed(err)) {
    if (eisg(err, err_not_found)) {
      efree(&err);
      err = err(err_type_ptk, err_ptk_target_variable_not_found);
      goto cleanup;
    }
    err = ethru(err);
    goto cleanup;
  }
  err = luastr_encode(&state, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sncpy(&tmp2, text.ptr, (size_t)pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scatm(&tmp2, L"ptkl=", tmp.ptr, text.ptr + pos + len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(&text, tmp2.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (wcscmp(old_file_path.ptr, file_path.ptr) != 0) {
    err = scpym(&tmp2,
                L"送り先には別のPSDファイルが割り当てられています。\r\n"
                L"本当に続行しますか？\r\n\r\n"
                L"現在のPSDファイルオブジェクト:\r\n",
                old_file_path.ptr,
                L"\r\n\r\n"
                L"割り当てられるPSDファイル:\r\n",
                file_path.ptr);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    HWND *disabled_windows = NULL;
    err = disable_family_windows(g_ptk_window, &disabled_windows);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    int const r = MessageBoxW(
        g_ptk_window, tmp2.ptr, L"確認 - PSDToolKit " VERSION_WIDE, MB_ICONWARNING | MB_OKCANCEL | MB_DEFBUTTON2);
    restore_disabled_family_windows(disabled_windows);
    if (r == IDCANCEL) {
      goto cleanup;
    }
  }
  SetWindowTextW(edit_control, text.ptr);
  SendMessageW(wnd, WM_COMMAND, (WPARAM)MAKELONG(GetDlgCtrlID(edit_control), EN_CHANGE), (LPARAM)edit_control);
cleanup:
  ereport(sfree(&tmp2));
  ereport(sfree(&tmp));
  ereport(sfree(&old_file_path));
  ereport(sfree(&text));
  ereport(sfree(&state));
  ereport(sfree(&file_path));
  if (params) {
    ereport(sfree(&params->state_utf8));
    ereport(sfree(&params->file_path_utf8));
    ereport(mem_free(&params));
  }
  if (efailed(err)) {
    wchar_t const *msg = NULL;
    if (eis(err, err_type_ptk, err_ptk_target_psd_file_object_not_found)) {
      msg = L"設定の送信先になるテキスト入力欄が見つかりませんでした。\r\n\r\n"
            L"「送る」ボタンを使用するにはPSDファイルオブジェクトの\r\n"
            L"複数行テキスト入力欄を見える状態にしておく必要があります。\r\n\r\n"
            L"詳しくは付属マニュアルのチュートリアルを参照してください。";
      efree(&err);
    } else if (eis(err, err_type_ptk, err_ptk_target_variable_not_found)) {
      msg = L"テキスト入力欄から書き換え対象テキストが見つかりませんでした。\r\n\r\n"
            L"PSDファイルオブジェクトに最初から含まれている\r\n"
            L"「ptkf=\"～\"」や「ptkl=\"～\"」などのテキストは\r\n"
            L"削除してしまうと正しく動作しなくなります。";
      efree(&err);
    } else {
      msg = L"「送る」ボタンで画像を送信する際にエラーが発生しました。";
    }
    ipc_friendly_error_message_box(err, g_ptk_window, msg);
  }
}

static void
wndproc_receive_export_faview_slider(HWND const window, struct ipc_export_faview_slider_params *params, error err) {
  struct ptklayer pi = {0};
  struct popup_menu pm = {0};
  struct wstr name = {0};
  struct wstr tmp = {0};
  struct wstr tmp2 = {0};
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = ptklayer_parse(
      &params->names_utf8, &params->values_utf8, &params->slider_name_utf8, &params->file_path_utf8, &pi);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  struct multi_parameter_dialog mpd = {0};
  err = find_exedit_multi_parameter_dialog(&mpd);
  if (efailed(err)) {
    if (eisg(err, err_not_found)) {
      efree(&err);
    } else {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = ptklayer_get_readable_item_name(&pi, (size_t)params->selected_index, &name);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpym(&tmp, L"\"", name.ptr, L"\" をクリップボードにコピー");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = apush(&pm, ((struct popup_menu_item){.caption = tmp, .id = 1000}));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp = (struct wstr){0};
  err = apush(&pm, ((struct popup_menu_item){0}));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  for (size_t i = 0; i < mpd.len; ++i) {
    err = get_window_text(mpd.label[i], &tmp2);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = scpym(&tmp, L"\"", name.ptr, L"\" を「", tmp2.ptr, L"」に割り当て");
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = apush(&pm, ((struct popup_menu_item){.caption = tmp, .id = 2000 + (int)i}));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    tmp = (struct wstr){0};
  }
  if (mpd.len) {
    err = apush(&pm, ((struct popup_menu_item){0}));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = ptklayer_get_readable_group_name(&pi, (size_t)params->selected_index, &name);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpym(&tmp, L"スライダー \"", name.ptr, L"\" 全体をクリップボードにコピー");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = apush(&pm, ((struct popup_menu_item){.caption = tmp, .id = 1001}));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp = (struct wstr){0};
  err = scpym(&tmp, L"スライダー \"", name.ptr, L"\" 全体をファイルにエクスポート");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = apush(&pm, ((struct popup_menu_item){.caption = tmp, .id = 1002}));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp = (struct wstr){0};

  int selected = 0;
  err = popup_menu_show(g_ptk_window, get_hinstance(), &pm, &selected);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  switch (selected) {
  case 0:
    goto cleanup;
  case 1000:
    err = ptklayer_copy_to_clipboard_item(&pi, window, (size_t)params->selected_index);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  case 1001:
    err = ptklayer_copy_to_clipboard_siblings(&pi, window, (size_t)params->selected_index);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  case 1002:
    err = ptklayer_save_to_file_siblings(&pi, g_ptk_window, (size_t)params->selected_index);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  default:
    if (selected >= 2000 && (size_t)(selected - 2000) < mpd.len) {
      err = ptklayer_send_item(&pi, mpd.edit[selected - 2000], (size_t)params->selected_index);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
    }
    break;
  }
cleanup:
  ereport(sfree(&tmp2));
  ereport(sfree(&tmp));
  ereport(sfree(&name));
  ptklayer_free(&pi);
  popup_menu_free(&pm);
  if (params) {
    ereport(sfree(&params->values_utf8));
    ereport(sfree(&params->names_utf8));
    ereport(sfree(&params->slider_name_utf8));
    ereport(sfree(&params->file_path_utf8));
    ereport(mem_free(&params));
  }
  if (efailed(err)) {
    wchar_t const *msg = NULL;
    msg = L"シンプルビューのエクスポート中にエラーが発生しました。";
    ipc_friendly_error_message_box(err, g_ptk_window, msg);
  }
}

static void
wndproc_receive_export_layer_names(HWND const window, struct ipc_export_layer_names_params *params, error err) {
  struct ptklayer pi = {0};
  struct popup_menu pm = {0};
  struct wstr name = {0};
  struct wstr tmp = {0};
  struct wstr tmp2 = {0};
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = ptklayer_parse(&params->names_utf8, &params->values_utf8, NULL, &params->file_path_utf8, &pi);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  struct multi_parameter_dialog mpd = {0};
  err = find_exedit_multi_parameter_dialog(&mpd);
  if (efailed(err)) {
    if (eisg(err, err_not_found)) {
      efree(&err);
    } else {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = ptklayer_get_readable_item_name(&pi, (size_t)params->selected_index, &name);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpym(&tmp, L"\"", name.ptr, L"\" をクリップボードにコピー");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = apush(&pm, ((struct popup_menu_item){.caption = tmp, .id = 1000}));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp = (struct wstr){0};
  err = apush(&pm, ((struct popup_menu_item){0}));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  for (size_t i = 0; i < mpd.len; ++i) {
    err = get_window_text(mpd.label[i], &tmp2);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = scpym(&tmp, L"\"", name.ptr, L"\" を「", tmp2.ptr, L"」に割り当て");
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = apush(&pm, ((struct popup_menu_item){.caption = tmp, .id = 2000 + (int)i}));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    tmp = (struct wstr){0};
  }
  if (mpd.len) {
    err = apush(&pm, ((struct popup_menu_item){0}));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = scpym(&tmp, L"\"", name.ptr, L"\" と同じ階層のレイヤー全てをクリップボードにコピー");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = apush(&pm, ((struct popup_menu_item){.caption = tmp, .id = 1001}));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp = (struct wstr){0};
  err = scpym(&tmp, L"\"", name.ptr, L"\" と同じ階層のレイヤー全てをファイルにエクスポート");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = apush(&pm, ((struct popup_menu_item){.caption = tmp, .id = 1002}));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp = (struct wstr){0};

  int selected = 0;
  err = popup_menu_show(g_ptk_window, get_hinstance(), &pm, &selected);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  switch (selected) {
  case 0:
    goto cleanup;
  case 1000:
    err = ptklayer_copy_to_clipboard_item(&pi, window, (size_t)params->selected_index);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  case 1001:
    err = ptklayer_copy_to_clipboard_siblings(&pi, window, (size_t)params->selected_index);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  case 1002:
    err = ptklayer_save_to_file_siblings(&pi, g_ptk_window, (size_t)params->selected_index);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  default:
    if (selected >= 2000 && (size_t)(selected - 2000) < mpd.len) {
      err = ptklayer_send_item(&pi, mpd.edit[selected - 2000], (size_t)params->selected_index);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
    }
    break;
  }
cleanup:
  ereport(sfree(&tmp2));
  ereport(sfree(&tmp));
  ereport(sfree(&name));
  ptklayer_free(&pi);
  popup_menu_free(&pm);
  if (params) {
    ereport(sfree(&params->values_utf8));
    ereport(sfree(&params->names_utf8));
    ereport(sfree(&params->file_path_utf8));
    ereport(mem_free(&params));
  }
  if (efailed(err)) {
    wchar_t const *msg = NULL;
    msg = L"レイヤー名のエクスポート中にエラーが発生しました。";
    ipc_friendly_error_message_box(err, g_ptk_window, msg);
  }
}

static BOOL filter_proc(FILTER *const fp, FILTER_PROC_INFO *fpip) {
  (void)fp;
  g_current_frame = fpip->frame;
  g_current_frame_n = fpip->frame_n;
  ++g_current_render_index;
  return TRUE;
}

static BOOL filter_project_load(FILTER *const fp, void *const editp, void *const data, int const size) {
  aviutl_set_pointers(fp, editp);
  error err = ipc_deserialize(g_ipc,
                              &(struct str){
                                  .ptr = data,
                                  .len = (size_t)size,
                              });
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  if (efailed(err)) {
    ereportmsg(err, &native_unmanaged_const(NSTR("データの読み込み中にエラーが発生しました。")));
    return FALSE;
  }
  return TRUE;
}

static BOOL filter_project_save(FILTER *const fp, void *const editp, void *const data, int *const size) {
  struct str tmp = {0};
  aviutl_set_pointers(fp, editp);
  error err = ipc_serialize(g_ipc, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *size = (int)tmp.len;
  // filter_project_save will be called twice.
  // In the first call, data == NULL.
  if (data) {
    memcpy(data, tmp.ptr, tmp.len);
    // filename may change after saving.
    PostMessage(fp->hwnd, WM_PTK_UPDATE_CURRENT_PROJECT_PATH, 0, 0);
  }
cleanup:
  ereport(sfree(&tmp));
  if (efailed(err)) {
    ereportmsg(err, &native_unmanaged_const(NSTR("データの保存中にエラーが発生しました。")));
    return FALSE;
  }
  return TRUE;
}

static BOOL wndproc(HWND const window,
                    UINT const message,
                    WPARAM const wparam,
                    LPARAM const lparam,
                    void *const editp,
                    FILTER *const fp) {
  (void)window;
  (void)lparam;
  aviutl_set_pointers(fp, editp);
  switch (message) {
  case WM_FILTER_INIT:
    CreateWindowExW(0,
                    L"STATIC",
                    L"まずは説明書を読みましょう。",
                    WS_CHILD | WS_VISIBLE | ES_LEFT,
                    0,
                    0,
                    400,
                    32,
                    window,
                    0,
                    get_hinstance(),
                    NULL);
    return FALSE;
  case WM_FILTER_FILE_OPEN:
    reset(window);
    return FALSE;
  case WM_FILTER_FILE_CLOSE:
    reset(window);
    return FALSE;
  case WM_FILTER_COMMAND:
    return wndproc_command(window, wparam);
  case WM_PTK_UPDATE_CURRENT_PROJECT_PATH:
    wndproc_update_current_project_path(window);
    return FALSE;
  case WM_PTK_RECEIVE_UPDATE_EDITING_IMAGE_STATE:
    wndproc_receive_update_editing_image_state((struct ipc_update_editing_image_state_params *)wparam, (error)lparam);
    return FALSE;
  case WM_PTK_RECEIVE_EXPORT_FAVIEW_SLIDER:
    wndproc_receive_export_faview_slider(window, (struct ipc_export_faview_slider_params *)wparam, (error)lparam);
    return FALSE;
  case WM_PTK_RECEIVE_EXPORT_LAYER_NAMES:
    wndproc_receive_export_layer_names(window, (struct ipc_export_layer_names_params *)wparam, (error)lparam);
    return FALSE;
  }
  return FALSE;
}

FILTER_DLL __declspec(dllexport) * *__stdcall GetFilterTableList(void);
FILTER_DLL __declspec(dllexport) * *__stdcall GetFilterTableList(void) {
  static FILTER_DLL g_filter_dll = {
      .flag = FILTER_FLAG_ALWAYS_ACTIVE | FILTER_FLAG_EX_INFORMATION | FILTER_FLAG_PRIORITY_HIGHEST,
      .name = "PSDToolKit",
      .func_init = filter_init,
      .func_exit = filter_exit,
      .func_proc = filter_proc,
      .func_WndProc = wndproc,
      .information = "PSDToolKit " VERSION,
      .func_project_load = filter_project_load,
      .func_project_save = filter_project_save,
  };
  static FILTER_DLL *filter_list[] = {&g_filter_dll, NULL};
  return (FILTER_DLL **)&filter_list;
}

static void
error_reporter(error const e, struct NATIVE_STR const *const message, struct ovbase_filepos const *const filepos) {
  struct NATIVE_STR tmp = {0};
  struct NATIVE_STR msg = {0};
  error err = error_to_string(e, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  NATIVE_CHAR buf[1024] = {0};
  wsprintfW(buf, NSTR("\r\n(reported at %hs:%ld %hs())\r\n"), filepos->file, filepos->line, filepos->func);
  err = scpym(&msg, message->ptr, buf, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  OutputDebugStringW(msg.ptr);

cleanup:
  if (efailed(err)) {
    OutputDebugStringW(NSTR("failed to report error"));
    efree(&err);
  }
  eignore(sfree(&msg));
  eignore(sfree(&tmp));
}

static BOOL main_init(HINSTANCE const inst) {
  if (!ovbase_init()) {
    return FALSE;
  }
  error_register_reporter(error_reporter);
  ereportmsg(error_ptk_init(), &native_unmanaged(NSTR("エラーメッセージマッパーの登録に失敗しました。")));
  set_hinstance(inst);
  return TRUE;
}

static BOOL main_exit(void) {
  ovbase_exit();
  return TRUE;
}

BOOL WINAPI DllMain(HINSTANCE const inst, DWORD const reason, LPVOID const reserved);
BOOL WINAPI DllMain(HINSTANCE const inst, DWORD const reason, LPVOID const reserved) {
  (void)reserved;
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    return main_init(inst);
  case DLL_PROCESS_DETACH:
    return main_exit();
  }
  return TRUE;
}
