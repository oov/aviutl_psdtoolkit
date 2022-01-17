#include "error_ptk.h"

NODISCARD static error get_ptk_message(uint_least32_t const code, struct NATIVE_STR *const message) {
  switch (code) {
  case err_ptk_arch_is_not_64bit:
    return scpy(message, NSTR("動作には64Bit版の Windows が必要です。"));
  case err_ptk_unsupported_aviutl_version:
    return scpy(message, NSTR("AviUtl のバージョンが動作対象外です。"));
  case err_ptk_exedit_not_found:
    return scpy(message, NSTR("拡張編集プラグインが見つかりません。"));
  case err_ptk_exedit_not_found_in_same_dir:
    return scpy(message, NSTR("拡張編集プラグインが同じフォルダー内に見つかりません。"));
  case err_ptk_lua51_cannot_load:
    return scpy(message, NSTR("lua51.dll の読み込みに失敗しました。"));
  case err_ptk_bridge_cannot_load:
    return scpy(message, NSTR("PSDToolKitBridge.dll の読み込みに失敗しました。"));
  case err_ptk_unsupported_exedit_version:
    return scpy(message, NSTR("拡張編集プラグインのバージョンが動作対象外です。"));
  case err_ptk_project_is_not_open:
    return scpy(message, NSTR("AviUtlのプロジェクトファイル(*.aup)が開かれていません。"));
  case err_ptk_project_has_not_yet_been_saved:
    return scpy(message, NSTR("AviUtlのプロジェクトファイル(*.aup)がまだ保存されていません。"));
  case err_ptk_target_psd_file_object_not_found:
    return scpy(message, NSTR("設定の送信先になるテキスト入力欄が見つかりませんでした。"));
  case err_ptk_target_variable_not_found:
    return scpy(message, NSTR("書き換える対象の変数が見つかりませんでした。"));

  case err_ptk_ipc_target_not_found:
    return scpy(message, NSTR("起動対象のプログラムが見つかりませんでした。"));
  case err_ptk_ipc_target_access_denied:
    return scpy(message, NSTR("プログラムの実行が拒否されました。"));

  case err_ptk_lua:
    return scpy(message, NSTR("Lua スクリプトの実行中にエラーが発生しました。"));
  }
  return scpy(message, NSTR("未知のエラーコードです。"));
}

error error_ptk_init(void) {
  error err = error_register_message_mapper(err_type_ptk, get_ptk_message);
  if (efailed(err)) {
    return err;
  }
  return eok();
}
