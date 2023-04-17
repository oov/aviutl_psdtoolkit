#pragma once

#include "ovbase.h"

enum {
  err_type_ptk = 100,
};

enum err_ptk {
  err_ptk_arch_is_not_64bit = 100,
  err_ptk_unsupported_aviutl_version = 101,
  err_ptk_exedit_not_found = 102,
  err_ptk_exedit_not_found_in_same_dir = 103,
  err_ptk_lua51_cannot_load = 104,
  err_ptk_bridge_cannot_load = 105,
  err_ptk_unsupported_exedit_version = 106,
  err_ptk_project_is_not_open = 107,
  err_ptk_project_has_not_yet_been_saved = 108,
  err_ptk_target_psd_file_object_not_found = 109,
  err_ptk_target_variable_not_found = 110,

  err_ptk_ipc_target_not_found = 200,
  err_ptk_ipc_target_access_denied = 201,

  err_ptk_lua = 300,
};

NODISCARD error ptk_error_message(int const type, int const code, struct NATIVE_STR *const dest);
