#include "error_ptk.h"

#include "ovutil/win32.h"

#include "i18n.h"

static NODISCARD error get_generic_message(int const type, int const code, struct NATIVE_STR *const dest) {
  if (!dest) {
    return errg(err_invalid_arugment);
  }
  if (type != err_type_generic) {
    dest->len = 0;
    return eok();
  }
  switch (code) {
  case err_fail:
    return to_wstr(&str_unmanaged_const(gettext("Failed.")), dest);
  case err_unexpected:
    return to_wstr(&str_unmanaged_const(gettext("Unexpected.")), dest);
  case err_invalid_arugment:
    return to_wstr(&str_unmanaged_const(gettext("Invalid argument.")), dest);
  case err_null_pointer:
    return to_wstr(&str_unmanaged_const(gettext("NULL pointer.")), dest);
  case err_out_of_memory:
    return to_wstr(&str_unmanaged_const(gettext("Out of memory.")), dest);
  case err_not_sufficient_buffer:
    return to_wstr(&str_unmanaged_const(gettext("Not sufficient buffer.")), dest);
  case err_not_found:
    return to_wstr(&str_unmanaged_const(gettext("Not found.")), dest);
  case err_abort:
    return to_wstr(&str_unmanaged_const(gettext("Aborted.")), dest);
  case err_not_implemented_yet:
    return to_wstr(&str_unmanaged_const(gettext("Not implemented yet.")), dest);
  }
  return to_wstr(&str_unmanaged_const(gettext("Unknown error code.")), dest);
}

static NODISCARD error get_ptk_message(int const type, int const code, struct NATIVE_STR *const dest) {
  if (!dest) {
    return errg(err_invalid_arugment);
  }
  if (type != err_type_ptk) {
    dest->len = 0;
    return eok();
  }
  switch (code) {
  case err_ptk_arch_is_not_64bit:
    return to_wstr(&str_unmanaged_const(gettext("A 64Bit version of Windows is required.")), dest);
  case err_ptk_unsupported_aviutl_version:
    return to_wstr(&str_unmanaged_const(gettext("The currently running version of AviUtl is not supported.")), dest);
  case err_ptk_exedit_not_found:
    return to_wstr(&str_unmanaged_const(gettext("Advanced Editing plug-in exedit.auf cannot be found.")), dest);
  case err_ptk_exedit_not_found_in_same_dir:
    return to_wstr(&str_unmanaged_const(gettext(
                       "Advanced Editing plug-in exedit.auf cannot be found in the same folder as GCMZDrops.auf.")),
                   dest);
  case err_ptk_lua51_cannot_load:
    return to_wstr(&str_unmanaged_const(gettext("Failed to load lua51.dll.")), dest);
  case err_ptk_bridge_cannot_load:
    return to_wstr(&str_unmanaged_const(gettext("Failed to load PSDToolKitBridge.dll.")), dest);
  case err_ptk_unsupported_exedit_version:
    return to_wstr(&str_unmanaged_const(gettext("The currently using version of exedit.auf is not supported.")), dest);
  case err_ptk_project_is_not_open:
    return to_wstr(&str_unmanaged_const(gettext("AviUtl project file (*.aup) has not yet been opened.")), dest);
  case err_ptk_project_has_not_yet_been_saved:
    return to_wstr(&str_unmanaged_const(gettext("AviUtl project file (*.aup) has not yet been saved.")), dest);

  case err_ptk_target_psd_file_object_not_found:
    return to_wstr(
        &str_unmanaged_const(gettext("Could not find the edit control to which the settings should be sent.")), dest);
  case err_ptk_target_variable_not_found:
    return to_wstr(&str_unmanaged_const(gettext("The variable to be rewritten could not be found.")), dest);

  case err_ptk_ipc_target_not_found:
    return to_wstr(&str_unmanaged_const(gettext("The program to be started could not be found.")), dest);
  case err_ptk_ipc_target_access_denied:
    return to_wstr(&str_unmanaged_const(gettext("Program execution refused.")), dest);

  case err_ptk_lua:
    return to_wstr(&str_unmanaged_const(gettext("An error occurred while executing a Lua script.")), dest);
  }
  return to_wstr(&str_unmanaged_const(gettext("Unknown error code.")), dest);
}

NODISCARD error ptk_error_message(int const type, int const code, struct NATIVE_STR *const dest) {
  if (type == err_type_generic) {
    return get_generic_message(type, code, dest);
  }
  if (type == err_type_hresult) {
    return error_win32_message_mapper(type, code, MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), dest);
  }
  if (type == err_type_ptk) {
    return get_ptk_message(type, code, dest);
  }
  if (type == err_type_errno) {
    return error_errno_message_mapper(type, code, dest);
  }
  return scpy(dest, NSTR("Unknown error code."));
}
