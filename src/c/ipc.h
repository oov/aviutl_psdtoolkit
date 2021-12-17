#pragma once

#include "3rd/base.c/include/base.h"

struct ipc;
struct ipc_update_editing_image_state_params {
  struct str file_path_utf8;
  struct str state_utf8;
};
struct ipc_export_faview_slider_params {
  struct str file_path_utf8;
  struct str slider_name_utf8;
  struct str names_utf8;
  struct str values_utf8;
  int32_t selected_index;
};
struct ipc_export_layer_names_params {
  struct str file_path_utf8;
  struct str names_utf8;
  struct str values_utf8;
  int32_t selected_index;
};
struct ipc_options {
  struct wstr cmdline;
  void *userdata;
  void (*on_update_editing_image_state)(void *const userdata,
                                        struct ipc_update_editing_image_state_params *const params);
  void (*on_export_faview_slider)(void *const userdata, struct ipc_export_faview_slider_params *const params);
  void (*on_export_layer_names)(void *const userdata, struct ipc_export_layer_names_params *const params);
};

NODISCARD error ipc_init(struct ipc **const ipc, struct ipc_options const *const opt);
void ipc_exit(struct ipc **const ipc);
NODISCARD error ipc_add_file(struct ipc *const ipc, struct str const *const path_utf8, uint32_t const tag);
NODISCARD error ipc_clear_files(struct ipc *const ipc);
NODISCARD error ipc_deserialize(struct ipc *const ipc, struct str const *const src_utf8);
NODISCARD error ipc_draw(struct ipc *const ipc,
                         int32_t const id,
                         struct str const *const path_utf8,
                         void *const p,
                         int32_t const width,
                         int32_t const height);
NODISCARD error ipc_get_layer_names(struct ipc *const ipc,
                                    int32_t const id,
                                    struct str const *const path_utf8,
                                    struct str *const dest_utf8);
NODISCARD error ipc_serialize(struct ipc *const ipc, struct str *const dest_utf8);
struct ipc_set_properties_options {
  uint32_t const *tag;
  struct str const *layer_utf8;
  float const *scale;
  int32_t const *offset_x;
  int32_t const *offset_y;
};
struct ipc_set_properties_results {
  uint32_t cache_key;
  int32_t width;
  int32_t height;
  bool modifiled;
  char reserved[3];
};
NODISCARD error ipc_set_properties(struct ipc *const ipc,
                                   int32_t const id,
                                   struct str const *const path_utf8,
                                   struct ipc_set_properties_options *options,
                                   struct ipc_set_properties_results *rets);
NODISCARD error ipc_show_gui(struct ipc *const ipc, uint64_t *const window_handle);
NODISCARD error ipc_update_current_project_path(struct ipc *const ipc, struct str const *const path_utf8);
