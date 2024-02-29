#pragma once

#include <ovbase.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

struct kerning_pairs;

NODISCARD error kerning_pairs_create(struct kerning_pairs **const kpp);
void kerning_pairs_destroy(struct kerning_pairs **const kpp);
NODISCARD error kerning_pairs_update_database(struct kerning_pairs *const kp, HDC const hdc);
size_t kerning_pairs_get_num_pairs(struct kerning_pairs const *const kp);
int kerning_pairs_get_kerning(struct kerning_pairs const *const kp, wchar_t const first, wchar_t const second);
