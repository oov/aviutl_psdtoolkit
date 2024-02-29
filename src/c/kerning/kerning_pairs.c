#include "kerning_pairs.h"

#include <ovarray.h>
#include <stdlib.h>

struct kerning_pairs {
  KERNINGPAIR *pairs;
};

NODISCARD error kerning_pairs_create(struct kerning_pairs **const kpp) {
  if (!kpp || *kpp) {
    return errg(err_invalid_arugment);
  }
  struct kerning_pairs *kp = NULL;
  error err = mem(&kp, 1, sizeof(*kp));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *kp = (struct kerning_pairs){0};
  *kpp = kp;
cleanup:
  if (efailed(err)) {
    kerning_pairs_destroy(&kp);
  }
  return err;
}

void kerning_pairs_destroy(struct kerning_pairs **const kpp) {
  if (!kpp || !*kpp) {
    return;
  }
  struct kerning_pairs *const kp = *kpp;
  if (kp->pairs) {
    OV_ARRAY_DESTROY(&kp->pairs);
  }
  ereport(mem_free(kpp));
}

static int kerning_pair_compare(void const *const a, void const *const b) {
  KERNINGPAIR const *const pa = a;
  KERNINGPAIR const *const pb = b;
  if (pa->wFirst < pb->wFirst) {
    return -1;
  }
  if (pa->wFirst > pb->wFirst) {
    return 1;
  }
  if (pa->wSecond < pb->wSecond) {
    return -1;
  }
  if (pa->wSecond > pb->wSecond) {
    return 1;
  }
  return 0;
}

NODISCARD error kerning_pairs_update_database(struct kerning_pairs *const kp, HDC const hdc) {
  if (!kp || !hdc) {
    return errg(err_invalid_arugment);
  }
  error err = eok();
  DWORD n = GetKerningPairsW(hdc, 0, NULL);
  if (!n) {
    OV_ARRAY_SET_LENGTH(kp->pairs, 0);
    goto cleanup;
  }
  err = OV_ARRAY_GROW(&kp->pairs, n);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  n = GetKerningPairsW(hdc, n, kp->pairs);
  if (!n) {
    err = errg(err_fail);
    goto cleanup;
  }
  OV_ARRAY_SET_LENGTH(kp->pairs, n);
  qsort(kp->pairs, n, sizeof(*kp->pairs), kerning_pair_compare);
cleanup:
  return err;
}

size_t kerning_pairs_get_num_pairs(struct kerning_pairs const *const kp) {
  if (!kp) {
    return 0;
  }
  return OV_ARRAY_LENGTH(kp->pairs);
}

int kerning_pairs_get_kerning(struct kerning_pairs const *const kp, wchar_t const first, wchar_t const second) {
  if (!kp) {
    return 0;
  }
  KERNINGPAIR const *const r = bsearch(&(KERNINGPAIR){.wFirst = first, .wSecond = second},
                                       kp->pairs,
                                       OV_ARRAY_LENGTH(kp->pairs),
                                       sizeof(*kp->pairs),
                                       kerning_pair_compare);
  if (!r) {
    return 0;
  }
  return r->iKernAmount;
}
