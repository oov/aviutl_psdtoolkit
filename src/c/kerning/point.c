#include "point.h"

void point_sort(struct point *const arr, size_t const n, point_compare_fn const compare, void *const userdata) {
  // this code is based on public-domain C implementation by Darel Rex Finley.
  // https://alienryderflex.com/quicksort/
  enum {
    max_levels = 64,
  };
  int const elements = (int)n;
  int beg[max_levels];
  int end[max_levels];

  int i = 0, l, r;
  beg[0] = 0;
  end[0] = elements;
  while (i >= 0) {
    l = beg[i];
    r = end[i] - 1;
    if (l >= r) {
      --i;
      continue;
    }
    struct point piv = arr[l];
    while (l < r) {
      while (compare(&arr[r], &piv, userdata) >= 0 && l < r) {
        --r;
      }
      if (l < r) {
        arr[l++] = arr[r];
      }
      while (compare(&arr[l], &piv, userdata) <= 0 && l < r) {
        ++l;
      }
      if (l < r) {
        arr[r--] = arr[l];
      }
    }
    arr[l] = piv;
    beg[i + 1] = l + 1;
    end[i + 1] = end[i];
    end[i++] = l;

    if (end[i] - beg[i] > end[i - 1] - beg[i - 1]) {
      int swap = beg[i];
      beg[i] = beg[i - 1];
      beg[i - 1] = swap;
      swap = end[i];
      end[i] = end[i - 1];
      end[i - 1] = swap;
    }
  }
}
