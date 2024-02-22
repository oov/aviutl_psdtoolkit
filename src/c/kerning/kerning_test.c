#include <ovtest.h>

#include "kerning.h"

static void test_kerning_context_create(void) {
  struct kerning_context *ctx = NULL;
  TEST_SUCCEEDED_F(kerning_context_create(&ctx));
  TEST_CHECK(ctx != NULL);
  kerning_context_destroy(&ctx);
}

TEST_LIST = {
    {"test_kerning_context_create", test_kerning_context_create},
    {0},
};
