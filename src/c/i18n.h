#pragma once

#include "ovmo.h"

#define gettext(id) mo_gettext(mo_get_default(), (id))
#define gettext_noop(id) (id)
