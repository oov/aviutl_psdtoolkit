#include <ovtest.h>

#include <ovthreads.h>
#include <ovutil/win32.h>

// Only intercept ipc.c's explicit unlocks, not the condition variables' unlocks.
// The command mutex belongs to the test thread throughout this one command.
static mtx_t *command_mutex;
static DWORD command_thread;
static LONG invalid_unlocks;

static int checked_mtx_unlock(mtx_t *const mtx) {
  if (mtx == command_mutex && GetCurrentThreadId() != command_thread) {
    InterlockedIncrement(&invalid_unlocks);
    // Do not corrupt the native critical section: report a test failure instead
    // of relying on Windows to eventually raise STATUS_RESOURCE_NOT_OWNED.
    return thrd_error;
  }
  return mtx_unlock(mtx);
}

static struct cndvar *reply_condition;
static HANDLE reply_wait_entered;

static int checked_cndvar_wait_while(struct cndvar *const cv, int const var) {
  if (cv == reply_condition && reply_wait_entered) {
    // The caller still owns cv->mtx here. The reader cannot publish its
    // stopped state until the waiter releases that mutex in cnd_wait.
    SetEvent(reply_wait_entered);
  }
  return cndvar_wait_while(cv, var);
}

#define mtx_unlock checked_mtx_unlock
#define cndvar_wait_while checked_cndvar_wait_while
#include "ipc.c"
#undef cndvar_wait_while
#undef mtx_unlock

static int reply_worker(void *const userdata) {
  // A successful reply has a zero-length error message. No GUI/PSD is needed.
  error err = read_reply((struct ipc *)userdata, 0);
  int const result = efailed(err) ? thrd_error : thrd_success;
  ereport(err);
  return result;
}

static void test_reply_mutex_owner(void) {
  struct ipc ipc = {
      .process = INVALID_HANDLE_VALUE,
      .pipe_in_w = INVALID_HANDLE_VALUE,
      .pipe_out_r = INVALID_HANDLE_VALUE,
  };
  HANDLE pipe_read = INVALID_HANDLE_VALUE;
  thrd_t worker = {0};
  bool worker_started = false;
  bool command_started = false;
  int worker_result = thrd_error;
  error err = eok();

  if (!TEST_CHECK(mtx_init(&ipc.mtx, mtx_plain | mtx_recursive) == thrd_success)) {
    return;
  }
  cndvar_init(&ipc.s2c);
  cndvar_init(&ipc.c2s);
  command_mutex = &ipc.mtx;
  command_thread = GetCurrentThreadId();
  invalid_unlocks = 0;

  // GetCurrentProcess() is a pseudo-handle equal to INVALID_HANDLE_VALUE;
  // ipc.c needs a real handle for its running-process check.
  if (!TEST_CHECK(DuplicateHandle(GetCurrentProcess(),
                                  GetCurrentProcess(),
                                  GetCurrentProcess(),
                                  &ipc.process,
                                  0,
                                  FALSE,
                                  DUPLICATE_SAME_ACCESS))) {
    goto cleanup;
  }
  if (!TEST_CHECK(CreatePipe(&pipe_read, &ipc.pipe_in_w, NULL, 0))) {
    goto cleanup;
  }
  // Use the real command entry point, but an already-running process and a
  // local pipe keep this test independent of PSDToolKit.exe and AviUtl.
  err = start_command_process(&ipc, FOURCC('C', 'L', 'R', 'F'));
  command_started = true;
  if (!TEST_CHECK(!efailed(err))) {
    goto cleanup;
  }
  if (!TEST_CHECK(thrd_create(&worker, reply_worker, &ipc) == thrd_success)) {
    goto cleanup;
  }
  worker_started = true;
  err = get_reply(&ipc);
  TEST_CHECK(!efailed(err));

cleanup:
  if (command_started) {
    err = finish_command_process(&ipc, err);
  }
  if (worker_started) {
    TEST_CHECK(thrd_join(worker, &worker_result) == thrd_success);
    TEST_CHECK(worker_result == thrd_success);
    TEST_CHECK(invalid_unlocks == 0);
    TEST_MSG("read_reply unlocked the command mutex from a non-owner thread %ld time(s)", invalid_unlocks);
  }
  ereport(err);
  command_mutex = NULL;
  command_thread = 0;
  if (pipe_read != INVALID_HANDLE_VALUE) {
    CloseHandle(pipe_read);
  }
  if (ipc.pipe_in_w != INVALID_HANDLE_VALUE) {
    CloseHandle(ipc.pipe_in_w);
  }
  if (ipc.process != INVALID_HANDLE_VALUE) {
    CloseHandle(ipc.process);
  }
  cndvar_exit(&ipc.c2s);
  cndvar_exit(&ipc.s2c);
  mtx_destroy(&ipc.mtx);
}

static int disconnected_reply_waiter(void *const userdata) {
  struct ipc *const ipc = (struct ipc *)userdata;
  error err = get_reply(ipc);
  bool const disconnected = eis_hr(err, HRESULT_FROM_WIN32(ERROR_BROKEN_PIPE));
  efree(&err);
  return disconnected ? thrd_success : thrd_error;
}

static void test_disconnect(bool const wait_first, bool const partial_reply) {
  struct ipc ipc = {0};
  HANDLE pipe_write = INVALID_HANDLE_VALUE;
  thrd_t reader = {0}, waiter = {0};
  bool reader_started = false, waiter_started = false;
  int result = thrd_error;
  cndvar_init(&ipc.s2c);
  cndvar_init(&ipc.c2s);
  reply_condition = &ipc.s2c;
  reply_wait_entered = CreateEventW(NULL, TRUE, FALSE, NULL);
  if (!TEST_CHECK(reply_wait_entered != NULL)) {
    goto cleanup;
  }
  if (!TEST_CHECK(CreatePipe(&ipc.pipe_out_r, &pipe_write, NULL, 0))) {
    goto cleanup;
  }
  if (!TEST_CHECK(thrd_create(&reader, read_worker, &ipc) == thrd_success)) {
    goto cleanup;
  }
  reader_started = true;
  if (wait_first) {
    if (!TEST_CHECK(thrd_create(&waiter, disconnected_reply_waiter, &ipc) == thrd_success)) {
      goto cleanup;
    }
    waiter_started = true;
    if (!TEST_CHECK(WaitForSingleObject(reply_wait_entered, 2000) == WAIT_OBJECT_0)) {
      goto cleanup;
    }
  }
  if (partial_reply) {
    // Announce four error-message bytes, but disconnect after only two.
    uint32_t const header = 0x80000004;
    DWORD written = 0;
    TEST_CHECK(WriteFile(pipe_write, &header, sizeof(header), &written, NULL));
    TEST_CHECK(written == sizeof(header));
    TEST_CHECK(WriteFile(pipe_write, "ab", 2, &written, NULL));
    TEST_CHECK(written == 2);
  }
cleanup:
  if (pipe_write != INVALID_HANDLE_VALUE) {
    CloseHandle(pipe_write);
  }
  if (reader_started) {
    TEST_CHECK(thrd_join(reader, &result) == thrd_success);
    TEST_CHECK(result == 1);
  }
  if (waiter_started) {
    TEST_CHECK(thrd_join(waiter, &result) == thrd_success);
    TEST_CHECK(result == thrd_success);
  }
  if (reader_started) {
    // A disconnect preceding get_reply must not be lost. Nor may consuming
    // the first error make a subsequent command wait forever.
    TEST_CHECK(disconnected_reply_waiter(&ipc) == thrd_success);
    TEST_CHECK(disconnected_reply_waiter(&ipc) == thrd_success);
  }
  if (ipc.pipe_out_r) {
    CloseHandle(ipc.pipe_out_r);
  }
  if (reply_wait_entered) {
    CloseHandle(reply_wait_entered);
  }
  reply_wait_entered = NULL;
  reply_condition = NULL;
  cndvar_exit(&ipc.c2s);
  cndvar_exit(&ipc.s2c);
}

static void test_disconnect_before_reply_wait(void) { test_disconnect(false, false); }
static void test_disconnect_during_reply_wait(void) { test_disconnect(true, false); }
static void test_disconnect_during_reply_body(void) { test_disconnect(true, true); }

TEST_LIST = {
    {"disconnect_before_reply_wait", test_disconnect_before_reply_wait},
    {"disconnect_during_reply_wait", test_disconnect_during_reply_wait},
    {"disconnect_during_reply_body", test_disconnect_during_reply_body},
    {"reply_mutex_owner", test_reply_mutex_owner},
    {NULL, NULL},
};
