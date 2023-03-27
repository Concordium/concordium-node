#include <os/log.h>

// Wrap os_log_with_type as it is a macro which cannot be used directly through FFI.
void wrapped_os_log_with_type(os_log_t log, os_log_type_t type, const char* message) {
    os_log_with_type(log, type, "%{public}s", message);
}
