use std::io::Error;
use std::process::Child;
use winapi::um::consoleapi::AllocConsole;
use winapi::um::wincon::{GenerateConsoleCtrlEvent, GetConsoleWindow, CTRL_BREAK_EVENT};

/// Send a CTRL+BREAK signal to a child process's process group.
/// This requires that the process should be created with CREATE_NEW_PROCESS_GROUP.
pub fn send_child_ctrl_break(child: &Child) -> Result<(), Error> {
    if 0 != unsafe { GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, child.id()) } {
        Ok(())
    } else {
        Err(Error::last_os_error())
    }
}

/// Create a console for the process if it doesn't already have one.
pub fn create_console() -> Result<(), Error> {
    if unsafe { GetConsoleWindow() }.is_null() {
        if 0 != unsafe { AllocConsole() } {
            Ok(())
        } else {
            Err(Error::last_os_error())
        }
    } else {
        Ok(())
    }
}
