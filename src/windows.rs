/// Enables ANSI code support on Windows 10.
///
/// This uses Windows API calls to alter the properties of the console that
/// the program is running in.
///
/// https://msdn.microsoft.com/en-us/library/windows/desktop/mt638032(v=vs.85).aspx
///
/// Returns a `Result` with the Windows error code if unsuccessful.
#[cfg(windows)]
pub fn enable_ansi_support() -> Result<(), u32> {
    // ref: https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences#EXAMPLE_OF_ENABLING_VIRTUAL_TERMINAL_PROCESSING @@ https://archive.is/L7wRJ#76%
    use windows::w;
    use windows::Win32::Foundation::GetLastError;
    use windows::Win32::Foundation::BOOL;
    use windows::Win32::Storage::FileSystem::FILE_FLAGS_AND_ATTRIBUTES;
    use windows::Win32::Storage::FileSystem::{CreateFileW, OPEN_EXISTING};
    use windows::Win32::Storage::FileSystem::{
        FILE_GENERIC_READ, FILE_GENERIC_WRITE, FILE_SHARE_WRITE,
    };
    use windows::Win32::System::Console::{GetConsoleMode, SetConsoleMode};
    use windows::Win32::System::Console::{CONSOLE_MODE, ENABLE_VIRTUAL_TERMINAL_PROCESSING};

    unsafe {
        // ref: https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew
        // Using `CreateFileW("CONOUT$", ...)` to retrieve the console handle works correctly even if STDOUT and/or STDERR are redirected
        if let Ok(console_handle) = CreateFileW(
            w!("CONOUT$"),
            FILE_GENERIC_READ | FILE_GENERIC_WRITE,
            FILE_SHARE_WRITE,
            None,
            OPEN_EXISTING,
            FILE_FLAGS_AND_ATTRIBUTES(0),
            None,
        ) {
            // ref: https://docs.microsoft.com/en-us/windows/console/getconsolemode
            let mut console_mode = CONSOLE_MODE(0);
            if BOOL(0) == GetConsoleMode(console_handle, &mut console_mode) {
                return Err(GetLastError().0);
            }

            // VT processing not already enabled?
            if console_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING == CONSOLE_MODE(0) {
                // https://docs.microsoft.com/en-us/windows/console/setconsolemode
                if BOOL(0)
                    == SetConsoleMode(
                        console_handle,
                        console_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING,
                    )
                {
                    return Err(GetLastError().0);
                }
            }
            Ok(())
        } else {
            Err(GetLastError().0)
        }
    }
}
