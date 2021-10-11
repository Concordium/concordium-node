use std::{
    ffi::{OsStr, OsString},
    fs::remove_dir_all,
    os::windows::ffi::{OsStrExt, OsStringExt},
    path::Path,
    ptr,
};
use winapi::{
    shared::{
        minwindef::{DWORD, LPDWORD, UINT},
        winerror::{ERROR_FUNCTION_NOT_CALLED, ERROR_MORE_DATA, ERROR_SUCCESS},
    },
    um::{
        winnt::{LPCWSTR, LPWSTR},
        winsvc::*,
    },
};

type MsiHandle = u32;

extern "system" {
    fn MsiGetPropertyW(
        hInstall: MsiHandle,
        szName: LPCWSTR,
        szValueBuf: LPWSTR,
        pcchValueBuf: LPDWORD,
    ) -> UINT;
}

/// A custom action to delete a directory and all of its contents.
#[no_mangle]
pub extern "system" fn DeleteDataFolder(install_handle: MsiHandle) -> UINT {
    match get_data(install_handle) {
        Err(_) => {
            return ERROR_FUNCTION_NOT_CALLED;
        }
        Ok(data_dir_wide) => {
            let data_dir = OsString::from_wide(&data_dir_wide[0..data_dir_wide.len() - 1]);
            let p = Path::new(&data_dir);
            if remove_dir_all(p).is_err() {
                return ERROR_FUNCTION_NOT_CALLED;
            }
        }
    }
    ERROR_SUCCESS
}

/// Service should start automatically at start-up
const SERVICE_AUTO_START: DWORD = 0x00000002;
/// Service should start on-demand
const SERVICE_DEMAND_START: DWORD = 0x00000003;

/// Custom action to configure a service as auto starting.
#[no_mangle]
pub extern "system" fn SetServiceStartAuto(install_handle: MsiHandle) -> UINT {
    match set_service_start(install_handle, SERVICE_AUTO_START) {
        Err(_) => ERROR_FUNCTION_NOT_CALLED,
        Ok(_) => ERROR_SUCCESS,
    }
}

/// Custom action to configure a service as on-demand starting.
#[no_mangle]
pub extern "system" fn SetServiceStartDemand(install_handle: MsiHandle) -> UINT {
    match set_service_start(install_handle, SERVICE_DEMAND_START) {
        Err(_) => ERROR_FUNCTION_NOT_CALLED,
        Ok(_) => ERROR_SUCCESS,
    }
}

/// Custom action to start a service.
#[no_mangle]
pub extern "system" fn StartService(install_handle: MsiHandle) -> UINT {
    match start_service(install_handle) {
        Err(_) => ERROR_FUNCTION_NOT_CALLED,
        Ok(_) => ERROR_SUCCESS,
    }
}

/// Configure a service's start type.
fn set_service_start(install_handle: MsiHandle, start_type: DWORD) -> Result<(), UINT> {
    let service_name = get_data(install_handle)?;
    unsafe {
        let sc_manager = OpenSCManagerW(ptr::null(), ptr::null(), SC_MANAGER_CONNECT);

        if sc_manager.is_null() {
            return Err(1);
        }

        let service_handle = OpenServiceW(sc_manager, service_name.as_ptr(), SERVICE_CHANGE_CONFIG);
        if service_handle.is_null() {
            CloseServiceHandle(sc_manager);
            return Err(1);
        }

        let res = ChangeServiceConfigW(
            service_handle,
            SERVICE_NO_CHANGE,
            start_type,
            SERVICE_NO_CHANGE,
            ptr::null(),
            ptr::null(),
            ptr::null_mut(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        );

        CloseServiceHandle(service_handle);
        CloseServiceHandle(sc_manager);
        if res != 0 {
            Ok(())
        } else {
            Err(1)
        }
    }
}

/// Start a service.
fn start_service(install_handle: MsiHandle) -> Result<(), UINT> {
    let service_name = get_data(install_handle)?;
    unsafe {
        let sc_manager = OpenSCManagerW(ptr::null(), ptr::null(), SC_MANAGER_CONNECT);

        if sc_manager.is_null() {
            return Err(1);
        }

        let service_handle = OpenServiceW(sc_manager, service_name.as_ptr(), SERVICE_START);
        if service_handle.is_null() {
            CloseServiceHandle(sc_manager);
            return Err(1);
        }

        let res = StartServiceW(service_handle, 0, ptr::null_mut());

        CloseServiceHandle(service_handle);
        CloseServiceHandle(sc_manager);
        if res != 0 {
            Ok(())
        } else {
            Err(1)
        }
    }
}

/// Get the CustomActionData property that is used to pass data to a deferred
/// custom action.
fn get_data(install_handle: MsiHandle) -> Result<Vec<u16>, UINT> {
    let mut len = 0;
    let mut value_vec: Vec<u16> = vec![0];
    let property: &OsStr = "CustomActionData\0".as_ref();
    let property_vec: Vec<u16> = property.encode_wide().collect();
    let res = unsafe {
        MsiGetPropertyW(install_handle, property_vec.as_ptr(), value_vec.as_mut_ptr(), &mut len)
    };
    if res == ERROR_MORE_DATA {
        len += 1;
        value_vec = vec![0; len as usize];
        let res = unsafe {
            MsiGetPropertyW(install_handle, property_vec.as_ptr(), value_vec.as_mut_ptr(), &mut len)
        };
        if res == ERROR_SUCCESS {
            Ok(value_vec)
        } else {
            Err(res)
        }
    } else if res == ERROR_SUCCESS {
        Ok(value_vec)
    } else {
        Err(res)
    }
}
