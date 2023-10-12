use ::std::os::raw::c_char;

/// C module generated by bindgen
pub mod c;

/// Exit codes
pub mod exit_code;
pub use exit_code::ExitCode;

/// Possible backends
pub mod back_end;
pub use back_end::BackEnd;

pub const PACKAGE_VERSION : &str = unsafe { std::str::from_utf8_unchecked(c::TREXIO_PACKAGE_VERSION) };

fn rc_return<T>(rc : c::trexio_exit_code, result: T) -> Result<T,ExitCode> {
    let rc = ExitCode::from(rc);
    match rc {
        ExitCode::Success => Ok(result),
        x => Err(x)
    }
}

fn string_to_c(s: &str) -> std::ffi::CString {
    std::ffi::CString::new(s).unwrap()
}



/// Type for a TREXIO file
pub struct File {
    ptr: *mut c::trexio_t,
}


impl File {

    pub fn open(file_name: &str, mode: char, back_end: BackEnd) -> Result<File, ExitCode> {
        let file_name_c = string_to_c(file_name);
        let file_name_c = file_name_c.as_ptr() as *const c_char;
        let mode = mode as c_char;
        let back_end = back_end.to_c();
        let rc: *mut c::trexio_exit_code = &mut c::TREXIO_SUCCESS.clone();
        let result = unsafe { c::trexio_open(file_name_c, mode, back_end, rc) };
        let rc = unsafe { *rc };
        rc_return(rc, File { ptr: result })
    }

    pub fn close(self) -> Result<(), ExitCode> {
        let rc = unsafe { c::trexio_close(self.ptr) };
        rc_return(rc, ())
    }

    pub fn inquire(file_name: &str) -> Result<bool, ExitCode> {
        let file_name_c = string_to_c(file_name);
        let file_name_c = file_name_c.as_ptr() as *const c_char;
        let rc = unsafe { c::trexio_inquire(file_name_c) };
        match ExitCode::from(rc) {
            ExitCode::Failure => Ok(false),
            ExitCode::Success => Ok(true),
            x       => Err(x),
        }
    }

}
include!("generated.rs");


#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;
    use c::*;

    #[test]
    fn read_trexio_file() {
        println!("============================================");
        println!("         TREXIO MAJOR VERSION  : {}", TREXIO_VERSION_MAJOR);
        println!("============================================");

    }
}
