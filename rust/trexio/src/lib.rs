use ::std::os::raw::c_char;

/// C module generated by bindgen
pub mod c;

/// Exit codes
pub mod exit_code;
pub use exit_code::ExitCode;

/// Possible backends
pub mod back_end;
pub use back_end::BackEnd;

/// Bit fields
pub mod bitfield;
pub use bitfield::Bitfield;

pub const PACKAGE_VERSION : &str = unsafe { std::str::from_utf8_unchecked(c::TREXIO_PACKAGE_VERSION) };

fn rc_return<T>(result: T, rc : c::trexio_exit_code) -> Result<T,ExitCode> {
    let rc = ExitCode::from(rc);
    match rc {
        ExitCode::Success => Ok(result),
        x => Err(x)
    }
}

fn string_to_c(s: &str) -> std::ffi::CString {
    std::ffi::CString::new(s).unwrap()
}


pub fn info() -> Result<(),ExitCode> {
    let rc = unsafe { c::trexio_info() };
    rc_return((), rc)
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
        rc_return(File { ptr: result }, rc)
    }

    pub fn close(self) -> Result<(), ExitCode> {
        let rc = unsafe { c::trexio_close(self.ptr) };
        rc_return((), rc)
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

    pub fn get_state(&self) -> Result<usize, ExitCode> {
        let mut num = 0i32;
        let rc = unsafe { c::trexio_get_state(self.ptr, &mut num) };
        let result: usize = num.try_into().expect("try_into failed in get_state");
        rc_return(result, rc)
    }

    pub fn set_state(&self, num: usize) -> Result<(), ExitCode> {
        let num: i32 = num.try_into().expect("try_into failed in set_state");
        let rc = unsafe { c::trexio_set_state(self.ptr, num) };
        rc_return((), rc)
    }

    pub fn set_one_based(&self) -> Result<(), ExitCode> {
        let rc = unsafe { c::trexio_set_one_based(self.ptr) };
        rc_return((), rc)
    }

    pub fn get_int64_num(&self) -> Result<usize, ExitCode> {
        let mut num = 0i32;
        let rc = unsafe {
            c::trexio_get_int64_num(self.ptr, &mut num)
         };
         let num:usize = num.try_into().expect("try_into failed in get_int64_num");
         rc_return(num, rc)
    }

/*
    pub fn read_determinant_list(&self, offset_file: usize, dset: Vec<Bitfield>) -> Result<usize, ExitCode> {
        let rc = unsafe {
            let offset_file: i64 = offset_file;
            let buffer_size: *mut i64 = dset.len().try_into().expect("try_into failed in read_determinant_list");
            let dset: *mut i64 = dset.to_c().as_mut_ptr();
            c::trexio_read_determinant_list(self.ptr, offset_file, buffer_size, dset)
         };
    }
    */

}
include!("generated.rs");


