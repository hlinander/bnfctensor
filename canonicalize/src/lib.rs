#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

#[no_mangle]
pub extern "C" fn canonicalize(perm: *const perm_t, out: *mut u8) -> () {
    unsafe {
        println!("HELLO RUST");
        // *out = 0x10;
    }
}



#[repr("C")]
pub struct perm_t {
    length: u64,
    dummies_length: u64,
    frees_length: u64,
    window_size: u64,
    perm: *const u64,
    generating_set: *const u64,
    frees: *const u64,
    dummies: *const u64,
}