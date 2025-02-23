use std::collections::HashMap;

pub type Register = u64;
pub type Labels = HashMap<String, Register>;
pub const REGISTER_WIDTH: usize = size_of::<Register>();

pub const NAME: &str = "nisvc-as";
pub const REGISTER_BYTES: usize = 1;
pub const OPCODE_BYTES: usize = 1;
pub const ADDRESS_BYTES: usize = REGISTER_BYTES;
pub const MMIO_ADDRESS_SPACE: usize = 42;
pub const SIGNATURE: &[u8] = b"NISVC-EF";
pub const DEFAULT_BINARY_NAME: &str = "nisvc.out";
pub const LABEL: char = '!';
pub const ABSOLUTE: char = '$';
pub const RELATIVE: char = '@';
pub const ASSEMBLY_PTR: char = '.';
pub const SEPERATOR: char = ';';
pub const COMMENT: &str = "//";
pub const OPEN_SECTION: char = '{';
pub const CLOSE_SECTION: char = '}';
