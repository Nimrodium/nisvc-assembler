pub type Register = u64;
pub const REGISTER_WIDTH: usize = size_of::<Register>();

pub const NAME: &str = "nisvc-as";
pub const REGISTER_BYTES: usize = 1;
pub const OPCODE_BYTES: usize = 1;
pub const ADDRESS_BYTES: usize = 8;
pub const HEADER_LENGTH_ENTRIES_BYTES: usize = 8;
pub const MMIO_ADDRESS_SPACE: usize = 42;
pub const SIGNATURE: &[u8] = b"NISVC-EF";
pub const DEFAULT_BINARY_NAME: &str = "nisvc.out";
pub const LABEL: char = '!';
pub const ABSOLUTE: char = '$';
pub const RELATIVE: char = '@';
pub const ASSEMBLY_PTR: char = '.';
pub const SEPERATOR: char = ';';
pub const DUP_SEPERATOR: char = ':';
pub const COMMENT: &str = "//";

pub const DATA_MARKER: &str = ".data";
pub const PROGRAM_MARKER: &str = ".program";
pub const ENTRY_MARKER: &str = ".entry";

pub const STR: char = '"';
pub const ESCAPE: char = '\\';
pub const SPACE: char = ' ';
pub const COMMA: char = ',';

pub const HEX: char = 'x';
pub const BINARY: char = 'b';
pub const DEC: char = 'd';

pub const MAGIC_RESERVED_MEM: u8 = 0xea;
