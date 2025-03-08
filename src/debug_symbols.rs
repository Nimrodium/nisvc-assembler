use crate::{
    data::{AssemblyError, Label, Labels},
    very_verbose_println,
};
pub struct DebugLabelTable {
    table: Vec<(u64, String)>, // memory address, name
}
impl DebugLabelTable {
    // add resolved labels to the table
    pub fn new() -> Self {
        Self { table: vec![] }
    }
    pub fn add_label(&mut self, table: &Label) -> Result<(), AssemblyError> {
        todo!()
    }
    // structure,
    //  address 8 bytes
    //  length of label string 8 bytes?
    //  string ...

    pub fn serialize(self) -> Result<Vec<u8>, AssemblyError> {
        let mut byte_buffer: Vec<u8> = vec![];
        for (address, name) in self.table {
            let address_bytes = &address.to_le_bytes();
            let name_length_bytes = &(name.len() as u64).to_le_bytes();
            let name_bytes = name.as_bytes();
            very_verbose_println!("adding label entry ({address}|{}|{name})", name.len());
            // let serialized_entry = vec![address_bytes, name_length_bytes, name_bytes];
            byte_buffer.extend_from_slice(address_bytes);
            byte_buffer.extend_from_slice(name_length_bytes);
            byte_buffer.extend_from_slice(name_bytes);
        }
        todo!()
    }
}
