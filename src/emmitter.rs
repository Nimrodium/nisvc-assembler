const SIGNATURE: &[u8] = b"NISVC-EF";
pub fn package(
    entry_point: u64,
    data: Vec<u8>,
    mut program: Vec<u8>,
    debug_symbols: Vec<u8>,
) -> Vec<u8> {
    let mut image = Vec::<u8>::with_capacity(
        program.len() + data.len() + debug_symbols.len() + SIGNATURE.len() + size_of::<u64>() * 3,
    );

    program.extend(data);

    image.extend_from_slice(SIGNATURE);
    image.extend(entry_point.to_le_bytes());
    image.extend((program.len() as u64).to_le_bytes());
    image.extend(program);
    image.extend((debug_symbols.len() as u64).to_le_bytes());
    image.extend(debug_symbols);
    image
}
