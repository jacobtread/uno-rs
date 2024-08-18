fn write_compressed_number(buf: &mut BytesMut, number: u32) {
    if number < 0xFF {
        // Write the number as a single byte
        buf.put_u8(number as u8);
    } else {
        // Write 0xFF followed by the 4-byte representation of the number
        buf.put_u8(0xFF);
        buf.put_u32(number);
    }
}

fn write_byte_sequence(buf: &mut BytesMut, value: &[u8]) {
    let length = value.len();

    // Ensure the max length is not reached
    debug_assert!(length < u32::MAX as usize);

    // Write the length number
    write_compressed_number(buf, length as u32);

    // Copy the bytes onto the buffer
    buf.copy_from_slice(value);
}

fn write_string(buf: &mut BytesMut, value: &str) {
    write_byte_sequence(buf, value.as_bytes())
}
