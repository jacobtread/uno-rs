use anyhow::{anyhow, Context};
use bytes::{Buf, BytesMut};
use num_enum::TryFromPrimitive;

use crate::ty::{UnoComplexType, UnoSimpleTypeClass, UnoType, UnoTypeClass};

use super::types::{
    BlockHeader, CacheIndex, HeaderExtraFlags, HeaderFlags, Message, RawBlock, ReplyMessage,
    RequestMessage, UnoAny, UnoSequence, UnoValue, CACHE_SIZE, OID, TID,
};

/// State for the reader
pub struct ReaderState {
    // Caches for last values
    last_oid: Option<OID>,
    last_tid: Option<TID>,
    last_type: Option<UnoType>,

    // Caches for lookups
    oid_cache: [Option<OID>; CACHE_SIZE],
    tid_cache: [Option<TID>; CACHE_SIZE],
    type_cache: [Option<UnoType>; CACHE_SIZE],
}

impl Default for ReaderState {
    fn default() -> Self {
        const EMPTY_OID_VALUE: Option<OID> = None;
        const EMPTY_TID_VALUE: Option<TID> = None;
        const EMPTY_TYPE_VALUE: Option<UnoType> = None;

        Self {
            last_oid: None,
            last_tid: None,
            last_type: None,
            oid_cache: [EMPTY_OID_VALUE; CACHE_SIZE],
            tid_cache: [EMPTY_TID_VALUE; CACHE_SIZE],
            type_cache: [EMPTY_TYPE_VALUE; CACHE_SIZE],
        }
    }
}

impl ReaderState {
    /// Gets a [OID] from the inbound cache at the provided index
    pub fn get_cached_oid(&self, index: usize) -> Option<OID> {
        let value = &self.oid_cache[index];

        value.clone()
    }

    /// Sets the [OID] at the provided cache index for the inbound cache
    pub fn set_cached_oid(&mut self, index: usize, value: OID) {
        self.oid_cache[index] = Some(value)
    }

    /// Gets a [TID] from the inbound cache at the provided index
    pub fn get_cached_tid(&self, index: usize) -> Option<TID> {
        let value = &self.tid_cache[index];

        value.clone()
    }

    /// Sets the [TID] at the provided cache index for the inbound cache
    pub fn set_cached_tid(&mut self, index: usize, value: TID) {
        self.tid_cache[index] = Some(value)
    }

    /// Gets a [UnoType] from the inbound cache at the provided index
    pub fn get_cached_type(&self, index: usize) -> Option<UnoType> {
        let value = &self.type_cache[index];

        value.clone()
    }

    /// Sets the [UnoType] at the provided cache index for the inbound cache
    pub fn set_cached_type(&mut self, index: usize, value: UnoType) {
        self.type_cache[index] = Some(value)
    }
}

/// Reads a block header from the provided bytes
pub fn read_block_header(input: &mut BytesMut) -> Option<BlockHeader> {
    // Not enough bytes to read header
    if input.len() < 8 {
        return None;
    }

    let size = input.get_u32();
    let message_count = input.get_u32();

    Some(BlockHeader {
        size,
        message_count,
    })
}

/// Reads a raw block from the provided input
pub fn read_block(input: &mut BytesMut) -> Option<RawBlock> {
    let block_header = read_block_header(input)?;
    let size = block_header.size as usize;

    if input.len() < size {
        println!("had {} but needed {}", input.len(), size);

        return None;
    }

    let block_bytes = input.split_to(size);

    Some(RawBlock {
        header: block_header,
        bytes: block_bytes,
    })
}

/// Reads a unsigned byte from the bytes
pub fn read_u8(buf: &mut BytesMut) -> Option<u8> {
    if buf.is_empty() {
        return None;
    }

    Some(buf.get_u8())
}

/// Reads a unsigned 16bit int from the bytes
pub fn read_u16(buf: &mut BytesMut) -> Option<u16> {
    if buf.len() < 2 {
        return None;
    }

    Some(buf.get_u16())
}

/// Reads a unsigned 32bit int from the bytes
pub fn read_u32(buf: &mut BytesMut) -> Option<u32> {
    if buf.len() < 4 {
        return None;
    }

    Some(buf.get_u32())
}

/// Reads a unsigned 64bit int from the bytes
pub fn read_u64(buf: &mut BytesMut) -> Option<u64> {
    if buf.len() < 8 {
        return None;
    }

    Some(buf.get_u64())
}

/// Reads a unsigned 32bit float from the bytes
pub fn read_f32(buf: &mut BytesMut) -> Option<f32> {
    if buf.len() < 4 {
        return None;
    }

    Some(buf.get_f32())
}

/// Reads a unsigned 64bit float from the bytes
pub fn read_f64(buf: &mut BytesMut) -> Option<f64> {
    if buf.len() < 8 {
        return None;
    }

    Some(buf.get_f64())
}

/// Reads a compressed number from the provided input
pub fn read_compressed_number(buf: &mut BytesMut) -> Option<u32> {
    if !buf.has_remaining() {
        return None;
    }

    let first_byte = buf.get_u8();

    if first_byte < 0xFF {
        // The number is represented as a single byte
        Some(first_byte as u32)
    } else if buf.remaining() >= 4 {
        // The number is represented by the next 4 bytes
        Some(buf.get_u32())
    } else {
        // Not enough bytes to read the full 32-bit integer
        None
    }
}

/// Reads a cache index from the provided buffer
pub fn read_cache_index(buf: &mut BytesMut) -> Option<anyhow::Result<CacheIndex>> {
    let value = read_u16(buf)?;
    Some(CacheIndex::try_from(value).map_err(anyhow::Error::from))
}

pub fn read_byte_sequence(buf: &mut BytesMut) -> Option<Vec<u8>> {
    let length = read_compressed_number(buf)? as usize;

    if buf.len() < length {
        return None;
    }

    // Create buffer for the bytes
    let mut bytes = vec![0; length];
    buf.copy_to_slice(&mut bytes);

    Some(bytes)
}

pub fn read_string(buf: &mut BytesMut) -> Option<anyhow::Result<String>> {
    // Read the text bytes
    let text_bytes = read_byte_sequence(buf)?;

    // Get the string value back
    let value = match String::from_utf8(text_bytes) {
        Ok(value) => value,
        Err(err) => return Some(Err(anyhow::Error::from(err))),
    };

    Some(Ok(value))
}

pub fn read_oid(buf: &mut BytesMut, state: &mut ReaderState) -> Option<anyhow::Result<OID>> {
    // Load the OID value
    let value = match read_string(buf)? {
        Ok(value) => value,
        Err(err) => return Some(Err(err)),
    };

    // Load the cache index
    let cache_index = match read_cache_index(buf)? {
        Ok(value) => value,
        Err(err) => return Some(Err(err)),
    };

    let mut value = OID(value);

    match cache_index {
        CacheIndex::Valid(cache_index) => {
            if value.is_empty() {
                // Load value from cache
                value = match state.get_cached_oid(cache_index) {
                    Some(value) => value,
                    None => return Some(Err(anyhow!("cache index has no value"))),
                };
            } else {
                // Cache new value
                state.set_cached_oid(cache_index, value.clone());
            }
        }
        CacheIndex::Ignore => {
            // Empty cache with empty ID is an error
            if value.is_empty() {
                return Some(Err(anyhow!(
                    "OID was empty and no cache index was specified"
                )));
            }
        }
    };

    Some(Ok(value))
}

pub fn read_tid(buf: &mut BytesMut, state: &mut ReaderState) -> Option<anyhow::Result<TID>> {
    // Read the TID bytes
    let value = read_byte_sequence(buf)?;

    // Load the cache index
    let cache_index = match read_cache_index(buf)? {
        Ok(value) => value,
        Err(err) => return Some(Err(err)),
    };

    let mut value = TID(value);

    match cache_index {
        CacheIndex::Valid(cache_index) => {
            if value.is_empty() {
                // Load value from cache
                value = match state.get_cached_tid(cache_index) {
                    Some(value) => value,
                    None => return Some(Err(anyhow!("cache index has no value"))),
                };
            } else {
                // Cache new value
                state.set_cached_tid(cache_index, value.clone());
            }
        }
        CacheIndex::Ignore => {
            // Empty cache with empty ID is an error
            if value.is_empty() {
                return Some(Err(anyhow!(
                    "OID was empty and no cache index was specified"
                )));
            }
        }
    };

    Some(Ok(value))
}

pub fn read_type(buf: &mut BytesMut, cache: &mut ReaderState) -> Option<anyhow::Result<UnoType>> {
    if buf.is_empty() {
        return None;
    }

    let type_byte = buf.get_u8();
    let type_bit = type_byte & 0x7F;
    let cache_flag_bit = type_byte & 0x80;

    let type_class =
        match UnoTypeClass::try_from_primitive(type_bit).context("unknown uno type class bit") {
            Ok(value) => value,
            Err(err) => return Some(Err(err)),
        };

    let complex_class_type = match type_class {
        UnoTypeClass::Complex(value) => value,
        // Simple type requires no extra handling
        UnoTypeClass::Simple(value) => {
            // Cache flag should never be set when using a simple type
            debug_assert_eq!(cache_flag_bit, 0);

            return Some(Ok(UnoType::Simple(value)));
        }
    };

    let cache_index = match read_cache_index(buf)? {
        Ok(value) => value,
        Err(err) => return Some(Err(err)),
    };

    // The type value is taken from the cache table
    if cache_flag_bit == 0 {
        let cache_index = match cache_index {
            CacheIndex::Valid(cache_index) => cache_index,
            CacheIndex::Ignore => return Some(Err(anyhow!("missing cache index for type"))),
        };

        let cache_value = match cache.get_cached_type(cache_index) {
            Some(value) => value,
            None => return Some(Err(anyhow!("unknown type cache index"))),
        };

        return Some(Ok(cache_value));
    }

    // Read the type name
    let type_name = match read_string(buf)? {
        Ok(value) => value,
        Err(err) => return Some(Err(err)),
    };

    let complex_type = match UnoType::from_str_complex(&type_name) {
        Ok(value) => value,
        Err(err) => return Some(Err(err)),
    };

    if !complex_type.type_matches(&complex_class_type) {
        return Some(Err(anyhow!("complex type mismatch")));
    }

    let type_value = UnoType::Complex(complex_type);

    // Cache is set, store the type
    if let CacheIndex::Valid(cache_index) = cache_index {
        cache.set_cached_type(cache_index, type_value.clone());
    }

    Some(Ok(type_value))
}

pub fn read_value(
    buf: &mut BytesMut,
    cache: &mut ReaderState,
    ty: &UnoType,
) -> Option<anyhow::Result<UnoValue>> {
    match ty {
        UnoType::Simple(ty) => read_value_simple(buf, cache, ty),
        UnoType::Complex(ty) => read_value_complex(buf, cache, ty),
    }
}

pub fn read_value_simple(
    buf: &mut BytesMut,
    cache: &mut ReaderState,
    ty: &UnoSimpleTypeClass,
) -> Option<anyhow::Result<UnoValue>> {
    match ty {
        UnoSimpleTypeClass::Void => Some(Ok(UnoValue::Void)),
        UnoSimpleTypeClass::Boolean => {
            let value = read_u8(buf)?;
            if value > 1 {
                return Some(Err(anyhow!("invalid boolean value, expected 1 or 0")));
            }

            Some(Ok(UnoValue::Boolean(value == 1)))
        }
        UnoSimpleTypeClass::Byte => {
            let value = read_u8(buf)?;
            Some(Ok(UnoValue::Byte(value as i8)))
        }
        UnoSimpleTypeClass::Short => {
            let value = read_u16(buf)?;
            Some(Ok(UnoValue::Short(value as i16)))
        }
        UnoSimpleTypeClass::UnsignedShort => {
            let value = read_u16(buf)?;
            Some(Ok(UnoValue::UnsignedShort(value)))
        }
        UnoSimpleTypeClass::Long => {
            let value = read_u32(buf)?;
            Some(Ok(UnoValue::Long(value as i32)))
        }
        UnoSimpleTypeClass::UnsignedLong => {
            let value = read_u32(buf)?;
            Some(Ok(UnoValue::UnsignedLong(value)))
        }
        UnoSimpleTypeClass::Hyper => {
            let value = read_u64(buf)?;
            Some(Ok(UnoValue::Hyper(value as i64)))
        }
        UnoSimpleTypeClass::UnsignedHyper => {
            let value = read_u64(buf)?;
            Some(Ok(UnoValue::UnsignedHyper(value)))
        }
        UnoSimpleTypeClass::Float => {
            let value = read_f32(buf)?;
            Some(Ok(UnoValue::Float(value)))
        }
        UnoSimpleTypeClass::Double => {
            let value = read_f64(buf)?;
            Some(Ok(UnoValue::Double(value)))
        }
        UnoSimpleTypeClass::Char => {
            let value = read_u16(buf)?;
            let value = match char::from_u32(value as u32) {
                Some(value) => value,
                None => return Some(Err(anyhow!("invalid character"))),
            };
            Some(Ok(UnoValue::Char(value)))
        }
        UnoSimpleTypeClass::String => {
            let value = match read_string(buf)? {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            Some(Ok(UnoValue::String(value)))
        }
        UnoSimpleTypeClass::Type => {
            let ty = match read_type(buf, cache)? {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            Some(Ok(UnoValue::Type(ty)))
        }
        UnoSimpleTypeClass::Any => {
            let ty = match read_type(buf, cache)? {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };

            let value = match read_value(buf, cache, &ty)? {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };

            Some(Ok(UnoValue::Any(Box::new(UnoAny { ty, value }))))
        }
    }
}

pub fn read_value_complex(
    buf: &mut BytesMut,
    cache: &mut ReaderState,
    ty: &UnoComplexType,
) -> Option<anyhow::Result<UnoValue>> {
    match ty {
        UnoComplexType::Sequence { value_ty } => {
            let values = match read_value_sequence(buf, cache, value_ty)? {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };

            Some(Ok(UnoValue::Sequence(UnoSequence {
                ty: *value_ty.clone(),
                values,
            })))
        }
        UnoComplexType::Enum => todo!(),
        UnoComplexType::Struct => todo!(),
        UnoComplexType::Exception => todo!(),
        UnoComplexType::Interface => todo!(),
    }
}

pub fn read_value_sequence(
    buf: &mut BytesMut,
    cache: &mut ReaderState,
    ty: &UnoType,
) -> Option<anyhow::Result<Vec<UnoValue>>> {
    let length = read_compressed_number(buf)? as usize;
    let mut values = Vec::with_capacity(length);
    for _ in 0..length {
        let value = match read_value(buf, cache, ty)? {
            Ok(value) => value,
            Err(err) => return Some(Err(err)),
        };

        values.push(value);
    }

    Some(Ok(values))
}

pub fn read_message(input: &mut BytesMut) -> Option<Message> {
    // Input too short to be a message
    if input.is_empty() {
        return None;
    }

    // Read the flag byte
    let flags_byte = input.get_u8();
    let flags = HeaderFlags::from_bits_truncate(flags_byte);

    if flags.contains(HeaderFlags::REQUEST) {
        read_request_message(input, flags).map(Message::Request)
    } else {
        read_reply_message(input, flags).map(Message::Reply)
    }
}

fn read_request_message(input: &mut BytesMut, flags: HeaderFlags) -> Option<RequestMessage> {
    let mut extra_flags = None;

    if flags.contains(HeaderFlags::MOREFLAGS) {
        // Not enough bytes to read the next flag byte
        if input.is_empty() {
            return None;
        }

        // Read the flag byte
        let flags_byte = input.get_u8();
        let flags = HeaderExtraFlags::from_bits_truncate(flags_byte);
        extra_flags = Some(flags)
    }

    Some(RequestMessage {})
}

fn read_reply_message(input: &mut BytesMut, flags: HeaderFlags) -> Option<ReplyMessage> {
    let mut tid = None;

    if flags.contains(HeaderFlags::NEWTID) {
        let value = read_compressed_number(input)?;
        tid = Some(value);
    }

    if flags.contains(HeaderFlags::EXCEPTION) {
    } else {
    }

    Some(todo!())
}

#[cfg(test)]
mod test {
    use bytes::BytesMut;

    use super::{read_block, ReaderState};

    #[test]
    fn test_sample_initial() {
        let bytes = std::fs::read("./private/sample-initial.bin").unwrap();
        let mut bytes = BytesMut::from(bytes.as_slice());

        let mut reader_state = ReaderState::default();

        // Should be able to read all blocks from the input without issue
        while !bytes.is_empty() {
            let block = read_block(&mut bytes).unwrap();
            let mut bytes = block.bytes;
        }
    }

    #[test]
    fn test_sample_convert() {
        let bytes = std::fs::read("./private/output_file.bin").unwrap();
        let mut bytes = BytesMut::from(bytes.as_slice());

        let mut i = 0;

        // Should be able to read all blocks from the input without issue
        while !bytes.is_empty() {
            let pre = bytes.clone();

            let _block = match read_block(&mut bytes) {
                Some(value) => value,
                None => {
                    std::fs::write("./private/sample-bad.bin", pre.to_vec()).unwrap();
                    panic!("didn't have enough bytes for block {i} {}", bytes.len());
                }
            };

            if (_block.bytes.len() < 500) {
                println!("done {i} {} {:?}", _block.header.size, _block.bytes);
            } else {
                println!("done {i} {}", _block.header.size);
            }
            i += 1;
        }
    }
}
