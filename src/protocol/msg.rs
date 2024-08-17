use anyhow::Context;
use bitflags::bitflags;
use bytes::{Buf, BufMut, Bytes, BytesMut};
use num_enum::{FromPrimitive, TryFromPrimitive, TryFromPrimitiveError};

bitflags! {
    pub struct HeaderFlags: u8 {
        /// ALL (bit 7)
        /// SET: The first byte contains flags with further information about the message.
        /// UNSET: The message is a short request message with default flags.
        const LONGHEADER   = 0b1000_0000;
        /// ALL (bit 6)
        /// SET: The message is a request
        /// UNSET: The message is a reply
        const REQUEST      = 0b0100_0000;
        /// REQUEST ONLY (bit 5)
        /// SET: The interface type t of the method call is given explicitly, as part of the message header.
        /// UNSET: The interface type t of the method call is given implicitly, using the first-level caching mechanism for types.
        const NEWTYPE      = 0b0010_0000;
        /// REPLY ONLY (bit 5)
        /// SET: The call terminated abnormally, by throwing an exception.
        /// UNSET: The call terminated normally.
        const EXCEPTION    = 0b0010_0000;
        /// REQUEST ONLY (bit 4)
        /// SET: The OID of the method call is given explicitly, as part of the message header.
        /// UNSET: The OID of the method call is given implicitly, using the first-level caching mechanism for OIDs.
        const NEWOID       = 0b0001_0000;
        /// ALL (bit 3)
        /// SET: The TID of the method call is given explicitly, as part of the message header.
        /// UNSET: The TID of the method call is given implicitly, using the first-level caching mechanism for TIDs.
        const NEWTID       = 0b0000_1000;
        /// REQUEST ONLY (bit 2)
        /// SET: The function ID is represented as a 16-bit unsigned integer.
        /// UNSET: The function ID is represented as an 8-bit unsigned integer.
        const FUNCTIONID16 = 0b0000_0100;
        /// REQUEST ONLY (bit 1)
        /// When sending, this bit must be set to 0; when receiving, the value of this bit should be ignored.
        const RESERVED     = 0b0000_0010;
        /// REQUEST ONLY (bit 0)
        /// SET: This byte is followed by a second flag byte.
        /// UNSET: This byte is the only flag byte.
        const MOREFLAGS    = 0b0000_0001;
    }
}

bitflags! {
    /// Additional flags set when the [HeaderFlags::MOREFLAGS] bit is set
    pub struct HeaderExtraFlags: u8 {
        /// SET: A reply must be sent back.
        /// UNSET: No reply must be sent back. (For a method that would originally be synchronous, setting this bit to 1 means that information about the return value, the output values of out and in-out parameters, and thrown exceptions is effectively lost.)
        const MUSTREPLY    = 0b1000_0000;
        /// SET: Execute the call synchronously.
        /// UNSET: Execute the call asynchronously.
        const SYNCHRONOUS  = 0b0100_0000;
    }
}

pub enum Message {
    Request(RequestMessage),
    Reply(ReplyMessage),
}

bitflags! {
    #[derive(Debug)]
    struct ReplyHeaderFlags: u8 {
        const LONGHEADER = 0b1000_0000;
        const REQUEST    = 0b0100_0000; // Always 0 for reply messages
        const EXCEPTION  = 0b0010_0000;
        const NEWTID     = 0b0000_1000;
    }
}

/// Represents a block of one or more messages
#[derive(Debug)]
struct Block {
    /// The header for the block
    header: BlockHeader,
    /// The parsed messages from the block
    messages: Vec<()>,
}

/// Header describing a block of messages
#[derive(Debug)]
struct BlockHeader {
    /// The size in bytes of the following block content
    size: u32,
    /// The number of messages included in the block
    message_count: u32,
}

/// Reads a block header from the provided bytes
fn read_block_header(input: &mut BytesMut) -> Option<BlockHeader> {
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

pub type Tid = u32;

#[derive(Debug)]
struct ReplyMessage {
    // header_flags: ReplyHeaderFlags,
    // tid: Option<Tid>,
    // exception: Option<(String, Vec<u8>)>,
    // return_value: Option<Vec<u8>>,
    // out_parameters: Vec<u8>,
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
pub struct RequestMessage {
    // header_flags: HeaderFlags,
    // extra_flags: Option<HeaderExtraFlags>,
    // function_id: u16,
    // interface_type: Option<String>,
    // oid: Option<String>,
    // tid: Option<Vec<u8>>,
    // parameters: Vec<u8>,
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

fn read_compressed_number(buf: &mut BytesMut) -> Option<u32> {
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

pub enum UnoValue {
    Void,
    Boolean(u8),
    Byte(i8),
    Short(i16),
    UnsignedShort(u16),
    Long(i32),
    UnsignedLong(u32),
    Hyper(i64),
    UnsignedHyper(u64),
    Float(f32),
    Double(f64),
    Char(char),
    Any(Box<UnoAny>),
    Sequence(UnoSequence),
    Enum(i32),
    Struct(UnoStruct),
    Exception(UnoException),
    Interface(UnoInterface),
}

pub struct UnoSequence {
    pub ty: UnoTypeClass,
    pub values: Vec<UnoValue>,
}

pub struct UnoAny {
    pub ty: UnoTypeClass,
    pub value: UnoValue,
}

pub struct UnoStruct {}

pub struct UnoException {}

pub struct UnoInterface {}

/// Type class of an UNO value
#[derive(Debug, PartialEq, Eq)]
pub enum UnoTypeClass {
    Simple(UnoSimpleTypeClass),
    Complex(UnoComplexTypeClass),
}

impl TryFromPrimitive for UnoTypeClass {
    type Primitive = u8;

    type Error = TryFromPrimitiveError<Self>;

    const NAME: &'static str = stringify!(UnoTypeClass);

    fn try_from_primitive(number: Self::Primitive) -> Result<Self, Self::Error> {
        if let Ok(value) = UnoSimpleTypeClass::try_from_primitive(number) {
            return Ok(Self::Simple(value));
        }

        if let Ok(value) = UnoComplexTypeClass::try_from_primitive(number) {
            return Ok(Self::Complex(value));
        }

        Err(TryFromPrimitiveError::new(number))
    }
}

#[derive(Debug, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum UnoSimpleTypeClass {
    Void = 0,
    Boolean = 2,
    Byte = 3,
    Short = 4,
    UnsignedShort = 5,
    Long = 6,
    UnsignedLong = 7,
    Hyper = 8,
    UnsignedHyper = 9,
    Float = 10,
    Double = 11,
    Char = 1,
    String = 12,
    Type = 13,
    Any = 14,
}

#[derive(Debug, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum UnoComplexTypeClass {
    Sequence = 20,
    Enum = 15,
    Struct = 17,
    Exception = 19,
    Interface = 22,
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnoType {
    Simple(UnoSimpleTypeClass),
    Complex(UnoComplexTypeClass, Option<String>),
}

fn read_type(buf: &mut BytesMut) -> Option<anyhow::Result<UnoType>> {
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

    let complex_type = match type_class {
        UnoTypeClass::Complex(value) => value,
        // Simple type requires no extra handling
        UnoTypeClass::Simple(value) => {
            // Cache flag should never be set when using a simple type
            debug_assert_eq!(cache_flag_bit, 0);

            return Some(Ok(UnoType::Simple(value)));
        }
    };

    // The type value is taken from the cache table
    if cache_flag_bit == 0 {
        return Some(Ok(UnoType::Complex(complex_type, None)));
    }

    // Read the type name
    let type_value = match read_string(buf)? {
        Ok(value) => value,
        Err(err) => return Some(Err(err)),
    };

    Some(Ok(UnoType::Complex(complex_type, Some(type_value))))
}

fn write_type(buf: &mut BytesMut, ty: UnoType) {
    todo!()
}

fn read_string(buf: &mut BytesMut) -> Option<anyhow::Result<String>> {
    let length = read_compressed_number(buf)? as usize;

    if buf.len() < length {
        return None;
    }

    // Create buffer for the text bytes
    let mut text_bytes = vec![0; length];
    buf.copy_to_slice(&mut text_bytes);

    // Get the string value back
    let value = match String::from_utf8(text_bytes).context("expected utf8 string value") {
        Ok(value) => value,
        Err(err) => return Some(Err(err)),
    };

    Some(Ok(value))
}

fn write_string(buf: &mut BytesMut, value: &str) {
    let length = value.len();

    // Ensure the max length is not reached
    debug_assert!(length < u32::MAX as usize);

    // Write the length number
    write_compressed_number(buf, length as u32);

    // Copy the string bytes onto the buffer
    buf.copy_from_slice(value.as_bytes());
}
