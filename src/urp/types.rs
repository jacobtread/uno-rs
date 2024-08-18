use std::ops::Deref;

use bitflags::bitflags;
use bytes::BytesMut;
use thiserror::Error;

use crate::ty::{UnoType, UnoTypeClass};

/// Header describing a block of messages
#[derive(Debug)]
pub struct BlockHeader {
    /// The size in bytes of the following block content
    pub size: u32,
    /// The number of messages included in the block
    pub message_count: u32,
}

/// Represents a block with its header and contents.
///
/// Where the contents are not yet decoded
pub struct RawBlock {
    /// The header for the block
    pub header: BlockHeader,
    /// Byte contents of the block
    pub bytes: BytesMut,
}

/// Represents a block of one or more messages
#[derive(Debug)]
struct Block {
    /// The header for the block
    pub header: BlockHeader,
    /// The parsed messages from the block
    pub messages: Vec<()>,
}

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

#[derive(Debug)]
pub enum Message {
    Request(RequestMessage),
    Reply(ReplyMessage),
}

#[derive(Debug)]
pub struct ReplyMessage {}

#[derive(Debug)]
pub struct RequestMessage {}

/// Index into a cache
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CacheIndex {
    /// Checked for valid bounds, valid cache index
    Valid(usize),
    /// Ignored cache
    Ignore,
}

pub const CACHE_IGNORE: u16 = 0xFFFF;
pub const CACHE_SIZE: usize = 256;

#[derive(Debug, Error)]
#[error("cache index was not within allowed range")]
pub struct CacheIndexOutOfBounds;

impl TryFrom<u16> for CacheIndex {
    type Error = CacheIndexOutOfBounds;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        // Special cache ignore index
        if value == CACHE_IGNORE {
            return Ok(Self::Ignore);
        }

        let value = value as usize;

        // Cache bounds check
        if value >= CACHE_SIZE {
            return Err(CacheIndexOutOfBounds);
        }

        Ok(Self::Valid(value))
    }
}

/// Uno Thread Identifier
#[derive(Debug, Clone)]
pub struct TID(pub Vec<u8>);

impl AsRef<[u8]> for TID {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl Deref for TID {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Object Identifier
///
/// Globally unique object identifier, always a non-empty ASCII string.
#[derive(Debug, Clone)]
pub struct OID(pub String);

impl AsRef<str> for OID {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Deref for OID {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl OID {
    pub fn value(&self) -> &str {
        &self.0
    }

    pub fn into_inner(self) -> String {
        self.0
    }
}

impl From<OID> for String {
    fn from(value: OID) -> Self {
        value.into_inner()
    }
}

pub enum UnoValue {
    Void,
    Boolean(bool),
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
    String(String),
    Type(UnoType),
    Any(Box<UnoAny>),
    Sequence(UnoSequence),
    Enum(UnoEnum),
    Struct(UnoStruct),
    Exception(UnoException),
    Interface(UnoInterface),
}

pub struct UnoAny {
    pub ty: UnoType,
    pub value: UnoValue,
}

pub struct UnoSequence {
    pub ty: UnoType,
    pub values: Vec<UnoValue>,
}

pub struct UnoEnum {
    pub id: OID,
    pub variant: i32,
}

pub struct UnoStruct {
    pub id: OID,
    pub values: Vec<UnoValue>,
}

pub struct UnoException(pub UnoStruct);

pub enum UnoInterfaceReference {
    /// Null reference
    Null,
    /// Object the interface references
    Object(OID),
}

pub struct UnoInterface {
    pub id: OID,
}
