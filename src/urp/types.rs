use bytes::BytesMut;

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
