use bytes::{Buf, BytesMut};

use crate::{
    protocol::msg::{OID, TID},
    ty::UnoType,
};

use super::types::{BlockHeader, RawBlock};

/// State for the reader
pub struct ReaderState {
    // Caches for last values
    last_oid: Option<OID>,
    last_tid: Option<TID>,
    last_type: Option<UnoType>,

    // Caches for lookups
    oid_cache: [Option<OID>; 256],
    tid_cache: [Option<TID>; 256],
    type_cache: [Option<UnoType>; 256],
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

#[cfg(test)]
mod test {
    use bytes::BytesMut;

    use super::read_block;

    #[test]
    fn test_sample_initial() {
        let bytes = std::fs::read("./private/sample-initial.bin").unwrap();
        let mut bytes = BytesMut::from(bytes.as_slice());

        // Should be able to read all blocks from the input without issue
        while !bytes.is_empty() {
            let _block = read_block(&mut bytes).unwrap();
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
