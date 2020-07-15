use thiserror::Error;

#[derive(Debug, Error)]
pub enum GBError {
    #[error("Unknown operation {:?}", .0)]
    UnknownOperation(u16),

    #[error("Unable to write byte")]
    WriteByte,

    #[error("Unable to read byte")]
    ReadByte,

    #[error("Unable to load rom: {}", .0)]
    LoadRom(String),

    #[error("Bad command")]
    BadCommand,
}
