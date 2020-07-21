use thiserror::Error;

#[derive(Debug, Error)]
pub enum GBError {
    #[error("Unknown operation {:?}", .0)]
    UnknownOperation(u8),

    #[error("Unable to write byte")]
    WriteByte,

    #[error("Write into register")]
    WriteIntoRegister,

    #[error("Unable to read byte")]
    ReadByte,

    #[error("Unable to load rom: {}", .0)]
    LoadRom(String),

    #[error("Bad command")]
    BadCommand,
}
