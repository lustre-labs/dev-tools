// IMPORTS ---------------------------------------------------------------------

import gleam/httpc
import simplifile

// TYPES -----------------------------------------------------------------------

pub type Error {
  UnsupportedPlatform(os: String, arch: String)
  NetworkError(httpc.HttpError)
  UnexpectedResponse(downloading: String, status: Int, body: String)
  CouldNotVerifyHash(for: String, expected: String, actual: String)
  CouldNotReadFile(path: String, reason: simplifile.FileError)
  CouldNotWriteDirectoryOrFile(path: String, reason: simplifile.FileError)
}
