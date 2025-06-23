// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
import gleam/bool
import gleam/crypto
import gleam/http
import gleam/http/request.{Request}
import gleam/http/response.{Response}
import gleam/httpc
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project
import lustre_dev_tools/system
import simplifile

//

const slugs = [
  #(#("android", "arm"), "android-arm/-/android-arm-0.25.2.tgz"),
  #(#("android", "arm64"), "android-arm64/-/android-arm64-0.25.2.tgz"),
  #(#("android", "x64"), "android-x64/-/android-x64-0.25.2.tgz"),
  #(#("darwin", "aarch64"), "darwin-arm64/-/darwin-arm64-0.25.2.tgz"),
  #(#("darwin", "arm64"), "darwin-arm64/-/darwin-arm64-0.25.2.tgz"),
  #(#("darwin", "amd64"), "darwin-x64/-/darwin-x64-0.25.2.tgz"),
  #(#("darwin", "x86_64"), "darwin-x64/-/darwin-x64-0.25.2.tgz"),
  #(#("freebsd", "aarch64"), "freebsd-arm64/-/freebsd-arm64-0.25.2.tgz"),
  #(#("freebsd", "amd64"), "freebsd-x64/-/freebsd-x64-0.25.2.tgz"),
  #(#("linux", "arm"), "linux-arm/-/linux-arm-0.25.2.tgz"),
  #(#("linux", "aarch64"), "linux-arm64/-/linux-arm64-0.25.2.tgz"),
  #(#("linux", "arm64"), "linux-arm64/-/linux-arm64-0.25.2.tgz"),
  #(#("linux", "ia32"), "linux-ia32/-/linux-ia32-0.25.2.tgz"),
  #(#("linux", "x64"), "linux-x64/-/linux-x64-0.25.2.tgz"),
  #(#("linux", "x86_64"), "linux-x64/-/linux-x64-0.25.2.tgz"),
  #(#("netbsd", "x64"), "netbsd-x64/-/netbsd-x64-0.25.2.tgz"),
  #(#("openbsd", "arm64"), "openbsd-arm64/-/openbsd-arm64-0.25.2.tgz"),
  #(#("openbsd", "x86_64"), "openbsd-x64/-/openbsd-x64-0.25.2.tgz"),
  #(#("sunos", "x64"), "sunos-x64/-/sunos-x64-0.25.2.tgz"),
  #(#("win32", "arm64"), "win32-arm64/-/win32-arm64-0.25.2.tgz"),
  #(#("win32", "ia32"), "win32-ia32/-/win32-ia32-0.25.2.tgz"),
  #(#("win32", "x64"), "win32-x64/-/win32-x64-0.25.2.tgz"),
  #(#("win32", "x86_64"), "win32-x64/-/win32-x64-0.25.2.tgz"),
]
