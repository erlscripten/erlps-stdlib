module Erl.Posix.Msg(erlps__message__1) where
{-
This file has been autogenerated
DO NOT EDIT - Your changes WILL be overwritten
Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote
Erlscripten v0.2.0
-}

import Prelude
import Data.BigInt as DBI
import Data.Array as DA
import Data.Maybe as DM
import Data.Map as Map
import Data.Tuple as DT
import Erlang.Builtins as BIF
import Erlang.Binary as BIN
import Erlang.Helpers
import Erlang.Exception as EXC
import Erlang.Type
import Partial.Unsafe (unsafePartial)


erlps__message__1 :: ErlangFun
erlps__message__1 [t_0] =
  let arg_1 = erlps__message_1__1 [t_0]
  in BIF.erlang__binary_to_list__1 [arg_1]
erlps__message__1 [arg_3] = EXC.function_clause unit
erlps__message__1 args =
  EXC.badarity (ErlangFun 1 erlps__message__1) args

erlps__message_1__1 :: ErlangFun
erlps__message_1__1 [(ErlangAtom "e2big")] =
  ErlangBinary
    (BIN.fromInts (toErl "argument list too long") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eacces")] =
  ErlangBinary
    (BIN.fromInts (toErl "permission denied") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eaddrinuse")] =
  ErlangBinary
    (BIN.fromInts (toErl "address already in use") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eaddrnotavail")] =
  ErlangBinary
    (BIN.fromInts (toErl "can\'t assign requested address") (toErl 8)
       1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eadv")] =
  ErlangBinary
    (BIN.fromInts (toErl "advertise error") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eafnosupport")] =
  ErlangBinary
    (BIN.fromInts
       (toErl "address family not supported by protocol family")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eagain")] =
  ErlangBinary
    (BIN.fromInts (toErl "resource temporarily unavailable")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ealign")] =
  ErlangBinary (BIN.fromInts (toErl "EALIGN") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ealready")] =
  ErlangBinary
    (BIN.fromInts (toErl "operation already in progress") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebade")] =
  ErlangBinary
    (BIN.fromInts (toErl "bad exchange descriptor") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebadf")] =
  ErlangBinary
    (BIN.fromInts (toErl "bad file number") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebadfd")] =
  ErlangBinary
    (BIN.fromInts (toErl "file descriptor in bad state") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebadmsg")] =
  ErlangBinary
    (BIN.fromInts (toErl "not a data message") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebadr")] =
  ErlangBinary
    (BIN.fromInts (toErl "bad request descriptor") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebadrpc")] =
  ErlangBinary
    (BIN.fromInts (toErl "RPC structure is bad") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebadrqc")] =
  ErlangBinary
    (BIN.fromInts (toErl "bad request code") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebadslt")] =
  ErlangBinary
    (BIN.fromInts (toErl "invalid slot") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebfont")] =
  ErlangBinary
    (BIN.fromInts (toErl "bad font file format") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ebusy")] =
  ErlangBinary
    (BIN.fromInts (toErl "file busy") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "echild")] =
  ErlangBinary
    (BIN.fromInts (toErl "no children") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "echrng")] =
  ErlangBinary
    (BIN.fromInts (toErl "channel number out of range") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "ecomm")] =
  ErlangBinary
    (BIN.fromInts (toErl "communication error on send") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "econnaborted")] =
  ErlangBinary
    (BIN.fromInts (toErl "software caused connection abort")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "econnrefused")] =
  ErlangBinary
    (BIN.fromInts (toErl "connection refused") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "econnreset")] =
  ErlangBinary
    (BIN.fromInts (toErl "connection reset by peer") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "edeadlk")] =
  ErlangBinary
    (BIN.fromInts (toErl "resource deadlock avoided") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "edeadlock")] =
  ErlangBinary
    (BIN.fromInts (toErl "resource deadlock avoided") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "edestaddrreq")] =
  ErlangBinary
    (BIN.fromInts (toErl "destination address required") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "edirty")] =
  ErlangBinary
    (BIN.fromInts (toErl "mounting a dirty fs w/o force") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "edom")] =
  ErlangBinary
    (BIN.fromInts (toErl "math argument out of range") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "edotdot")] =
  ErlangBinary
    (BIN.fromInts (toErl "cross mount point") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "edquot")] =
  ErlangBinary
    (BIN.fromInts (toErl "disk quota exceeded") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eduppkg")] =
  ErlangBinary
    (BIN.fromInts (toErl "duplicate package name") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eexist")] =
  ErlangBinary
    (BIN.fromInts (toErl "file already exists") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "efault")] =
  ErlangBinary
    (BIN.fromInts (toErl "bad address in system call argument")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "efbig")] =
  ErlangBinary
    (BIN.fromInts (toErl "file too large") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eftype")] =
  ErlangBinary (BIN.fromInts (toErl "EFTYPE") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ehostdown")] =
  ErlangBinary
    (BIN.fromInts (toErl "host is down") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ehostunreach")] =
  ErlangBinary
    (BIN.fromInts (toErl "host is unreachable") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eidrm")] =
  ErlangBinary
    (BIN.fromInts (toErl "identifier removed") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "einit")] =
  ErlangBinary
    (BIN.fromInts (toErl "initialization error") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "einprogress")] =
  ErlangBinary
    (BIN.fromInts (toErl "operation now in progress") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eintr")] =
  ErlangBinary
    (BIN.fromInts (toErl "interrupted system call") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "einval")] =
  ErlangBinary
    (BIN.fromInts (toErl "invalid argument") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eio")] =
  ErlangBinary
    (BIN.fromInts (toErl "I/O error") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eisconn")] =
  ErlangBinary
    (BIN.fromInts (toErl "socket is already connected") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eisdir")] =
  ErlangBinary
    (BIN.fromInts (toErl "illegal operation on a directory")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eisnam")] =
  ErlangBinary
    (BIN.fromInts (toErl "is a name file") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "elbin")] =
  ErlangBinary (BIN.fromInts (toErl "ELBIN") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "el2hlt")] =
  ErlangBinary
    (BIN.fromInts (toErl "level 2 halted") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "el2nsync")] =
  ErlangBinary
    (BIN.fromInts (toErl "level 2 not synchronized") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "el3hlt")] =
  ErlangBinary
    (BIN.fromInts (toErl "level 3 halted") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "el3rst")] =
  ErlangBinary
    (BIN.fromInts (toErl "level 3 reset") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "elibacc")] =
  ErlangBinary
    (BIN.fromInts (toErl "cannot access a needed shared library")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "elibbad")] =
  ErlangBinary
    (BIN.fromInts (toErl "accessing a corrupted shared library")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "elibexec")] =
  ErlangBinary
    (BIN.fromInts (toErl "cannot exec a shared library directly")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "elibmax")] =
  ErlangBinary
    (BIN.fromInts
       (toErl
          "attempting to link in more shared libraries than system limit")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "elibscn")] =
  ErlangBinary
    (BIN.fromInts (toErl ".lib section in a.out corrupted") (toErl 8)
       1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "elnrng")] =
  ErlangBinary
    (BIN.fromInts (toErl "link number out of range") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eloop")] =
  ErlangBinary
    (BIN.fromInts (toErl "too many levels of symbolic links")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "emfile")] =
  ErlangBinary
    (BIN.fromInts (toErl "too many open files") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "emlink")] =
  ErlangBinary
    (BIN.fromInts (toErl "too many links") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "emsgsize")] =
  ErlangBinary
    (BIN.fromInts (toErl "message too long") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "emultihop")] =
  ErlangBinary
    (BIN.fromInts (toErl "multihop attempted") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enametoolong")] =
  ErlangBinary
    (BIN.fromInts (toErl "file name too long") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enavail")] =
  ErlangBinary
    (BIN.fromInts (toErl "not available") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enet")] =
  ErlangBinary (BIN.fromInts (toErl "ENET") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enetdown")] =
  ErlangBinary
    (BIN.fromInts (toErl "network is down") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enetreset")] =
  ErlangBinary
    (BIN.fromInts (toErl "network dropped connection on reset")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enetunreach")] =
  ErlangBinary
    (BIN.fromInts (toErl "network is unreachable") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enfile")] =
  ErlangBinary
    (BIN.fromInts (toErl "file table overflow") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enoano")] =
  ErlangBinary
    (BIN.fromInts (toErl "anode table overflow") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enobufs")] =
  ErlangBinary
    (BIN.fromInts (toErl "no buffer space available") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enocsi")] =
  ErlangBinary
    (BIN.fromInts (toErl "no CSI structure available") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enodata")] =
  ErlangBinary
    (BIN.fromInts (toErl "no data available") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enodev")] =
  ErlangBinary
    (BIN.fromInts (toErl "no such device") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enoent")] =
  ErlangBinary
    (BIN.fromInts (toErl "no such file or directory") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enoexec")] =
  ErlangBinary
    (BIN.fromInts (toErl "exec format error") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enolck")] =
  ErlangBinary
    (BIN.fromInts (toErl "no locks available") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enolink")] =
  ErlangBinary
    (BIN.fromInts (toErl "link has be severed") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enomem")] =
  ErlangBinary
    (BIN.fromInts (toErl "not enough memory") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enomsg")] =
  ErlangBinary
    (BIN.fromInts (toErl "no message of desired type") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enonet")] =
  ErlangBinary
    (BIN.fromInts (toErl "machine is not on the network") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enopkg")] =
  ErlangBinary
    (BIN.fromInts (toErl "package not installed") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enoprotoopt")] =
  ErlangBinary
    (BIN.fromInts (toErl "bad proocol option") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enospc")] =
  ErlangBinary
    (BIN.fromInts (toErl "no space left on device") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enosr")] =
  ErlangBinary
    (BIN.fromInts
       (toErl "out of stream resources or not a stream device")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enostr")] =
  ErlangBinary
    (BIN.fromInts (toErl "not a stream") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enosym")] =
  ErlangBinary
    (BIN.fromInts (toErl "unresolved symbol name") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enosys")] =
  ErlangBinary
    (BIN.fromInts (toErl "function not implemented") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enotblk")] =
  ErlangBinary
    (BIN.fromInts (toErl "block device required") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enotconn")] =
  ErlangBinary
    (BIN.fromInts (toErl "socket is not connected") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enotdir")] =
  ErlangBinary
    (BIN.fromInts (toErl "not a directory") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enotempty")] =
  ErlangBinary
    (BIN.fromInts (toErl "directory not empty") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enotnam")] =
  ErlangBinary
    (BIN.fromInts (toErl "not a name file") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enotsock")] =
  ErlangBinary
    (BIN.fromInts (toErl "socket operation on non-socket") (toErl 8)
       1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enotsup")] =
  ErlangBinary
    (BIN.fromInts (toErl "operation not supported") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enotty")] =
  ErlangBinary
    (BIN.fromInts (toErl "inappropriate device for ioctl") (toErl 8)
       1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "enotuniq")] =
  ErlangBinary
    (BIN.fromInts (toErl "name not unique on network") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "enxio")] =
  ErlangBinary
    (BIN.fromInts (toErl "no such device or address") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eopnotsupp")] =
  ErlangBinary
    (BIN.fromInts (toErl "operation not supported on socket")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eoverflow")] =
  ErlangBinary
    (BIN.fromInts (toErl "offset too large for file system")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eperm")] =
  ErlangBinary
    (BIN.fromInts (toErl "not owner") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "epfnosupport")] =
  ErlangBinary
    (BIN.fromInts (toErl "protocol family not supported") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "epipe")] =
  ErlangBinary
    (BIN.fromInts (toErl "broken pipe") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eproclim")] =
  ErlangBinary
    (BIN.fromInts (toErl "too many processes") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eprocunavail")] =
  ErlangBinary
    (BIN.fromInts (toErl "bad procedure for program") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eprogmismatch")] =
  ErlangBinary
    (BIN.fromInts (toErl "program version wrong") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eprogunavail")] =
  ErlangBinary
    (BIN.fromInts (toErl "RPC program not available") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eproto")] =
  ErlangBinary
    (BIN.fromInts (toErl "protocol error") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eprotonosupport")] =
  ErlangBinary
    (BIN.fromInts (toErl "protocol not suppored") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eprototype")] =
  ErlangBinary
    (BIN.fromInts (toErl "protocol wrong type for socket") (toErl 8)
       1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "erange")] =
  ErlangBinary
    (BIN.fromInts (toErl "math result unrepresentable") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "erefused")] =
  ErlangBinary
    (BIN.fromInts (toErl "EREFUSED") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eremchg")] =
  ErlangBinary
    (BIN.fromInts (toErl "remote address changed") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eremdev")] =
  ErlangBinary
    (BIN.fromInts (toErl "remote device") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eremote")] =
  ErlangBinary
    (BIN.fromInts (toErl "pathname hit remote file system") (toErl 8)
       1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eremoteio")] =
  ErlangBinary
    (BIN.fromInts (toErl "remote i/o error") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eremoterelease")] =
  ErlangBinary
    (BIN.fromInts (toErl "EREMOTERELEASE") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "erofs")] =
  ErlangBinary
    (BIN.fromInts (toErl "read-only file system") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "erpcmismatch")] =
  ErlangBinary
    (BIN.fromInts (toErl "RPC version is wrong") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "erremote")] =
  ErlangBinary
    (BIN.fromInts (toErl "object is remote") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eshutdown")] =
  ErlangBinary
    (BIN.fromInts (toErl "can\'t send after socket shutdown")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "esocktnosupport")] =
  ErlangBinary
    (BIN.fromInts (toErl "socket type not supported") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "espipe")] =
  ErlangBinary
    (BIN.fromInts (toErl "invalid seek") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "esrch")] =
  ErlangBinary
    (BIN.fromInts (toErl "no such process") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "esrmnt")] =
  ErlangBinary
    (BIN.fromInts (toErl "srmount error") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "estale")] =
  ErlangBinary
    (BIN.fromInts (toErl "stale remote file handle") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "esuccess")] =
  ErlangBinary (BIN.fromInts (toErl "Error 0") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "etime")] =
  ErlangBinary
    (BIN.fromInts (toErl "timer expired") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "etimedout")] =
  ErlangBinary
    (BIN.fromInts (toErl "connection timed out") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "etoomanyrefs")] =
  ErlangBinary
    (BIN.fromInts (toErl "too many references: can\'t splice")
       (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "etxtbsy")] =
  ErlangBinary
    (BIN.fromInts (toErl "text file or pseudo-device busy") (toErl 8)
       1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "euclean")] =
  ErlangBinary
    (BIN.fromInts (toErl "structure needs cleaning") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eunatch")] =
  ErlangBinary
    (BIN.fromInts (toErl "protocol driver not attached") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "eusers")] =
  ErlangBinary
    (BIN.fromInts (toErl "too many users") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "eversion")] =
  ErlangBinary
    (BIN.fromInts (toErl "version mismatch") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "ewouldblock")] =
  ErlangBinary
    (BIN.fromInts (toErl "operation would block") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "exdev")] =
  ErlangBinary
    (BIN.fromInts (toErl "cross-domain link") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "exfull")] =
  ErlangBinary
    (BIN.fromInts (toErl "message tables full") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "nxdomain")] =
  ErlangBinary
    (BIN.fromInts (toErl "non-existing domain") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [(ErlangAtom "exbadport")] =
  ErlangBinary
    (BIN.fromInts (toErl "inet_drv bad port state") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [(ErlangAtom "exbadseq")] =
  ErlangBinary
    (BIN.fromInts (toErl "inet_drv bad request sequence") (toErl 8) 1
       BIN.Big)
erlps__message_1__1 [_] =
  ErlangBinary
    (BIN.fromInts (toErl "unknown POSIX error") (toErl 8) 1 BIN.Big)
erlps__message_1__1 [arg_0] = EXC.function_clause unit
erlps__message_1__1 args =
  EXC.badarity (ErlangFun 1 erlps__message_1__1) args