##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2019 The MOPSA Project.                                     #
#                                                                            #
#  This program is free software: you can redistribute it and/or modify      #
#  it under the terms of the GNU Lesser General Public License as published  #
#  by the Free Software Foundation, either version 3 of the License, or      #
#  (at your option) any later version.                                       #
#                                                                            #
#  This program is distributed in the hope that it will be useful,           #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#  GNU Lesser General Public License for more details.                       #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
##############################################################################

from typing import Union, Optional, Dict, Generic, AnyStr, Iterator, ContextManager, overload, Any, Callable, Set, Text, Protocol, IO
import posixpath as path

environ : Dict[str, str]
sep: str
name: str

def urandom(__size: int) -> bytes: ...

class PathLike(Protocol[AnyStr]):
    def __fspath__(self) -> AnyStr: ...

_PathType = Union[bytes, Text, PathLike] # cheating _PathType = path._PathType
_FdOrPathType = Union[int, _PathType]



class stat_result:
    # For backward compatibility, the return value of stat() is also
    # accessible as a tuple of at least 10 integers giving the most important
    # (and portable) members of the stat structure, in the order st_mode,
    # st_ino, st_dev, st_nlink, st_uid, st_gid, st_size, st_atime, st_mtime,
    # st_ctime. More items may be added at the end by some implementations.

    st_mode: int  # protection bits,
    st_ino: int  # inode number,
    st_dev: int  # device,
    st_nlink: int  # number of hard links,
    st_uid: int  # user id of owner,
    st_gid: int  # group id of owner,
    st_size: int  # size of file, in bytes,
    st_atime: float  # time of most recent access,
    st_mtime: float  # time of most recent content modification,
    st_ctime: float  # platform dependent (time of most recent metadata change on Unix, or the time of creation on Windows)
    st_atime_ns: int  # time of most recent access, in nanoseconds
    st_mtime_ns: int  # time of most recent content modification in nanoseconds
    st_ctime_ns: int  # platform dependent (time of most recent metadata change on Unix, or the time of creation on Windows) in nanoseconds

def stat(path: _FdOrPathType, dir_fd: Optional[int] = ..., follow_symlinks: bool = ...) -> stat_result: ...

def open(file: _PathType, flags: int, mode: int = ..., *, dir_fd: Optional[int] = ...) -> int: ...
def remove(path: _PathType, *, dir_fd: Optional[int] = ...) -> None: ...

def stat(path: _FdOrPathType, *, dir_fd: Optional[int] = ..., follow_symlinks: bool = ...) -> stat_result: ...
def unlink(path: _PathType, *, dir_fd: Optional[int] = ...) -> None: ...
def rmdir(path: _PathType, *, dir_fd: Optional[int] = ...) -> None: ...
def makedirs(name: _PathType, mode: int = ..., exist_ok: bool = ...) -> None: ...

class DirEntry(PathLike[AnyStr]):
    # This is what the scandir interator yields
    # The constructor is hidden

    name: AnyStr
    path: AnyStr
    def inode(self) -> int: ...
    def is_dir(self, *, follow_symlinks: bool = ...) -> bool: ...
    def is_file(self, *, follow_symlinks: bool = ...) -> bool: ...
    def is_symlink(self) -> bool: ...
    def stat(self, *, follow_symlinks: bool = ...) -> stat_result: ...

    def __fspath__(self) -> AnyStr: ...


class _ScandirIterator(Iterator[DirEntry[AnyStr]],ContextManager[_ScandirIterator[AnyStr]]):
    def __next__(self) -> DirEntry[AnyStr]: ...
    def close(self) -> None: ...

@overload
def scandir() -> _ScandirIterator[str]: ...
@overload
def scandir(path: int) -> _ScandirIterator[str]: ...
@overload
def scandir(path: Union[AnyStr, PathLike[AnyStr]]) -> _ScandirIterator[AnyStr]: ...


supports_dir_fd: Set[Callable[..., Any]]
supports_fd: Set[Callable[..., Any]]
supports_follow_symlinks: Set[Callable[..., Any]]
