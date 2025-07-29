from typing import Union, Optional, Callable, Any, IO
from os import PathLike

# cheating
def open(file: Union[str, bytes, int, PathLike], mode: str = ..., buffering: int = ..., encoding: Optional[str] = ...,
             errors: Optional[str] = ..., newline: Optional[str] = ..., closefd: bool = ...,
             opener: Optional[Callable[[str, int], int]] = ...) -> IO[str]: ...
# more complicated: if mode = 'r' then IO[str], 'rb', IO[bytes], etc
