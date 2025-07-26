from typing import Optional, Any, Dict, Callable, Tuple

class CodeType:
#    """Create a code object.  Not for the faint of heart."""
    co_argcount: int
    co_kwonlyargcount: int
    co_nlocals: int
    co_stacksize: int
    co_flags: int
    co_code: bytes
    co_consts: Tuple[Any, ...]
    co_names: Tuple[str, ...]
    co_varnames: Tuple[str, ...]
    co_filename: str
    co_name: str
    co_firstlineno: int
    co_lnotab: bytes
    co_freevars: Tuple[str, ...]
    co_cellvars: Tuple[str, ...]
    def __init__(
        self,
        argcount: int,
        kwonlyargcount: int,
        nlocals: int,
        stacksize: int,
        flags: int,
        codestring: bytes,
        constants: Tuple[Any, ...],
        names: Tuple[str, ...],
        varnames: Tuple[str, ...],
        filename: str,
        name: str,
        firstlineno: int,
        lnotab: bytes,
        freevars: Tuple[str, ...] = ...,
        cellvars: Tuple[str, ...] = ...,
    ) -> None: ...


class FrameType:
    f_back: FrameType
    f_builtins: Dict[str, Any]
    f_code: CodeType
    f_globals: Dict[str, Any]
    f_lasti: int
    f_lineno: int
    f_locals: Dict[str, Any]
    f_trace: Callable[[], None]
#    if sys.version_info >= (3, 7):
    f_trace_lines: bool
    f_trace_opcodes: bool

    def clear(self) -> None: ...

class TracebackType:
    def __init__(self, tb_next: Optional[TracebackType], tb_frame: FrameType, tb_lasti: int, tb_lineno: int) -> None: ...
    tb_next: Optional[TracebackType]
    # the rest are read-only even in 3.7
    @property
    def tb_frame(self) -> FrameType: ...
    @property
    def tb_lasti(self) -> int: ...
    @property
    def tb_lineno(self) -> int: ...
