from typing import Any, Type, Optional, Union, Tuple, Callable, Iterator

class JSONEncoder:
    item_separator: str
    key_separator: str

    skipkeys: bool
    ensure_ascii: bool
    check_circular: bool
    allow_nan: bool
    sort_keys: bool
    indent: int

    def __init__(self, skipkeys: bool = ..., ensure_ascii: bool = ...,
                 check_circular: bool = ..., allow_nan: bool = ..., sort_keys: bool = ...,
                 indent: Optional[int] = ..., separators: Optional[Tuple[str, str]] = ...,
                 default: Optional[Callable] = ...) -> None: ...

    def default(self, o: Any) -> Any: ...
    def encode(self, o: Any) -> str: ...
    def iterencode(self, o: Any, _one_shot: bool = ...) -> Iterator[str]: ...


def dumps(obj: Any,
          skipkeys: bool = ...,
          ensure_ascii: bool = ...,
          check_circular: bool = ...,
          allow_nan: bool = ...,
          cls: Optional[Type[JSONEncoder]] = ...,
          indent: Union[None, int, str] = ...,
          separators: Optional[Tuple[str, str]] = ...,
          default: Optional[Callable[[Any], Any]] = ...,
          sort_keys: bool = ...,
          #**kwds: Any
) -> str: ...
