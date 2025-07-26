from typing import Any, Tuple, Union, Optional

_TimeTuple = Tuple[int, int, int, int, int, int, int, int, int]

altzone: int
daylight: int
timezone: int
tzname: Tuple[str, str]


class struct_time:
    tm_year: int
    tm_mon: int
    tm_mday: int
    tm_hour: int
    tm_min: int
    tm_sec: int
    tm_wday: int
    tm_yday: int
    tm_isdst: int
    tm_zone: str
    tm_gmtoff: int


def strftime(format: str, t: struct_time = ...) -> str: ... # cheating Union[_TimeTuple, struct_time] = ...
def strptime(string: str, format: str = ...) -> struct_time: ...
def localtime(secs: Optional[float] = ...) -> struct_time: ...
