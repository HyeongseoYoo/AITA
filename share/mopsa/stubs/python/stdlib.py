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

import mopsa


###############
##  Classes  ##
###############

class object:
    def __new__(cls, args): pass
    @mopsa.type("wrapper_descriptor")
    def __init__(self, args): pass
    @mopsa.stub
    @mopsa.type("wrapper_descriptor")
    def __eq__(self, other): return NotImplemented
    @mopsa.stub
    @mopsa.type("wrapper_descriptor")
    def __ne__(self, other): return NotImplemented
    @mopsa.stub
    @mopsa.type("wrapper_descriptor")
    def __ge__(self, other): return NotImplemented
    @mopsa.stub
    @mopsa.type("wrapper_descriptor")
    def __gt__(self, other): return NotImplemented
    @mopsa.stub
    @mopsa.type("wrapper_descriptor")
    def __lt__(self, other): return NotImplemented
    @mopsa.stub
    @mopsa.type("wrapper_descriptor")
    def __le__(self, other): return NotImplemented
    @mopsa.type("method_descriptor")
    def __format__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __getattribute__(self, attr): pass
    @mopsa.type("wrapper_descriptor")
    def __delattr__(self, attr): pass
    @mopsa.type("wrapper_descriptor")
    def __hash__(self, attr): pass
    @mopsa.type("method_descriptor")
    def __reduce__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __setattr__(self, attr, v): pass
    @mopsa.type("method_descriptor")
    def __sizeof__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __str__(self): pass
    def __init_subclass__(cls): pass

class type(object):
    def __new__(cls, args): pass
    @mopsa.type("wrapper_descriptor")
    def __getattribute__(self, attr): pass
    @mopsa.type("wrapper_descriptor")
    def __setattr__(self, attr): pass
    @mopsa.type("wrapper_descriptor")
    def __init__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __call__(self, args): pass
    @mopsa.type("wrapper_descriptor")
    def __repr__(self): pass

class function(object):
    def __new__(cls, args): pass
    @mopsa.type("wrapper_descriptor")
    def __call__(self, args): pass
    @mopsa.type("wrapper_descriptor")
    def __get__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __repr__(self): pass

class builtin_function_or_method(object):
    @mopsa.type("wrapper_descriptor")
    def __call__(self, args): pass

class wrapper_descriptor(object):
    @mopsa.type("wrapper_descriptor")
    def __get__(self): pass

class method(object):
    def __new__(cls, args): pass
    @mopsa.type("wrapper_descriptor")
    def __call__(self, args): pass

class method_descriptor(object):
    def __get__(self): pass

class member_descriptor(object):
    @mopsa.type("wrapper_descriptor")
    def __get__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __set__(self, val): pass

class module(object):
    def __new__(cls, args): pass

class int(object):
    @mopsa.type("wrapper_descriptor")
    def __abs__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __add__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __and__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __bool__(self): pass
    @mopsa.type("method_descriptor")
    def __ceil__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __divmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __eq__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __float__(self): pass
    @mopsa.type("method_descriptor")
    def __floor__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __floordiv__(self, other): pass
    @mopsa.type("method_descriptor")
    def __format__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __ge__(self, other): pass
     # def __getattribute__(self, attr): pass
    @mopsa.type("method_descriptor")
    def __getnewargs__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __gt__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __index__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __int__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __invert__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __le__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __lshift__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __lt__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __mod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __mul__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __ne__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __neg__(self): pass
    def __new__(cls, args): pass
    @mopsa.type("wrapper_descriptor")
    def __or__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __pow__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __pos__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __radd__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rand__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rdivmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __repr__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __rfloordiv__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rshift__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rmul__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __ror__(self, other): pass
    @mopsa.type("method_descriptor")
    def __round__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __rpow__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rrshift__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rshift__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rsub__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rtruediv__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rxor__(self, other): pass
    @mopsa.type("method_descriptor")
    def __sizeof__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __str__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __sub__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __truediv__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __xor__(self, other): pass

class bool(int):
    @mopsa.type("wrapper_descriptor")
    def __and__(self, other): pass
    def __new__(self, args): pass
    @mopsa.type("wrapper_descriptor")
    def __or__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rand__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __repr__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __ror__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rxor__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __str__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __xor__(self, other): pass

class float(object):
    @mopsa.type("wrapper_descriptor")
    def __abs__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __add__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __bool__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __divmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __eq__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __float__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __floordiv__(self, other): pass
    @mopsa.type("method_descriptor")
    def __format__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __ge__(self, other): pass
     # def __getattribute__(self, attr): pass
    @mopsa.type("method_descriptor")
    def __getnewargs__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __gt__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __int__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __le__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __lt__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __mod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __mul__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __ne__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __neg__(self): pass
    def __new__(cls, args): pass
    @mopsa.type("wrapper_descriptor")
    def __pow__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __pos__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __radd__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rdivmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __repr__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __rfloordiv__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rmul__(self, other): pass
    @mopsa.type("method_descriptor")
    def __round__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __rpow__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rsub__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rtruediv__(self, other): pass
    @mopsa.type("method_descriptor")
    def __sizeof__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __str__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __sub__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __truediv__(self, other): pass
    @mopsa.type("method_descriptor")
    def __trunc__(self): pass

class complex(object):
    @mopsa.type("wrapper_descriptor")
    def __abs__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __add__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __bool__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __divmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __eq__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __float__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __floordiv__(self, other): pass
    @mopsa.type("method_descriptor")
    def __format__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __ge__(self, other): pass
     # def __getattribute__(self, attr): pass
    @mopsa.type("method_descriptor")
    def __getnewargs__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __gt__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __int__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __le__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __lt__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __mod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __mul__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __ne__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __neg__(self): pass
    def __new__(cls, args): pass
    @mopsa.type("wrapper_descriptor")
    def __pow__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __pos__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __radd__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rdivmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __repr__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __rfloordiv__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rmul__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rpow__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rsub__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rtruediv__(self, other): pass
    @mopsa.type("method_descriptor")
    def __sizeof__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __str__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __sub__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __truediv__(self, other): pass
    @mopsa.type("method_descriptor")
    def conjugate(self): pass

class str(object):
    @mopsa.type("wrapper_descriptor")
    def __add__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __contains__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __eq__(self, other): pass
    @mopsa.type("method_descriptor")
    def __format__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __ge__(self, other): pass
     # def __getattribute__(self, attr): pass
    @mopsa.type("wrapper_descriptor")
    def __getitem__(self, i): pass
    @mopsa.type("method_descriptor")
    def __getnewargs__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __gt__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __le__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __len__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __lt__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __mod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __mul__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __ne__(self, other): pass
    def __new__(cls, args): pass
    @mopsa.type("wrapper_descriptor")
    def __repr__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __rmod__(self, other): pass
    @mopsa.type("wrapper_descriptor")
    def __rmul__(self, other): pass
    @mopsa.type("method_descriptor")
    def __sizeof__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __str__(self): pass
    @mopsa.type("method_descriptor")
    def capitalize(self): pass
    @mopsa.type("method_descriptor")
    def casefold(self): pass
    @mopsa.type("method_descriptor")
    def center(self): pass
    @mopsa.type("method_descriptor")
    def count(self): pass
    @mopsa.type("method_descriptor")
    def encode(self): pass
    @mopsa.type("method_descriptor")
    def endswith(self): pass
    @mopsa.type("method_descriptor")
    def expandtabs(self): pass
    @mopsa.type("method_descriptor")
    def find(self): pass
    @mopsa.type("method_descriptor")
    def format(self): pass
    @mopsa.type("method_descriptor")
    def index(self): pass
    @mopsa.type("method_descriptor")
    def isalnum(self): pass
    @mopsa.type("method_descriptor")
    def isalpha(self): pass
    @mopsa.type("method_descriptor")
    def isdecimal(self): pass
    @mopsa.type("method_descriptor")
    def isdigit(self): pass
    @mopsa.type("method_descriptor")
    def isidentifier(self): pass
    @mopsa.type("method_descriptor")
    def islower(self): pass
    @mopsa.type("method_descriptor")
    def isnumeric(self): pass
    @mopsa.type("method_descriptor")
    def isprintable(self): pass
    @mopsa.type("method_descriptor")
    def isspace(self): pass
    @mopsa.type("method_descriptor")
    def istitle(self): pass
    @mopsa.type("method_descriptor")
    def isupper(self): pass
    @mopsa.type("method_descriptor")
    def join(self): pass
    @mopsa.type("method_descriptor")
    def ljust(self): pass
    @mopsa.type("method_descriptor")
    def lower(self): pass
    @mopsa.type("method_descriptor")
    def lstrip(self): pass
    def maketrans(self): pass
    @mopsa.type("method_descriptor")
    def partition(self): pass
    @mopsa.type("method_descriptor")
    def replace(self): pass
    @mopsa.type("method_descriptor")
    def rfind(self): pass
    @mopsa.type("method_descriptor")
    def rindex(self): pass
    @mopsa.type("method_descriptor")
    def rjust(self): pass
    @mopsa.type("method_descriptor")
    def rpartition(self): pass
    @mopsa.type("method_descriptor")
    def rsplit(self): pass
    @mopsa.type("method_descriptor")
    def rstrip(self): pass
    @mopsa.type("method_descriptor")
    def split(self): pass
    @mopsa.type("method_descriptor")
    def splitlines(self): pass
    @mopsa.type("method_descriptor")
    def startswith(self): pass
    @mopsa.type("method_descriptor")
    def strip(self): pass
    @mopsa.type("method_descriptor")
    def swapcase(self): pass
    @mopsa.type("method_descriptor")
    def title(self): pass
    @mopsa.type("method_descriptor")
    def translate(self): pass
    @mopsa.type("method_descriptor")
    def upper(self): pass
    @mopsa.type("method_descriptor")
    def zfill(self): pass

class str_iterator:
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass

class generator(object):
    @mopsa.type("wrapper_descriptor")
    def __del__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __eq__(self): pass
    @mopsa.type("method_descriptor")
    def __format__(self): pass
     # def __getattribute__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __repr__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass
    @mopsa.type("method_descriptor")
    def close(self): pass
    @mopsa.type("method_descriptor")
    def send(self): pass
    @mopsa.type("method_descriptor")
    def throw(self): pass

class list(object):
    @mopsa.type("wrapper_descriptor")
    def __add__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __contains__(self, v): pass
    @mopsa.type("wrapper_descriptor")
    def __delitem__(self, k): pass
    @mopsa.type("wrapper_descriptor")
    def __eq__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __ge__(self, o): pass
     # def __getattribute__(self, a): pass
    @mopsa.type("method_descriptor")
    def __getitem__(self, k): pass
    @mopsa.type("wrapper_descriptor")
    def __gt__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __iadd__(self, o): pass
    # @mopsa.type("wrapper_descriptor")
    # def __imul__(self, o): pass
    def __new__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __init__(self, itr): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __le__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __len__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __lt__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __mul__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __ne__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __repr__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __rmul__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __setitem__(self, k, v): pass
    @mopsa.type("method_descriptor")
    def __sizeof__(self, o): pass
    @mopsa.type("method_descriptor")
    def __reversed__(self): pass
    @mopsa.type("method_descriptor")
    def append(self): pass
    @mopsa.type("method_descriptor")
    def clear(self): pass
    @mopsa.type("method_descriptor")
    def copy(self): pass
    @mopsa.type("method_descriptor")
    def count(self): pass
    @mopsa.type("method_descriptor")
    def extend(self): pass
    @mopsa.type("method_descriptor")
    def index(self): pass
    @mopsa.type("method_descriptor")
    def insert(self): pass
    @mopsa.type("method_descriptor")
    def pop(self): pass
    @mopsa.type("method_descriptor")
    def remove(self): pass
    @mopsa.type("method_descriptor")
    def reverse(self): pass
    @mopsa.type("method_descriptor")
    def sort(self): pass

class list_iterator(object):
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass

class list_reverseiterator(object):
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass

class enumerate(object):
    def __new__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass

class filter(object): pass

class reversed(object):
    def __new__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass

class zip(object):
    def __new__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass

class dict(object):
    def __new__(cls): pass
    @mopsa.type("method_descriptor")
    def __getitem__(self, k): pass
    @mopsa.type("wrapper_descriptor")
    def __setitem__(self, k, v): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __len__(self): pass
    @mopsa.type("method_descriptor")
    def __contains__(self, k): pass
    @mopsa.type("method_descriptor")
    def copy(self): pass
    @mopsa.type("method_descriptor")
    def clear(self): pass
    @mopsa.type("method_descriptor")
    def get(self): pass
    @mopsa.type("method_descriptor")
    def pop(self, key): pass
    @mopsa.type("method_descriptor")
    def popitem(self): pass
    @mopsa.type("method_descriptor")
    def keys(self): pass
    @mopsa.type("method_descriptor")
    def update(self, other): pass
    @mopsa.type("method_descriptor")
    def values(self): pass
    @mopsa.type("method_descriptor")
    def items(self): pass
    @mopsa.type("method_descriptor")
    def setdefault(self): pass

class dict_values(object):
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass

class dict_valueiterator(object):
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass

class dict_keys(object):
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __len__(self): pass

class dict_keyiterator(object):
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass

class dict_items(object):
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __len__(self): pass

class dict_itemiterator(object):
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass

class range(object):
    def __new__(cls, args): pass
    @mopsa.type("wrapper_descriptor")
    def __len__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __contains__(self, v): pass
    @mopsa.type("wrapper_descriptor")
    def __getitem__(self, k): pass
    @mopsa.type("method_descriptor")
    def __reversed__(self): pass

class range_iterator(object):
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass

class set(object):
    def __new__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __init__(self, start, stop): pass
    @mopsa.type("wrapper_descriptor")
    def __len__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("method_descriptor")
    def __contains__(self, v): pass
    @mopsa.type("wrapper_descriptor")
    def __eq__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __ge__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __gt__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __le__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __lt__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __ne__(self, o): pass
    @mopsa.type("method_descriptor")
    def add(self, c): pass
    @mopsa.type("method_descriptor")
    def clear(self): pass
    @mopsa.type("method_descriptor")
    def discard(self, o): pass

class set_iterator(object):
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass

class tuple(object):
    def __new__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __contains__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __getitem__(self, k): pass
    @mopsa.type("wrapper_descriptor")
    def __len__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __eq__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __ge__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __gt__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __le__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __lt__(self, o): pass
    @mopsa.type("wrapper_descriptor")
    def __ne__(self, o): pass

class tuple_iterator(object):
    @mopsa.type("wrapper_descriptor")
    def __next__(self): pass
    @mopsa.type("wrapper_descriptor")
    def __iter__(self): pass

class slice(object):
    def __new__(self, args): pass
    @mopsa.type("method_descriptor")
    def indices(self, length): pass

class NotImplementedType(object): pass

class ellipsis(object): pass

class NoneType(object):
    @mopsa.type("wrapper_descriptor")
    def __bool__(self): pass
     # def __eq__(self, other): pass

@mopsa.unsupported
class bytearray(object): pass

class bytes(object):
    def __new__(self, args): pass
    @mopsa.type("wrapper_descriptor")
    def __getitem__(self, i): pass
    @mopsa.type("wrapper_descriptor")
    def __len__(self, i): pass
    @mopsa.type("method_descriptor")
    def decode(self): pass
    @mopsa.type("method_descriptor")
    def split(self): pass
    @mopsa.type("method_descriptor")
    def strip(self): pass
    @mopsa.type("method_descriptor")
    def replace(self): pass

@mopsa.unsupported
class bytes_iterator(object): pass

class classmethod(object):
    def __new__(self, func): pass
    def __init__(self, func): pass
    def __get__(self): pass

class staticmethod(object):
    def __new__(self, func): pass
    def __init__(self, func): pass
    def __get__(self): pass


class property(object):
    # stubs from the python doc, except for getter/setter/deleter
    @mopsa.type("wrapper_descriptor")
    @mopsa.stub
    def __init__(self, fget=None, fset=None, fdel=None, doc=None):
        if doc is None and fget is not None and hasattr(fget, "__doc__"):
            doc = fget.__doc__
        self.__get = fget
        self.__set = fset
        self.__del = fdel
        self.__doc__ = doc

    @mopsa.type("wrapper_descriptor")
    @mopsa.stub
    def __get__(self, inst, type=None):
        if inst is None:
            return self
        if self.__get is None:
            raise AttributeError("unreadable attribute")
        return self.__get(inst)

    @mopsa.type("wrapper_descriptor")
    @mopsa.stub
    def __set__(self, inst, value):
        if self.__set is None:
            raise AttributeError("can't set attribute")
        return self.__set(inst, value)

    @mopsa.type("wrapper_descriptor")
    @mopsa.stub
    def __delete__(self, inst):
        if self.__del is None:
            raise AttributeError("can't delete attribute")
        return self.__del(inst)

    @mopsa.type("method_descriptor")
    @mopsa.stub
    def getter(self, g):
        self.__get = g
        return self

    @mopsa.type("method_descriptor")
    @mopsa.stub
    def setter(self, s):
        self.__set = s
        return self

    @mopsa.type("method_descriptor")
    @mopsa.stub
    def deleter(self, d):
        self.__del = d
        return self

class callable_iterator:
    @mopsa.stub
    def __init__(self, c, s):
        self.callable = c
        self.sentinel = s

    @mopsa.stub
    def __iter__(self): return self

    @mopsa.stub
    def __next__(self):
        r = self.callable()
        if r == self.sentinel: raise StopIteration
        return r


@mopsa.unsupported
class frozenset(object): pass

@mopsa.unsupported
class map(object): pass

@mopsa.unsupported
class memoryview(object): pass

class super(object):
    def __init__(self, cls): pass
    def __get__(self): pass

class cell(object):
    @mopsa.type("wrapper_descriptor")
    def __init__(self): pass

    @mopsa.type("wrapper_descriptor")
    def __getattribute__(self, attr): pass

class BaseException(object):
    @mopsa.type("wrapper_descriptor")
    def __init__(self, arg): pass

class SystemExit(BaseException): pass

class KeyboardInterrupt(BaseException): pass

class GeneratorExit(BaseException): pass

class Exception(BaseException): pass

class StopIteration(Exception): pass

class StopAsyncIteration(Exception): pass

class ArithmeticError(Exception): pass

class FloatingPointError(ArithmeticError): pass

class OverflowError(ArithmeticError): pass

class ZeroDivisionError(ArithmeticError): pass

class AssertionError(Exception): pass

class AttributeError(Exception): pass

class BufferError(Exception): pass

class EOFError(Exception): pass

class ImportError(Exception): pass

class ModuleNotFoundError(ImportError): pass

class LookupError(Exception): pass

class IndexError(LookupError): pass

class KeyError(LookupError): pass

class MemoryError(Exception): pass

class NameError(Exception): pass

class UnboundLocalError(NameError): pass

class OSError(Exception): pass

class BlockingIOError(OSError): pass

class ChildProcessError(OSError): pass

class ConnectionError(OSError): pass

class BrokenPipeError(ConnectionError): pass

class ConnectionAbortedError(ConnectionError): pass

class ConnectionRefusedError(ConnectionError): pass

class ConnectionResetError(ConnectionError): pass

class FileExistsError(OSError): pass

class FileNotFoundError(OSError): pass

class InterruptedError(OSError): pass

class IsADirectoryError(OSError): pass

class NotADirectoryError(OSError): pass

class PermissionError(OSError): pass

class ProcessLookupError(OSError): pass

class TimeoutError(OSError): pass

class EnvironmentError(OSError): pass

class IOError(OSError): pass

class ReferenceError(Exception): pass

class RuntimeError(Exception): pass

class NotImplementedError(RuntimeError): pass

class RecursionError(RuntimeError): pass

class SyntaxError(Exception): pass

class IndentationError(SyntaxError): pass

class TabError(IndentationError): pass

class SystemError(Exception): pass

class TypeError(Exception): pass

class ValueError(Exception): pass

class UnicodeError(ValueError): pass

class UnicodeDecodeError(UnicodeError): pass

class UnicodeEncodeError(UnicodeError): pass

class UnicodeTranslateError(UnicodeError): pass

class Warning(Exception): pass

class DeprecationWarning(Warning): pass

class PendingDeprecationWarning(Warning): pass

class RuntimeWarning(Warning): pass

class SyntaxWarning(Warning): pass

class UserWarning(Warning): pass

class FutureWarning(Warning): pass

class ImportWarning(Warning): pass

class UnicodeWarning(Warning): pass

class BytesWarning(Warning): pass

class ResourceWarning(Warning): pass


#################
##  Functions  ##
#################

def abs(x): pass
@mopsa.stub
def all(itr):
    for x in itr:
        if not x: return False
    return True
@mopsa.stub
def any(itr):
    for x in itr:
        if x: return True
    return False
def ascii(x): pass
def bin(x): pass
def callable(obj): pass
def chr(i): pass
def compile(src,f,mode): pass
def delattr(obj, attr): pass
def dir(obj): pass
def divmod(a, b): pass
def eval(e): pass
def exec(obj): pass
def format(val, fmt): pass
def getattr(obj, attr): pass
def globals(): pass
def hasattr(obj, attr): pass
def hash(obj): pass
def hex(x): pass
def id(obj): pass
def input(prompt): pass
def isinstance(obj, cls): pass
def issubclass(cls1, cls2): pass
def iter(obj): pass
def len(obj): pass
def locals(): pass
def max(arg1, arg2): pass
def min(arg1, arg2): pass
def next(itr): pass
def oct(x): pass
def open(f): pass
def ord(c): pass
def pow(x, y): pass
def print(obj): pass
def repr(obj): pass
def round(n): pass
def setattr(obj, attr, val): pass
def sorted(itr): pass
def sum(itr): pass
def vars(obj): pass
def __import__(modl): pass
