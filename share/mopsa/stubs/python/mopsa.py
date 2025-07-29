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

# Assertions
def assert_equal(x, y): pass
def assert_not_equal(x, y): pass
def massert(cond): pass
def assert_exists(cond): pass
def assert_safe(cond): pass
def assert_unsafe(cond): pass
def assert_exception(exn): pass
def assert_exception_exists(exn): pass
def ignore_exception(exn): pass

# Ranges
def random_int(a, b): pass
def random_float(a, b): pass
def random_bool(): pass
def random_string(): pass

# Decorators
def stub(f): pass
def unsupported(f): pass
def builtin(f, name): pass
def typedas(name): pass # default is builtin_function_or_method in stdlib.py

# Assertions used in the type analysis
def assert_list_of(l, ty): pass
def assert_set_of(l, ty): pass
def assert_dict_of(l, ty_k, ty_v): pass
def assert_tuple_of(t, ty): pass
