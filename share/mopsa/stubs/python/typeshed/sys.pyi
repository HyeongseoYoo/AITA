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

from typing import List, NoReturn, TextIO

# maxsize = mopsa.random_int()
path: List[str]
argv: List[str]
version_info = (3, 7)

maxsize = 9223372036854775807

base_prefix: str

def exit(arg: object = ...) -> NoReturn:
    raise SystemExit()

stdin: TextIO
stdout: TextIO
stderr: TextIO

class int_info:
    bits_per_digit = 30
    sizeof_digit = 4


class float_info:
    epsilon = 2.220446049250313e-16
    dig = 15
    mant_dig = 53
    max = 1.7976931348623157e+308
    max_exp = 1024
    max_10_exp = 308
    min = 2.2250738585072014e-308
    min_exp = -1021
    min_10_exp = -307
    radix = 2
    rounds = 1
