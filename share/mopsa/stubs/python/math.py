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

pi = 3.141592653589793
e  = 2.718281828459045
tau = 6.283185307179586
inf = float('inf')
nan = float('nan')

def sin(x):
    if isinstance(x, int) or isinstance(x, float):
        return mopsa.random_float(-1, 1)
    else:
        raise TypeError
def cos(x):
    if isinstance(x, int) or isinstance(x, float):
        return mopsa.random_float(-1, 1)
    else:
        raise TypeError


def acos(x): return mopsa.random_float(0, pi) # -1 <= x <= 1
def asin(x): return mopsa.random_float(-pi/2, pi/2) # -1 <= x <= 1

def cosh(x): return mopsa.random_float() # actually, [1; +\infty)
def sinh(x): return mopsa.random_float()

def acosh(x): return mopsa.random_float() # [0; +infty), but x >= 1
def asinh(x): return mopsa.random_float()

def tanh(x): return mopsa.random_float(-1, 1)
def atanh(x): return mopsa.random_float() # -1 < x < 1

def tan(x): return mopsa.random_float() # x != k*pi+pi/2
def atan(x): return mopsa.random_float(-pi/2, pi/2)
def atan2(y, x): return atan(y/x)

def degrees(x): return x * 180 / pi
def radians(x): return x * pi / 180

@mopsa.builtin("math.ceil")
def ceil(x): pass

@mopsa.builtin("math.copysign")
def copysign(x, y): pass

@mopsa.builtin("math.fabs")
def fabs(x): pass

@mopsa.builtin("math.factorial")
def factorial(x): pass

@mopsa.builtin("math.floor")
def floor(x): pass

@mopsa.builtin("math.erf")
def erf(x): pass

@mopsa.builtin("math.erfc")
def erfc(x): pass

@mopsa.builtin("math.exp")
def exp(x): pass

@mopsa.builtin("math.expm1")
def expm1(x): pass

@mopsa.builtin("math.fmod")
def fmod(x, n): pass

@mopsa.builtin("math.frexp")
def frexp(x): pass

@mopsa.builtin("math.fsum")
def fsum(x): pass

@mopsa.builtin("math.gamma")
def gamma(x): pass

@mopsa.builtin("math.gcd")
def gcd(x, y): pass

@mopsa.builtin("math.hypot")
def hypot(x, y): pass

@mopsa.builtin("math.isclose")
def isclose(a, b): pass

@mopsa.builtin("math.isfinite")
def isfinite(x): pass

@mopsa.builtin("math.isinf")
def isinf(x): pass

@mopsa.builtin("math.isnan")
def isnan(x): pass

@mopsa.builtin("math.ldexp")
def ldexp(x, i): pass

@mopsa.builtin("math.lgamma")
def lgamma(x): pass

@mopsa.builtin("math.log")
def log(x): pass

@mopsa.builtin("math.log10")
def log10(x): pass

@mopsa.builtin("math.log1p")
def log1p(x): pass

@mopsa.builtin("math.log2")
def log2(x): pass

@mopsa.builtin("math.modf")
def modf(x): pass

@mopsa.builtin("math.pow")
def pow(x, y): pass

@mopsa.builtin("math.trunc")
def trunc(x): pass

@mopsa.builtin("math.sqrt")
def sqrt(x): pass
