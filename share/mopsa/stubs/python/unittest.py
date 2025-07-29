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

# TestCase is NOT a mopsa.builtin, since assertRaises is defined here
class TestCase:
    @mopsa.builtin("unittest.TestCase.assertTrue")
    def assertTrue(self, cond): pass

    @mopsa.builtin("unittest.TestCase.assertFalse")
    def assertFalse(self, cond): pass

    @mopsa.builtin("unittest.TestCase.assertEqual")
    def assertEqual(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertNotEqual")
    def assertNotEqual(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertGreater")
    def assertGreater(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertGreaterEqual")
    def assertGreaterEqual(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertLess")
    def assertLess(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertLessEqual")
    def assertLessEqual(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertIs")
    def assertIs(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertIsNot")
    def assertIsNot(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertIn")
    def assertIn(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertNotIn")
    def assertNotIn(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertIsInstance")
    def assertIsInstance(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertNotIsInstance")
    def assertNotIsInstance(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertRaises")
    def assertRaises(self, exn): pass

    def assertRaisesRegex(self, exn, regex):
        # FIXME: and the message?
        return ExceptionContext(exn, regex)

    @mopsa.builtin("unittest.TestCase.fail")
    def fail(self, msg): pass


#  ExceptionContext is NOT a mopsa.builtin, since __init__ and __enter__ are defined here
class ExceptionContext(object):
    def __init__(self, exn, regex=None):
        self.expected = exn
        self.regex = regex

    def __enter__(self):
        return self

    @mopsa.builtin("unittest.ExceptionContext.__exit__")
    def __exit__(self, type, exn, trace): pass

@mopsa.builtin("unittest.main")
def main(): pass

@mopsa.builtin("unittest.skipUnless")
def skipUnless(): pass


class SkipTest(Exception): pass

@mopsa.stub
def skip(f):
    def bla(a):
        raise SkipTest
    return bla
