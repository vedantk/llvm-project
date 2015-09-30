"""Test that importing modules in C works as expected."""

import os, time
import unittest2
import lldb
import platform
import lldbutil

from distutils.version import StrictVersion

from lldbtest import *

class CModulesTestCase(TestBase):

    mydir = TestBase.compute_mydir(__file__)

    @skipIfFreeBSD
    @expectedFailureDarwin('http://llvm.org/pr24302')
    @expectedFailureLinux('http://llvm.org/pr23456') # 'fopen' has unknown return type
    @expectedFailureWindows("llvm.org/pr24489: Name lookup not working correctly on Windows")
    def test_expr(self):
        if platform.system() == "Darwin" and platform.release() < StrictVersion('12.0.0'):
            self.skipTest()

        self.build()
        exe = os.path.join(os.getcwd(), "a.out")
        self.runCmd("file " + exe, CURRENT_EXECUTABLE_SET)

        # Break inside the foo function which takes a bar_ptr argument.
        lldbutil.run_break_set_by_file_and_line (self, "main.c", self.line, num_expected_locations=1, loc_exact=True)

        self.runCmd("run", RUN_SUCCEEDED)

        # The stop reason of the thread should be breakpoint.
        self.expect("thread list", STOPPED_DUE_TO_BREAKPOINT,
            substrs = ['stopped',
                       'stop reason = breakpoint'])

        # The breakpoint should have a hit count of 1.
        self.expect("breakpoint list -f", BREAKPOINT_HIT_ONCE,
            substrs = [' resolved, hit count = 1'])

        self.expect("expr @import Darwin; 3", VARIABLES_DISPLAYED_CORRECTLY,
            substrs = ["int", "3"])

        self.expect("expr *fopen(\"/dev/zero\", \"w\")", VARIABLES_DISPLAYED_CORRECTLY,
            substrs = ["FILE", "_close", "__sclose"])

        self.expect("expr *myFile", VARIABLES_DISPLAYED_CORRECTLY,
            substrs = ["a", "5", "b", "9"])

        self.expect("expr MIN((uint64_t)2, (uint64_t)3)", VARIABLES_DISPLAYED_CORRECTLY,
            substrs = ["uint64_t", "2"])

        self.expect("expr stdin", VARIABLES_DISPLAYED_CORRECTLY,
            substrs = ["(FILE *)", "0x"])

    def setUp(self):
        # Call super's setUp().
        TestBase.setUp(self)
        # Find the line number to break inside main().
        self.line = line_number('main.c', '// Set breakpoint 0 here.')
            
if __name__ == '__main__':
    import atexit
    lldb.SBDebugger.Initialize()
    atexit.register(lambda: lldb.SBDebugger.Terminate())
    unittest2.main()
