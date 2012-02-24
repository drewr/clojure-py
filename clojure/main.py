#!/usr/bin/env python

import sys
import os.path
import traceback


try:
    import readline
except ImportError:
    pass
else:
    import os
    import atexit
    histfile = os.path.join(os.path.expanduser("~"), ".clojurepyhist")
    try:
        readline.read_history_file(histfile)
    except IOError:
        # Pass here as there isn't any history file, so one will be
        # written by atexit
        pass
    atexit.register(readline.write_history_file, histfile)

from clojure.lang.lispreader import read
from clojure.lang.fileseq import StringReader
from clojure.lang.globals import currentCompiler
import clojure.lang.rt as RT
from clojure.lang.compiler import Compiler
from clojure.lang.symbol import Symbol, symbol

VERSION = "0.0.0"


def requireClj(filename, stopafter=None):
    with open(filename) as fl:
        r = StringReader(fl.read())

    RT.init()
    comp = Compiler()
    comp.setFile(filename)
    currentCompiler.set(comp)

    try:
        while True:
            s = read(r, True, None, True)
            try:
                res = comp.compile(s)
                comp.executeCode(res)
                if stopafter is not None:
                    if hasattr(comp.getNS(), stopafter):
                        break
            except IOError as exp:
                print s
                raise exp

            while True:
                ch = r.read()

                if ch == "":
                    raise IOError()

                if ch not in [" ", "\t", "\n", "\r"]:
                    r.back()
                    break
    except IOError as e:
        pass


def main():
    requireClj(os.path.dirname(__file__) + "/core.clj")

    RT.init()
    comp = Compiler()
    currentCompiler.set(comp)
    comp.setNS(symbol("user"))

    if not sys.argv[1:]:
        while True:
            try:
                line = raw_input(comp.getNS().__name__ + "=> ")
            except EOFError:
                break

            if not line:
                continue

            while unbalanced(line):
                try:
                    line += raw_input('.' * len(comp.getNS().__name__) + '.. ')
                except EOFError:
                    break

            # Propogate break from above loop.
            if unbalanced(line):
                break

            r = StringReader(line)
            s = read(r, True, None, True)

            try:
                res = comp.compile(s)
                print comp.executeCode(res)
            except Exception:
                traceback.print_exc()
    else:
        for x in sys.argv[1:]:
            requireClj(x)


def unbalanced(s):
    return (s.count('(') != s.count(')')
            or s.count('[') != s.count(']')
            or s.count('{') != s.count('}'))


if __name__ == "__main__":
    main()
