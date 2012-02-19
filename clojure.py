#!/usr/bin/env python

try:
    import readline
except ImportError:
    pass

import sys

from py.clojure.lang.lispreader import read
from py.clojure.lang.fileseq import StringReader
from py.clojure.lang.globals import currentCompiler
import py.clojure.lang.rt as RT
from py.clojure.lang.compiler import Compiler
from py.clojure.lang.symbol import Symbol

VERSION = "0.0.0"

def requireClj(filename, stopafter = None):
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
    requireClj("./clj/clojure/core.clj")

    RT.init()
    comp = Compiler()
    currentCompiler.set(comp)
    comp.setNS(Symbol.intern("user"))

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
                import traceback; traceback.print_exc()
    else:
        for x in sys.argv[1:]:
            requireClj(x)


def unbalanced(s):
    return (s.count('(') != s.count(')')
            or s.count('[') != s.count(']')
            or s.count('{') != s.count('}'))



if __name__ == "__main__":
    main()
