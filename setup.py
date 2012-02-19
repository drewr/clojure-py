#!/usr/bin/env python

from os.path import exists
try:
    # Use setup() from setuptools(/distribute) if available
    from setuptools import setup
except ImportError:
    from distutils.core import setup

from clojure import VERSION

setup(name='clojure_py',
      version=VERSION,
      author='Timothy Baldridge',
      author_email='tbaldridge@gmail.com',
      packages=['py'],
      scripts=[],
      url='https://github.com/halgari/clojure-py',
      license='',
      description='Clojure implemented on top of Python',
      long_description=open('README.md').read() if exists("README.md") else "",
      entry_points=dict(console_scripts=['clojurepy=clojure:main']),
      install_requires=[],
)
