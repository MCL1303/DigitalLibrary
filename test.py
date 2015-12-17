#!/usr/bin/env python3

""" Digital Library — a digital book management system
    Copyright (C) 2015  Yuriy Syrovetskiy <cblp@cblp.su>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""


from ast import literal_eval
from os import listdir
from subprocess import CalledProcessError, check_call, check_output
from sys import stderr


PEP8_OPTIONS = [
    "--show-source",
    "--ignore=" + ','.join([
        'E251',   # unexpected spaces around keyword / parameter equals
        'E402',   # module level import not at top of file
                  # for import selenium.egg
        'W503',   # line break before binary operator
    ]),
]

PYLINT_OPTIONS = [
    "--output-format=colorized",
    "--rcfile=.pylintrc",
    "--reports=no",
]

PYLINT_ISSUED_WARNING = 4
PYLINT_ISSUED_REFACTOR = 8


def get_python_exe_version():
    return literal_eval(
        check_output(
            ['python', '-c', 'import sys; print(tuple(sys.version_info))']
        )
        .decode()
    )


def run(prog, args=None, ignore_exit_code=None):
    if args is None:
        args = []
    try:
        check_call([prog] + args)
    except CalledProcessError as e:
        if ignore_exit_code is not None and ignore_exit_code(e.returncode):
            print(e, file=stderr)
        else:
            raise e from None


def ignore_flags(mask):
    return lambda code: code & ~mask == 0


def main():
    src_files = [f for f in listdir() if f.endswith('.py')]
    src_packages = ["digital_library"]
    srcs = src_files + src_packages

    try:
        python_exe_version = get_python_exe_version()
        if python_exe_version >= (3,):
            pyflakes = 'pyflakes'
            pylint = 'pylint'
            pytest = 'py.test'
        else:
            pyflakes = 'pyflakes3'
            pylint = 'pylint3'
            pytest = 'py.test-3'

        run('pep8', PEP8_OPTIONS + srcs)
        run(pyflakes, srcs)
        run(
            pylint,
            PYLINT_OPTIONS + srcs,
            ignore_exit_code = ignore_flags(
                PYLINT_ISSUED_WARNING | PYLINT_ISSUED_REFACTOR
            ),
        )
        run(pytest)
    except CalledProcessError as e:
        print(e, file=stderr)
        exit(e.returncode)
    else:
        print("OK")


if __name__ == '__main__':
    main()
