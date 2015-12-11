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
import sys


PEP8_OPTIONS = [
    "--show-source",
    "--ignore=E251,W503",
    # E251: unexpected spaces around keyword / parameter equals
    # W503: line break before binary operator
]


PYLINT_OPTIONS = [
    "--output-format=colorized",
    "--rcfile=.pylintrc",
    "--reports=no",
]


def get_python_exe_version():
    return literal_eval(
        check_output(
            ['python', '-c', 'import sys; print(tuple(sys.version_info))']
        )
        .decode()
    )


def main():
    python_files = [f for f in listdir() if f.endswith('.py')]
    python_packages = ["digital_library"]
    print('sys.path =', sys.path)

    try:
        python_exe_version = get_python_exe_version()
        if python_exe_version >= (3,):
            pyflakes_cmd = "pyflakes"
            pylint_cmd = "pylint"
        else:
            pyflakes_cmd = "pyflakes3"
            pylint_cmd = "pylint3"

        cmds = [
            ["pep8"] + PEP8_OPTIONS + ["."],
            [pyflakes_cmd] + python_files + python_packages,
            [pylint_cmd] + PYLINT_OPTIONS + python_files + python_packages,
            ["py.test-3"],
        ]
        for cmd in cmds:
            check_call(cmd)
    except CalledProcessError as e:
        print(e, file=stderr)
        return 1
    else:
        print("OK")
        return 0


if __name__ == '__main__':
    sys.exit(main())
