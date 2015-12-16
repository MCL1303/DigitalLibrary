# Digital Library — a digital book management system
# Copyright (C) 2015  Igor Tarakanov <igortarakanov144999usa@gmail.com>,
#                     Yuriy Syrovetskiy <cblp@cblp.su>
#                     Pavel Fedorov <pfedorovs18@gmail.com>
#                     Danila Starostin <starostindanila@yandex.ru>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from enum import Enum


class Action(Enum):
    # pylint: disable=no-init
    Take = 1
    Return = 2
    Fail = 3


class ClientType(Enum):
    # pylint: disable=no-init
    User = 1
    Terminal = 2


class AccessLevel(Enum):
    # pylint: disable=no-init
    Student = 1
    Librarian = 2
