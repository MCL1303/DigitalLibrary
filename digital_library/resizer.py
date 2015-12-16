# Digital Library — a digital book management system
# Copyright (C) 2015  Igor Tarakanov <igortarakanov144999usa@gmail.com>,
#                     Yuriy Syrovetskiy <cblp@cblp.su>
#                     Pavel Fedorov <pfedorovs18@gmail.com>
#                     Danila Starostin <starostindanila@yandex.ru>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


import PIL.Image


def Resize(path, folder, name, ext):
    img = PIL.Image.open(path)
    width = int(img.size[0] * (480 / img.size[1]))
    img = img.resize((width, 480), PIL.Image.ANTIALIAS)
    img.save(
        "static/images/{folder}/large-covers/{name}.{ext}"
        .format(folder=folder, name=name, ext=ext)
    )
    width = int(img.size[0] * (320 / img.size[1]))
    img = img.resize((width, 320), PIL.Image.ANTIALIAS)
    img.save(
        "static/images/{folder}/small-covers/{name}.{ext}"
        .format(folder=folder, name=name, ext=ext)
    )
