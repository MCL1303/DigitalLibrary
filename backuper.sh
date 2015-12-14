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

if [[ "$1" == "h" ]]; then
	rm /var/backups/digital_library/twoHour -rf
	mv /var/backups/digital_library/oneHour /var/backups/digital_library/twoHour
	mongodump -d digital_library -o /var/backups/digital_library/oneHour
fi

if [[ "$1" == "d" ]]; then
	rm /var/backups/digital_library/twoDay -rf
	mv /var/backups/digital_library/oneDay /var/backups/digital_library/twoDay
	mongodump -d digital_library -o /var/backups/digital_library/oneDay
fi

if [[ "$1" == "w" ]]; then
	rm /var/backups/digital_library/twoWeek -rf
	mv /var/backups/digital_library/oneWeek /var/backups/digital_library/twoWeek
	mongodump -d digital_library -o /var/backups/digital_library/oneWeek
fi
