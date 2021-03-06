#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Digital Library — a digital book management system
# Copyright (C) 2015  Igor Tarakanov <igortarakanov144999usa@gmail.com>,
#                     Yuriy Syrovetskiy <cblp@cblp.su>
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


import sys
from threading import Thread
import configparser

sys.path.append("./selenium.egg")

from selenium.webdriver import Firefox


def load_config():
    config = configparser.ConfigParser()
    config.read('config')
    return config['Terminal']


def user_scanner(driver, package):
    config = load_config()
    scanner = open(config["user_scanner"])
    while True:
        new_user = scanner.read().strip("\2\3\r\n")
        driver.execute_script("user(" + new_user + ")")


def book_scanner(driver, package):
    config = load_config()
    while True:
        scanner = open(config["book_scanner"], "rb")
        i = 0
        barcode = ""
        while True:
            i += 1
            scanner.read(12)
            number = int.from_bytes(scanner.read(1), byteorder='big') - 29
            scanner.read(3)
            if i % 2 == 0:
                continue
            if number < 0:
                continue
            if number == 11:
                break
            print(number % 10)
            barcode += str(number % 10)
        driver.execute_script("barcode(" + barcode + ")")


def main():
    driver = Firefox()
    driver.get("http://yandex.ru/")
    package = {
        "user": "",
        "book": "",
    }
    user = Thread(target=user_scanner, args=(driver, package))
    book = Thread(target=book_scanner, args=(driver, package))
    user.start()
    book.start()


if __name__ == '__main__':
    main()
