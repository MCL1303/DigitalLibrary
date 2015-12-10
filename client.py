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


from selenium.webdriver.common.keys import Keys
from selenium.webdriver import Firefox
import time
from threading import Thread
import configparser


def load_config():
    config = configparser.ConfigParser()
    config.read('config')
    return config['Client']


def send_scanner_data(package, driver):
    driver.execute_script(
        "send('" + package["user"] + "', '"+ package["book"] + "')"
    )


def new_package(data, dtype, driver, package):
    if package == {"user": "", "book": ""}:
        package[dtype] = data
        return
    else:
        if package[dtype] == "":
            package[dtype] = data
            send_scanner_data(package, driver)
            package = {"user": "", "book": ""}
        else:
            package[dtype] = data


def user_scanner(driver, package):
    config = load_config()
    scanner = open(config["user_scanner"])
    while True:
        new_user = scanner.read().strip("\2\3\r\n")
        driver.execute_script("new_user(" + new_user + ")")
        new_package(
            new_user,
            "user",
            driver,
            package,
        )


def book_scanner(driver, package):
    config = load_config()
    while True:
        scanner = open(config["book_scanner"], "rb")
        i = 0
        bar_code = ""
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
            bar_code += str(number % 10)
        new_package(
            bar_code,
            "book",
            driver,
            package,
        )


def main():
    config = load_config()
    driver = Firefox()
    # driver.get(config["operations_url"])
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
