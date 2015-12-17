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


import configparser
import errno
import os
from PySide.QtCore import QObject, Qt, Signal
from PySide.QtGui import QApplication
from PySide.QtWebKit import QWebView
from threading import current_thread, main_thread, Thread
from time import sleep


def current_thread_is_main():
    return current_thread() is main_thread()


class Browser(QObject):
    # pylint: disable=too-many-public-methods

    __execute_script_called = Signal(str)

    def __init__(self):
        super().__init__()

        self.app = QApplication([])

        self.webview = QWebView()
        self.webview.setAttribute(Qt.WA_DeleteOnClose)
        self.webview.destroyed.connect(self.app.quit)

        self.__execute_script_called.connect(self.__execute_script)

    def __execute_script(self, javascript_code: str):
        assert current_thread_is_main()
        self.webview.page().mainFrame().evaluateJavaScript(javascript_code)

    def execute_script(self, javascript_code: str):
        if current_thread_is_main():
            self.__execute_script(javascript_code)
        else:
            self.__execute_script_called.emit(javascript_code)

    def run(self, url):
        assert current_thread_is_main()
        self.webview.show()
        self.webview.load(url)
        self.app.exec_()


def opener_nonblock(path, mode):
    return os.open(path, mode | os.O_NONBLOCK)


def readline_nonblock(fileobject) -> 'Optional[str]':
    try:
        return fileobject.readline() or None
    except OSError as err:
        if err.errno == errno.EAGAIN or err.errno == errno.EWOULDBLOCK:
            return None


def user_scanner(config, browser):
    scanner = open(config["user_scanner"], opener=opener_nonblock)
    while True:
        data = readline_nonblock(scanner)
        if data is not None:
            new_user = data.strip("\2\3\r\n")
            browser.execute_script("user(" + new_user + ")")
        else:
            sleep(0.1)


def book_scanner(config, browser):
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
        browser.execute_script("barcode(" + barcode + ")")


def main():
    def load_config():
        config = configparser.ConfigParser()
        config.read('config')
        return config['Terminal']

    config = load_config()

    browser = Browser()

    user = Thread(target=user_scanner, args=(config, browser), daemon=True)
    book = Thread(target=book_scanner, args=(config, browser), daemon=True)
    user.start()
    book.start()

    browser.run(config['operations_url'])


if __name__ == '__main__':
    main()
