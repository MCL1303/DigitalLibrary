#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from configparser import ConfigParser
import errno
import logging
import os
from PyQt4.QtCore import QUrl
from PyQt4.Qt import QApplication, pyqtSignal
from PyQt4.QtWebKit import QWebView
from sys import argv
from time import sleep


def load_config():
    config = ConfigParser()
    config.read("config")
    return config['Terminal']


def opener_nonblock(path, mode):
    return os.open(path, mode | os.O_NONBLOCK)


def readline_nonblock(fileobject) -> 'Optional[str]':
    try:
        return fileobject.readline() or None
    except OSError as err:
        if err.errno == errno.EAGAIN or err.errno == errno.EWOULDBLOCK:
            return None
        else:
            raise


def scanner_read(device_file):
    with open(device_file, opener=opener_nonblock) as device:
        data = readline_nonblock(device)
        if data is None:
            return None
        return data.strip("\2\3\r\n")


def send_scanner_data(user, webview):
    webview.page().mainFrame().evaluateJavaScript(
        "send_scanner_data({!r});".format(user)
    )


class BrowserWindow(QWebView):
    closed = pyqtSignal()

    def __init__(self, url):
        super().__init__()
        self.setWindowTitle("Библиотека Московского Химического Лицея")
        self.load(QUrl(url))

    def closeEvent(self, _event):
        self.closed.emit()


def main():
    global running

    logging.basicConfig(level=logging.DEBUG)

    config = load_config()

    app = QApplication(argv)
    browser = BrowserWindow(config['url_start'])
    browser.show()

    app_state = {'running': True}
    browser.closed.connect(lambda: app_state.__setitem__('running', False))

    while app_state['running']:
        user = scanner_read(config["user_scanner"])
        if user is not None:
            # book = scanner_read(config["book_scanner"])
            # logging.debug('user = %r', book)
            # if book is not None:
            logging.debug('send_scanner_data%r', (user, browser))
            send_scanner_data(user, browser)
        app.processEvents()
        sleep(0.001)


if __name__ == '__main__':
    main()
