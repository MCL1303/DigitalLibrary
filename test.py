#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
from time import sleep
from PyQt4.QtCore import *
from PyQt4.QtGui import *

from simple_thread import SimpleThread


class Sender(QLabel):
    def __init__(self, parent = None):
        pass

    @SimpleThread
    def send_scanner_data(self, user, book, uuid, webview):
        webview.page().mainFrame().evaluateJavaScript("send_scanner_data({!r}, {!r}, {!r})".format(
        user,
        book,
        uuid,
        ))


if __name__ == "__main__":
    app = QApplication(sys.argv)
    foo = Foo()
    foo.show()
    foo.bar('From thread', thr_start = True)
    app.exec_()