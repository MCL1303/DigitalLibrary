#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
from time import sleep
from configparser import ConfigParser
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from PyQt4.QtWebKit import QWebView
from sys import argv
from threading import Thread
import requests
from simple_thread import SimpleThread


USER_SCANNER_DEVICE_FILE = "/dev/serial/by-id/usb-1a86_USB2.0-Ser_-if00-port0"


class Sender():
    def __init__(self, parent = None):
        pass

    @SimpleThread
    def send_scanner_data(self, user, book, uuid, webview):
        webview.page().mainFrame().evaluateJavaScript("send_scanner_data({!r}, {!r}, {!r})".format(
        user,
        book,
        uuid,
        ))


def load_config():
    config = ConfigParser()
    config.read("config")
    return config["Demon"]


def scanner_read(device_file):
    with open(device_file) as device:
        return device.readline().strip("\2\3\r\n")


def send_scanner_data(user, book, uuid, webview):
    sender = Sender()
    sender.send_scanner_data(user, book, uuid, webview)


def scan_user(device_file, curent_user, curent_book, uuid, webview):
    curent_book = "curant"
    while True:
        user = scanner_read(device_file)
        if curent_user is not None and curent_book is None:
            curent_user = user
            return
        if curent_user is None and curent_book is None:
            curent_user = user
        else:
            curent_user = user
            thread = Thread(
                target=send_scanner_data,
                args=(
                    curent_book,
                    curent_user,
                    uuid,
                    webview,
                )
            )
            thread.start()
            curent_user = None
            curent_book = None


def scan_book(device_file, curent_user, curent_book, uuid, webview):
    while True:
        book = scanner_read(device_file)
        if curent_book is not None and curent_user is None:
            curent_book = book
            return
        if curent_user is None and curent_book is None:
            curent_book = book
        else:
            curent_book = book
            thread = Thread(
                target=send_scanner_data,
                args=(
                    curent_book,
                    curent_user,
                    uuid,
                    webview,
                )
            )
            thread.start()
            curent_user = None
            curent_book = None


def main():
    config = load_config()

    app = QApplication(argv)
    webview = QWebView()
    webview.load(QUrl(config["url"]))
    webview.setWindowTitle("Библиотека Московского Химического Лицея")
    webview.showFullScreen()

    curent_user, curent_book = None, None
    uuid = requests.get(
        "http://localhost:5000/connect"
    ).json()["uuid"]
    thread_user = Thread(
        target=scan_user,
        args=(
            USER_SCANNER_DEVICE_FILE,
            curent_user,
            curent_book,
            uuid,
            webview,
        )
    )
    thread_user.start()
    # thread_book = Thread(
    #     target=scan_book,
    #     args=(
    #         config.get("Demon", "bookScanner"),
    #         curent_user,
    #         curent_book,
    #         uuid,
    #         webview,
    #     ),
    # )
    # thread_book.start()

    app.exec_()


if __name__ == '__main__':
    main()
