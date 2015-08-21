#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from configparser import ConfigParser
from PyQt5.Qt import QApplication, QUrl
from PyQt5.QtWebKitWidgets import QWebView
from sys import argv
from threading import Thread


USER_SCANNER_DEVICE_FILE = "/dev/serial/by-id/usb-1a86_USB2.0-Ser_-if00-port0"


def load_config():
    config = ConfigParser()
    config.read("config")
    return config


def scanner_read(device_file):
    with open(device_file) as device:
        return device.readline().strip("\2\3\r\n")


def send_scanner_data(user, book, dialog):
    dialog.browser.RunScript("send_scanner_data({!r}, {!r})".format(
        user,
        book
        ))


def scan_user(device_file, curent_user, curent_book):
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
            send_scanner_data(curent_user, curent_book, dialog)
            curent_user = None
            curent_book = None


def scan_book(device_file, curent_user, curent_book):
    while True:
        book = scanner_read(device_file)
        if curent_book is not None and curent_user is None:
            curent_book = book
            return
        if curent_user is None and curent_book is None:
            curent_book = book
        else:
            curent_book = book
            send_scanner_data(curent_user, curent_book, dialog)
            curent_user = None
            curent_book = None


def main():
    config = load_config()

    app = QApplication(argv)
    webview = QWebView()
    webview.load(QUrl(config.get("Demon", "url")))
    webview.setWindowTitle("Библиотека Московского Химического Лицея")
    webview.show()

    curent_user, curent_book = None, None
    # uuid = requests.get(
    #     "http://localhost:5000/connect"
    # ).json()["uuid"]
    thread_user = Thread(
        target=scan_user,
        args=(
            USER_SCANNER_DEVICE_FILE,
            curent_user,
            curent_book,
        )
    )
    thread_user.start()
    # thread_book = Thread(
    #     target=scan_book,
    #     args=(
    #         config.get("Demon", "bookScanner"),
    #         curent_user,
    #         curent_book,
    #     ),
    # )
    # thread_book.start()

    app.exec_()


if __name__ == '__main__':
    main()
