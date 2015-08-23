#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests
import sys
from time import sleep
from configparser import ConfigParser
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from PyQt4.QtWebKit import QWebView
from sys import argv
from threading import Thread
from digital_library.simple_thread import SimpleThread


USER_SCANNER_DEVICE_FILE = "/dev/serial/by-id/usb-1a86_USB2.0-Ser_-if00-port0"


def load_config():
    config = ConfigParser()
    config.read("config")
    return config["Demon"]


def scanner_read(device_file):
    with open(device_file) as device:
        return device.readline().strip("\2\3\r\n")


class Sender():
    def __init__(self):
        pass

    @SimpleThread
    def send(self, user, book, uuid, webview):
        print(user)
        webview.page().mainFrame().evaluateJavaScript('window.alert("ok")')
        webview.page().mainFrame().evaluateJavaScript("send_scanner_data({!r}, {!r}, {!r})".format(
            user,
            book,
            uuid,
        ))


def send_scanner_data(user, book, uuid, webview):
    print(user)
    sender = Sender()
    sender.send(user, book, uuid, webview)
    print("sended")


def scan_user(device_file, curent_user, curent_book, uuid, webview):
    while True:
        curent_book = "curant"
        user = scanner_read(device_file)
        print("scanned " + user)
        if curent_user is not None and curent_book is None:
            curent_user = user
            continue
        if curent_user is None and curent_book is None:
            curent_user = user
        else:
            print("sending")
            curent_user = user
            send_scanner_data(curent_user, curent_book, uuid, webview)
            curent_user = None
            curent_book = None


def scan_book(device_file, curent_user, curent_book, uuid, webview):
    while True:
        book = scanner_read(device_file)
        if curent_book is not None and curent_user is None:
            curent_book = book
            continue
        if curent_user is None and curent_book is None:
            curent_book = book
        else:
            curent_book = book
            send_scanner_data(curent_user, curent_book, uuid, webview)
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
