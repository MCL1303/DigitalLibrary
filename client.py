#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import wx
import wx.html2
import requests
from threading import Thread
import ConfigParser


def load_config():
    config = ConfigParser.ConfigParser()
    config.read("config")
    return config


# pylint: disable=too-many-public-methods
class MyBrowser(wx.Dialog):
    def __init__(self, *args, **kwds):
        wx.Dialog.__init__(self, *args, **kwds)
        sizer = wx.BoxSizer(wx.VERTICAL)
        self.browser = wx.html2.WebView.New(self)
        sizer.Add(self.browser, 1, wx.EXPAND, 10)
        self.SetSizer(sizer)
        self.SetSize(wx.GetDisplaySize())


def scanner_read(device_file):
    with open(device_file) as device:
        return device.readline().strip("\2\3\r\n")


def send_scanner_data(user, book, uuid, dialog):
    dialog.browser.RunScript("send_scanner_data({!r}, {!r}, {!r})".format(
        user,
        book,
        uuid
        ))


def scan_user(device_file, uuid, dialog, curent_user, curent_book):
    user = scanner_read(device_file)
    if curent_user is not None and curent_book is None:
        curent_user = user
        return
    if curent_user is None and curent_book is None:
        curent_user = user
    else:
        curent_user = user
        send_scanner_data(curent_user, curent_book, uuid, dialog)
        curent_user = None
        curent_book = None


def scan_book(device_file, uuid, dialog, curent_user, curent_book):
    book = scanner_read(device_file)
    if curent_book is not None and curent_user is None:
        curent_book = book
        return
    if curent_user is None and curent_book is None:
        curent_book = book
    else:
        curent_book = book
        send_scanner_data(curent_user, curent_book, uuid, dialog)
        curent_user = None
        curent_book = None


def main():
    config = load_config()
    curent_user, curent_book = None, None
    app = wx.App()
    dialog = MyBrowser(None, -1)
    uuid = requests.get(
        "http://localhost:5000/connect"
    ).json()["uuid"]
    thread_user = Thread(
        target=scan_user("path", uuid, dialog, curent_user, curent_book),
        args=config.get("Demon", "userScanner")
    )
    thread_book = Thread(
        target=scan_book("path", uuid, dialog, curent_user, curent_book),
        args=config.get("Demon", "bookScanner")
    )
    thread_book.start()
    thread_user.start()
    dialog.browser.LoadURL(config.get("Demon", "url"))
    dialog.SetTitle("Библиотека Московского Химического Лицея")
    dialog.Show()
    app.MainLoop()

if __name__ == '__main__':
    main()
