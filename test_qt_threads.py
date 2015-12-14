from PySide.QtCore import QObject, Signal
from PySide.QtGui import QApplication
from PySide.QtWebKit import QWebView
from threading import Thread
from time import sleep, time


class Browser(QObject):
    input = Signal((float,))

    def __init__(self):
        super().__init__()
        self.app = QApplication([])
        self.webview = QWebView()
        self.input.connect(self.receive)

    def run(self):
        self.webview.show()
        self.app.exec_()

    def receive(self, cur_time):
        self.webview.page().mainFrame().evaluateJavaScript(
            'document.write({})'.format(cur_time)
        )


def scanner_worker(browser):
    while True:
        print('scanner is working, browser is', browser)
        browser.input.emit(time())
        sleep(2)


def main():
    browser = Browser()

    t2 = Thread(target=scanner_worker, args=(browser,), daemon=True)
    t2.start()

    browser.run()


if __name__ == '__main__':
    main()
