#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import configparser
from datetime import datetime
from flask import Flask, jsonify, request
import flask
import logging
import pymongo
import uuid


app = Flask('digital_library')


def load_config():
    config = configparser.ConfigParser()
    config.read('config')
    return config['Server']


# pylint: disable=too-few-public-methods
class Collection():
    pass


class Terminals(Collection):
    def __init__(self):
        super().__init__()
        self.db = pymongo.MongoClient().digital_library

    def add(self, client_ip, terminal_uuid):
        self.db.terminals.insert({"ip": client_ip, "uuid": str(terminal_uuid)})

    def get(self, client_ip):
        return self.db.terminals.find_one({'ip': client_ip})


class Hands(Collection):
    def __init__(self):
        super().__init__()
        self.db = pymongo.MongoClient().digital_library

    def add(self, user, book):
        now = datetime.utcnow()
        self.db.hands.insert({
            "user": user,
            "book": book,
            "datetime": now,
        })

    def get(self, user, book):
        return self.db.hands.find_one({'user': user, 'book': book})

    def exists(self, user, book):
        return self.get(user, book) is not None

    def delete(self, user, book):
        self.db.hands.remove({"user": user, "book": book})


# pylint: disable=too-few-public-methods
class Handlog(Collection):
    def __init__(self):
        super().__init__()
        self.db = pymongo.MongoClient().digital_library

    def add(self, user, book, event):
        assert event in {'take', 'return'}
        now = datetime.utcnow()
        self.db.handlog.insert({
            "user": user,
            "book": book,
            "datetime": now,
            "event": event,
        })


# pylint: disable=too-few-public-methods
class Database:
    def __init__(self):
        super().__init__()
        self.terminals = Terminals()
        self.hands = Hands()
        self.handlog = Handlog()


def render_template(template_name, **context):
    test_books = [
        {
            "title": "Что такое математика?",
            "author": "Р. Курант, Г. Роббинс",
            "id": "curant",
        },
        {
            "title": "Сборник задач по алгебре",
            "author": "М.Л. Галицкий, А.М. Гольдман, Л.И. Звавич",
            "id": "gal",
        },
        {
            "title": "Алгоритмы: построение и анализ",
            "author": "К. Штайн, Р. Линн Ривест, Т. Кормен, Ч. Эрик Лейзерсон",
            "id": "cormen",
        },
        {
            "title": "Совершенный код",
            "author": "С. Макконнелл",
            "id": "codecompl",
        },
    ]
    test_context = {
        'user': {"name": "Иван Иванов", },
        'device': "terminal",
        'books': test_books,
        'booksLen': len(test_books),
        'recomendedBooks': test_books,
        'recomendedBooksLen': len(test_books),
    }
    return flask.render_template(
        template_name + '.html',
        **dict(context, template_name=template_name, **test_context)
    )


@app.route("/login")
def login():
    return render_template("login")


@app.route("/reg")
def reg():
    return render_template("registration")


@app.route("/")
def home():
    return render_template("home")


@app.route("/handed")
def handed():
    return render_template("handed")


@app.route("/books")
def books():
    return render_template("books")


@app.route("/operations")
def operations():
    return render_template("operations")


@app.route('/connect')
def get_current_user():
    db = Database()
    client_ip = request.remote_addr
    terminal = db.terminals.get(client_ip)
    if terminal is not None:
        return jsonify(terminal_uuid=terminal['uuid'])
    else:
        terminal_uuid = uuid.uuid4()
        db.terminals.add(client_ip, terminal_uuid)
        return jsonify(terminal_uuid=terminal_uuid)


@app.route('/api/book/action', methods=['POST'])
def api_book_action():
    form = request.form
    db = Database()
    user, book = form["user"], form["book"]
    if db.hands.exists(user, book):
        db.hands.delete(user, book)
        db.handlog.add(user, book, "return")
        action = "return"
    else:
        db.hands.add(user, book)
        db.handlog.add(user, book, "take")
        action = "take"
    return jsonify(action=action, book=book)


def main():
    logging.basicConfig(level=logging.DEBUG)
    config = load_config()
    app.run(host=config["host"], debug=True)


if __name__ == '__main__':
    main()
