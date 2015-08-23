#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from digital_library.database import DigitalLibraryDatabase
from digital_library.types import Action

import configparser
from flask import Flask, jsonify, request
import flask
import logging
import uuid


app = Flask('DigitalLibraryApplication')


def load_config():
    config = configparser.ConfigParser()
    config.read('config')
    return config['Server']


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
        'user': {
            "name": "Иван Иванов",
            "priority": "student"
        },
        'device': "terminall",
        'books': test_books,
        'booksLen': len(test_books),
        'recomendedBooks': test_books,
        'recomendedBooksLen': len(test_books),
        'page': template_name,
    }
    return flask.render_template(
        template_name + '.html',
        **dict(context, template_name=template_name, **test_context)
    )


@app.route("/login")
def login():
    return render_template("login")


@app.route("/registration")
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
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    terminal = db.terminals.get(client_ip)
    if terminal is not None:
        return jsonify(uuid=terminal['uuid'])
    else:
        terminal_uuid = uuid.uuid4()
        db.terminals.add(client_ip, terminal_uuid)
        return jsonify(uuid=terminal_uuid)


@app.route('/api/book/action', methods=['POST'])
def api_book_action():
    form = request.form
    db = DigitalLibraryDatabase()
    user, book = form["user"], form["book"]
    if db.hands.exists(user, book):
        db.hands.delete(user, book, uuid)
        action = Action.Return
    else:
        db.hands.add(user, book, uuid)
        action = Action.Take
    db.handlog.log(action, user, book, uuid)
    return jsonify(action=action.name, book=book)


def main():
    logging.basicConfig(level=logging.DEBUG)
    config = load_config()
    app.run(host=config["host"], debug=True)


if __name__ == '__main__':
    main()
