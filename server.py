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


def render_template(template_name, device, **context):
    db = DigitalLibraryDatabase()
    test_books = [
        {
            "title": "Что такое математика?",
            "author": "Р. Курант, Г. Роббинс",
            "id": "courant",
            "handed": 14,
            "count": 20,
            "old": 30,
            "oldOwnerName": "Иван Иванов",
            "oldOwnerNameInRP": "Ивана Иванова",
            "oldOwner": "IvanIvanovId",
            "code": 1234567890123,
        },
        {
            "title": "Сборник задач по алгебре",
            "author": "М.Л. Галицкий, А.М. Гольдман, Л.И. Звавич",
            "id": "gal",
            "handed": 14,
            "count": 20,
            "old": 30,
            "oldOwnerName": "Иван Иванов",
            "oldOwnerNameInRP": "Ивана Иванова",
            "oldOwner": "IvanIvanovId",
            "code": 1234567890123,
        },
        {
            "title": "Алгоритмы: построение и анализ",
            "author": "К. Штайн, Р. Линн Ривест, Т. Кормен, Ч. Эрик Лейзерсон",
            "id": "cormen",
            "handed": 14,
            "count": 20,
            "old": 30,
            "oldOwnerName": "Иван Иванов",
            "oldOwnerNameInRP": "Ивана Иванова",
            "oldOwner": "IvanIvanovId",
            "code": 1234567890123,
        },
        {
            "title": "Совершенный код",
            "author": "С. Макконнелл",
            "id": "codecompl",
            "handed": 14,
            "count": 20,
            "old": 30,
            "oldOwnerName": "Иван Иванов",
            "oldOwnerNameInRP": "Ивана Иванова",
            "oldOwner": "IvanIvanovId",
            "code": 1234567890123,
        },
    ]
    test_users = [
        {
            "name": "Игорь Тараканов",
            "id": "Igor_Tarakanov",
            "booksCount": 8,
            "oldBookId": "courant",
            "oldBookDate": 12,
            "books": [
                {"id": "courant", "title": "Что такое математика?"},
                {"id": "gal", "title": "Сборник задач по алгебре"},
            ],
        },
        {
            "name": "Юрий Сыровецкий",
            "id": "Yuriy_Syrovetskiy",
            "booksCount": 8,
            "oldBookId": "courant",
            "oldBookDate": 12,
            "books": [
                {"id": "courant", "title": "Что такое математика?"},
                {"id": "gal", "title": "Сборник задач по алгебре"},
            ],
        }
    ]
    test_context = {
        'user': {
            "name": "Иван Иванов",
            "priority": "librarian" # student, specially_trained, librarian
        },
        'device': device,
        'books': test_books,
        'booksLen': len(test_books),
        'recomendedBooks': test_books,
        'recomendedBooksLen': len(test_books),
        'page': template_name,
        "handedBooks": test_books,
        "editBook": {
            "title": "Что такое математика?",
            "author": "Р. Курант, Г. Роббинс",
            "id": "courant",
            "handed": 14,
            "count": 20,
            "old": 30,
            "oldOwnerName": "Иван Иванов",
            "oldOwnerNameInRP": "Ивана Иванова",
            "oldOwner": "IvanIvanovId",
            "code": 1234567890123,
        },
        "users": test_users,
        "handlog": db.handlog.get(),
        "handlogLen": len(db.handlog.get()),
    }
    return flask.render_template(
        template_name + '.html',
        **dict(context, template_name=template_name, **test_context)
    )


@app.route("/login")
def login():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    if not db.logs._exists({"ip": client_ip}):
        return render_template("auth", "terminall")
    if db.logs._find_one({"ip": client_ip})["status"] == "terminal":
        return render_template("operations", "terminal")
    if db.logs._find_one({"ip": client_ip})["status"] == "computer":
        return render_template("login", "terminall")


@app.route("/registration")
def reg():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    if not db.logs._exists({"ip": client_ip}):
        return render_template("auth", "terminall")
    if db.logs._find_one({"ip": client_ip})["status"] == "terminal":
        return render_template("operations", "terminal")
    if db.logs._find_one({"ip": client_ip})["status"] == "computer":
        return render_template("registration", "terminall")


@app.route("/")
def home():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    if not db.logs._exists({"ip": client_ip}):
        return render_template("auth", "terminall")
    if db.logs._find_one({"ip": client_ip})["status"] == "terminal":
        return render_template("operations", "terminal")
    if db.logs._find_one({"ip": client_ip})["status"] == "computer":
        return render_template("home", "terminall")


@app.route("/handed")
def handed():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    if not db.logs._exists({"ip": client_ip}):
        return render_template("auth", "terminall")
    if db.logs._find_one({"ip": client_ip})["status"] == "terminal":
        return render_template("operations", "terminal")
    if db.logs._find_one({"ip": client_ip})["status"] == "computer":
        return render_template("handed", "terminall")


@app.route("/books")
def books():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    if not db.logs._exists({"ip": client_ip}):
        return render_template("auth", "terminall")
    if db.logs._find_one({"ip": client_ip})["status"] == "terminal":
        return render_template("operations", "terminal")
    if db.logs._find_one({"ip": client_ip})["status"] == "computer":
        return render_template("books", "terminall")


@app.route("/operations")
def operations():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    if not db.logs._exists({"ip": client_ip}):
        return render_template("auth", "terminall")
    if db.logs._find_one({"ip": client_ip})["status"] == "terminal":
        return render_template("operations", "terminal")
    if db.logs._find_one({"ip": client_ip})["status"] == "computer":
        return render_template("operations", "terminall")


@app.route("/add")
def add():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    if not db.logs._exists({"ip": client_ip}):
        return render_template("auth", "terminall")
    if db.logs._find_one({"ip": client_ip})["status"] == "terminal":
        return render_template("operations", "terminal")
    if db.logs._find_one({"ip": client_ip})["status"] == "computer":
        return render_template("add", "terminall")


@app.route("/users")
def users():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    if not db.logs._exists({"ip": client_ip}):
        return render_template("auth", "terminall")
    if db.logs._find_one({"ip": client_ip})["status"] == "terminal":
        return render_template("operations", "terminal")
    if db.logs._find_one({"ip": client_ip})["status"] == "computer":
        return render_template("users", "terminall")


@app.route("/journal")
def journal():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    if not db.logs._exists({"ip": client_ip}):
        return render_template("auth", "terminall")
    if db.logs._find_one({"ip": client_ip})["status"] == "terminal":
        return render_template("operations", "terminal")
    if db.logs._find_one({"ip": client_ip})["status"] == "computer":
        return render_template("journal", "terminall")


@app.route("/auth")
def auth():
    return render_template("auth", "terminall")


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
    user, book, uuid = form["user"], form["book"], form["uuid"]
    print(db.hands.exists(user, book))
    if db.hands.exists(user, book):
        db.hands.delete(user, book)
        action = Action.Return
    else:
        db.hands.add(user, book, uuid)
        action = Action.Take
    db.handlog.log(action, user, book, uuid)
    return jsonify(action=action.name, book=book)


@app.route('/api/device/auth', methods=['POST'])
def api_device_auth():
    client_ip = request.remote_addr
    db = DigitalLibraryDatabase()
    form = request.form
    if not db.logs._exists({"ip": client_ip}):
        db.logs._insert({"ip": client_ip, "try": 0, "status": "none"})
    if db.logs._exists({"ip": client_ip}) and int(db.logs._find_one({"ip": client_ip})["try"]) > 10:
        return ""
    if form["password"] == "1303303113033031":
        db.logs._remove({"ip": client_ip})
        db.logs._insert({"ip": client_ip, "status": "computer", "try": "0"})
    else:
        tr = db.logs._find_one({"ip": client_ip})["try"]
        db.logs._remove({"ip": client_ip})
        db.logs._insert({"ip": client_ip, "try": int(tr) + 1, "status": "none"})
    return jsonify(answer="good")


def main():
    logging.basicConfig(level=logging.DEBUG)
    config = load_config()
    app.run(host=config["host"], debug=True)


if __name__ == '__main__':
    main()
