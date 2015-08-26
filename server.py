#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from digital_library.database import DigitalLibraryDatabase
from digital_library.types import Action

from flask import Flask, jsonify, request, redirect
from datetime import datetime
import configparser
import flask
import logging
import uuid


app = Flask('DigitalLibraryApplication')


def load_config():
    config = configparser.ConfigParser()
    config.read('config')
    return config['Server']


def render_template(template_name, user, client_ip):

    if user["priority"] == "student" and template_name not in [
        "login",
        "registration",
        "handed",
        "books",
        "operations",
    ]:
        return flask.render_template("404.html", template_name="404")
    if user["priority"] == "librarian" and template_name not in [
        "login",
        "registration",
        "home",
        "handed",
        "books",
        "add",
        "users",
        "journal",
    ]:
        return flask.render_template("404.html", template_name="404")
    db = DigitalLibraryDatabase()
    hands = db.hands.find({"user": user["nfc"]})
    handedBooks = []
    for hand in hands:
        book = db.books.get({"barcode": hand["book"]})
        handedBooks += [book]
    handlog = db.handlog.find({})
    page_context = {
        "user": user,
        "handedBooks": handedBooks,
        "handedBooksLen": len(handedBooks),
        "ip": client_ip,
        "handlog": handlog,
        "handlogLen": len(handlog),
        "log": db.logs.get({"ip": client_ip}),
    }
    return flask.render_template(
        template_name + '.html',
        **dict(template_name=template_name, **page_context)
    )


def crossroad(template_name, client_ip):
    db = DigitalLibraryDatabase()
    log = db.logs.get({"ip": client_ip})
    print(client_ip)

    if log is None and template_name in ["login", "registration"]: # Перенаправление на страницы авторизации и решистрации
        return render_template(template_name, {"priority": "student", "nfc": ""}, client_ip)

    if log is None: # Перенаправление на страницу авторизации(не найден лог)
        return redirect("/login")

    user = db.users.get({"id": log["user"]})


    if template_name in ["login", "registration"]:
        return redirect("/")

    if user is None: # Перенаправление на страницу авторизации(неправильный лог)
        db.logs.remove({"ip": client_ip})
        return redirect("/login")

    if log["remember"] == "false" and log["datetime"] != str(datetime.utcnow())[0:-11]: # Перенаправление на страницу авторизации(недолговременный лог)
        db.logs.remove({"ip": client_ip})
        return redirect("/login")

    if log["is_terminal"]: # Перенаправление на страницу операций(запрос с терминала)
        return render_template("operations", {"priority": "student", "nfc": ""}, client_ip)

    if not log["is_terminal"] and template_name == "operations": # 404 (неправильное устройство запроса)
        return flask.render_template("404.html", template_name="404")


    if user["priority"] == "student" and template_name not in [ # 404 (неправильный приоритет запроса)
        "login",
        "registration",
        "home",
        "handed",
        "books",
        "operations",
    ]:
        return flask.render_template("404.html", template_name="404")

    if user["priority"] == "librarian" and template_name not in [ # 404 (неправильный приоритет запроса)
        "login",
        "registration",
        "home",
        "handed",
        "books",
        "add",
        "users",
        "journal",
    ]:
        return flask.render_template("404.html", template_name="404")

     # Данные корректны
    return render_template(template_name, db.users.get({"id": log["user"]}), client_ip)


@app.route("/")
def home():
    return crossroad("handed", request.remote_addr)


@app.route('/api/login', methods=['POST'])
def api_login():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    ip = db.ips.get({"ip": client_ip})
    form = request.form
    user = db.users.get({"login": form["login"], "password": form["password"]})

    if ip is None:
        db.ips.insert({"ip": client_ip, "logAttempts": 0, "regAttempts": 0, "datetime": str(datetime.utcnow())[0:-16]})
        ip = db.ips.get({"ip": client_ip})

    if ip["datetime"] != str(datetime.utcnow())[0:-16]:
        db.ips.update({"ip": client_ip}, {"logAttempts": 0, "datetime": str(datetime.utcnow())[0:-16]})

    if ip["logAttempts"] > 10:
        return jsonify(answer="fail")

    if user is None:
        db.ips.update({"ip": client_ip}, {"logAttempts": ip["logAttempts"] + 1})
        return jsonify(answer="fail")
    else:
        db.logs.insert({
            "user": user["id"],
            "ip": client_ip,
            "is_terminal": False,
            "remember": form["remember"],
            "datetime": str(datetime.utcnow())[0:-11],
        })
        return jsonify(answer="ok")
    return jsonify(answer="fail")


@app.route('/api/registration', methods=['POST'])
def api_registration():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    ip = db.ips.get({"ip": client_ip})
    form = request.form
    invitation = db.invitations.get({"inviteCode": form["inviteCode"]})

    if ip is None:
        db.ips.insert({"ip": client_ip, "logAttempts": 0, "regAttempts": 0, "datetime": str(datetime.utcnow())[0:-16]})
        ip = db.ips.get({"ip": client_ip})

    if ip["datetime"] != str(datetime.utcnow())[0:-16]:
        db.ips.update({"ip": client_ip}, {"regAttempts": 0, "datetime": str(datetime.utcnow())[0:-16]})

    if ip["regAttempts"] > 10:
        return jsonify(answer="fail")

    if invitation is None:
        db.ips.update({"ip": client_ip}, {"regAttempts": ip["regAttempts"] + 1})
        return jsonify(answer="fail")
    else:
        user_uudi = uuid.uuid4()
        db.users.insert({
            "login": form["login"],
            "password": form["password"],
            "nfc": invitation["nfc"],
            "RuName": invitation["RuName"],
            "id": user_uudi,
            "priority": invitation["priority"],
        })
        db.logs.insert({
            "user": user_uudi,
            "ip": client_ip,
            "is_terminal": False,
            "remember": "false",
            "datetime": str(datetime.utcnow())[0:-11],
        })
        db.invitations.remove({"inviteCode": invitation["inviteCode"]})
        return jsonify(answer="ok")
    return jsonify(answer="fail")


@app.route('/api/exit', methods=['POST'])
def api_exit():
    form = request.form
    db = DigitalLibraryDatabase()
    db.logs.remove({"ip": form["ip"]})
    print(form["ip"])
    return jsonify(answer="ok")


@app.route('/api/book/action', methods=['POST'])
def api_book_action():
    form = request.form
    db = DigitalLibraryDatabase()
    user, book = form["user"], form["book"]
    if db.hands.exists({"user": user, "book": book}):
        db.hands.remove({"user": user, "book": book})
        action = Action.Return
    else:
        db.hands.insert({"user": user, "book": book})
        action = Action.Take
    db.handlog.insert({
        "user": user,
        "book": book,
        "datetime": str(datetime.utcnow())[0:-6],
    })
    return jsonify(action=action.name, book=book)


@app.route("/<template_name>")
def render(template_name):
    if template_name not in [
        "login",
        "registration",
        "home",
        "handed",
        "journal",
        "operations",
    ]:
        return render_template("404", {"priority": "student", "nfc": "asd"}, request.remote_addr)
    return crossroad(template_name, request.remote_addr)


def main():
    logging.basicConfig(level=logging.DEBUG)
    config = load_config()
    app.run(host=config["host"], debug=True, port=1303)


if __name__ == '__main__':
    main()


template_user = {
    "login": "str",
    "password": "str",
    "nfc": "str",
    "RuName": "RuStr",
    "id": "str",
    "priority": "str",
}

template_book = {
    "title": "RuStr",
    "author": "RuStr",
    "count": "int",
    "barcode": "int",
}

tempalte_log = {
    "user": "str",
    "ip": "str",
    "is_terminal": "bool",
    "remember": "str",
    "datetime": "str",
}

tempalte_hand = {
    "user": "str",
    "book": "str",
    "datetime": "str",
}

tempalte_journal =  {
    "user": "str",
    "book": "str",
    "datetime": "str",
}

tempalte_ip = {
    "ip": "str",
    "attempts": "int",
}

tempalte_invitation = {
    "RuName": "RuStr",
    "nfc": "str",
    "inviteCode": "str",
    "priority": "str",
}