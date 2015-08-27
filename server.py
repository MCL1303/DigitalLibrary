#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from digital_library.database import DigitalLibraryDatabase
from digital_library.types import Action
from digital_library.resizer import resizer

from flask import Flask, jsonify, request, redirect
from datetime import datetime
import urllib.request
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
    db = DigitalLibraryDatabase()
    handedBooks = []
    if user["priority"] == "librarian":
        hands = db.hands.find({})
        allHandedBooks = []
        for hand in hands:
            book = db.books.get({"barcode": hand["book"]})
            book["owner"] = db.users.get({"nfc": hand["user"]})["name"]
            book["time"] = hand["datetime"]
            book["count"] = int(book["count"])
            allHandedBooks += [book]
        current_book = {}
        for i in range(0, len(allHandedBooks)):
            print("\n\n\n\n")
            print(allHandedBooks[i])
            current_book = allHandedBooks[i]
            current_book["handed"] = 1
            current_book["old"] = (
                (datetime.utcnow() - allHandedBooks[i]["time"]).days
            )
            current_book["oldName"] = allHandedBooks[i]["owner"]
            current_book["oldOwner"] = (
                db.users.get({"name": allHandedBooks[i]["owner"]})["id"]
            )
            for j in range(i + 1, len(allHandedBooks)):
                if allHandedBooks[i]["book"] != allHandedBooks[j]["book"]:
                    continue
                else:
                    current_book["handed"] += 1
                    handed_days_ago = (
                        (datetime.utcnow() - allHandedBooks[j]["time"]).days
                    )
                    if current_book["old"] < handed_days_ago:
                        current_book["old"] = handed_days_ago
                        current_book["oldName"] = allHandedBooks[j]["owner"]
                        current_book["oldOwner"] = (
                            db.users.get({"name": allHandedBooks[j]["owner"]})
                            ["id"]
                        )
            if current_book != {}:
                handedBooks += [current_book]
            print(current_book)
    else:
        hands = db.hands.find({"user": user["nfc"]})
        for hand in hands:
            book = db.books.get({"barcode": hand["book"]})
            handedBooks += [book]
    handlog = db.handlog.find({})
    for handl in handlog:
        if handl["action"] == "Take":
            handl["RuAction"] = "Взял"
        else:
            handl["RuAction"] = "Вернул"
        handl["RuName"] = db.users.get({"nfc": handl["user"]})["name"]
        handl["RuBook"] = db.books.get({"barcode": handl["book"]})["title"]
    page_context = {
        "user": user,
        "handedBooks": handedBooks,
        "handedBooksLen": len(handedBooks),
        "ip": client_ip,
        "handlog": handlog,
        "handlogLen": len(handlog),
        "log": db.sessions.get({"ip": client_ip}),
    }
    print(handedBooks)
    return flask.render_template(
        template_name + '.html',
        **dict(template_name=template_name, **page_context)
    )


def crossroad(template_name, client_ip):
    db = DigitalLibraryDatabase()
    session = db.sessions.get({"ip": client_ip})

    if session is None and template_name in ["login", "registration"]:
        return render_template(
            template_name,
            {"priority": "student", "nfc": ""},
            client_ip
        )

    if session is None:
        return redirect("/login")

    user = db.users.get({"id": session["user"]})

    if template_name in ["login", "registration"]:
        return redirect("/")

    if user is None:
        db.sessions.remove({"ip": client_ip})
        return redirect("/login")

    if (
        session["remember"] == "false"
        and session["datetimeStr"] != str(datetime.utcnow())[0:-11]
    ):
        db.sessions.remove({"ip": client_ip})
        return redirect("/login")

    if session["is_terminal"]:
        return render_template(
            "operations",
            {"priority": "student", "nfc": ""},
            client_ip
        )

    if not session["is_terminal"] and template_name == "operations":
        return redirect("/")

    if user["priority"] == "student" and template_name not in [
        "login",
        "registration",
        "home",
        "handed",
        "books",
        "operations",
    ]:
        return redirect("/")

    if user["priority"] == "librarian" and template_name not in [
        "login",
        "registration",
        "home",
        "handed",
        "add",
        "journal",
    ]:
        return redirect("/")

    return render_template(
        template_name,
        db.users.get({"id": session["user"]}),
        client_ip
    )


@app.route("/")
def home():
    return redirect("/handed")


@app.route('/api/login', methods=['POST'])
def api_login():
    db = DigitalLibraryDatabase()
    client_ip = request.remote_addr
    ip = db.ips.get({"ip": client_ip})
    form = request.form
    user = db.users.get({"login": form["login"], "password": form["password"]})

    if ip is None:
        db.ips.insert({
            "ip": client_ip,
            "logAttempts": 0,
            "regAttempts": 0,
            "datetimeStr": str(datetime.utcnow())[0:-16]
        })
        ip = db.ips.get({"ip": client_ip})

    if ip["datetimeStr"] != str(datetime.utcnow())[0:-16]:
        db.ips.update(
            {"ip": client_ip},
            {"logAttempts": 0, "datetimeStr": str(datetime.utcnow())[0:-16]}
        )

    if ip["logAttempts"] > 10:
        return jsonify(answer="fail")

    if (
        user is None
        and form["login"] == "terminal"
        and form["password"] == "terminal"
    ):
        db.users.insert({"id": "terminal", "priority": "student", "nfc": ""})
        db.sessions.insert({
            "user": "terminal",
            "ip": client_ip,
            "is_terminal": True,
            "remember": form["remember"],
            "datetimeStr": str(datetime.utcnow())[0:-11],
        })
        return jsonify(answer="ok")

    if user is None:
        db.ips.update(
            {"ip": client_ip},
            {"logAttempts": ip["logAttempts"] + 1}
        )
        return jsonify(answer="fail")
    else:
        db.sessions.insert({
            "user": user["id"],
            "ip": client_ip,
            "is_terminal": False,
            "remember": form["remember"],
            "datetimeStr": str(datetime.utcnow())[0:-11],
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
        db.ips.insert({
            "ip": client_ip,
            "logAttempts": 0,
            "regAttempts": 0,
            "datetimeStr": str(datetime.utcnow())[0:-16]
        })
        ip = db.ips.get({"ip": client_ip})

    if ip["datetimeStr"] != str(datetime.utcnow())[0:-16]:
        db.ips.update(
            {"ip": client_ip},
            {"regAttempts": 0, "datetimeStr": str(datetime.utcnow())[0:-16]}
        )

    if ip["regAttempts"] > 10:
        return jsonify(answer="fail")

    if invitation is None:
        db.ips.update(
            {"ip": client_ip},
            {"regAttempts": ip["regAttempts"] + 1}
        )
        return jsonify(answer="fail")
    else:
        user_uudi = uuid.uuid4()
        db.users.insert({
            "login": form["login"],
            "password": form["password"],
            "nfc": invitation["nfc"],
            "name": invitation["name"],
            "id": user_uudi,
            "priority": invitation["priority"],
        })
        db.sessions.insert({
            "user": user_uudi,
            "ip": client_ip,
            "is_terminal": False,
            "remember": "false",
            "datetimeStr": str(datetime.utcnow())[0:-11],
        })
        db.invitations.remove({"inviteCode": invitation["inviteCode"]})
        return jsonify(answer="ok")
    return jsonify(answer="fail")


@app.route('/api/exit', methods=['POST'])
def api_exit():
    form = request.form
    db = DigitalLibraryDatabase()
    db.sessions.remove({"ip": form["ip"]})
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
        db.hands.insert({
            "user": user,
            "book": book,
            "datetime": datetime.utcnow()
        })
        action = Action.Take
    db.handlog.insert({
        "user": user,
        "book": book,
        "datetimeStr": str(datetime.utcnow())[0:-7],
        "datetime": datetime.utcnow(),
        "action": action.name,
    })
    return jsonify(action=action.name, book=book)


@app.route('/api/book/add', methods=['POST'])
def api_book_add():
    form = request.form
    db = DigitalLibraryDatabase()
    db.books.insert({
        "title": form["title"],
        "author": form["author"],
        "count": form["count"],
        "barcode": form["code"],
        })
    local_filename, trash = urllib.request.urlretrieve(form["url"])
    resizer(local_filename, "book", form["code"], "jpg")
    return jsonify(answer="ok")


@app.route('/api/book/change', methods=['POST'])
def api_book_change():
    form = request.form
    db = DigitalLibraryDatabase()
    if db.books.get({"barcode": form["barcode"]}) is None:
        return jsonify(answer="fail")
    db.books.update({"barcode": form["barcode"]}, {
        "title": form["title"],
        "author": form["author"],
        "count": form["count"],
    })
    return jsonify(answer="ok")


@app.route('/api/book/delete', methods=['POST'])
def api_book_delete():
    form = request.form
    db = DigitalLibraryDatabase()
    if db.books.get({"barcode": form["barcode"]}) is None:
        return jsonify(answer="fail")
    db.books.remove({"barcode": form["barcode"]})
    db.handlog.remove({"book": form["barcode"]})
    db.hands.remove({"book": form["barcode"]})
    return jsonify(answer="ok")


@app.route("/<template_name>")
def render(template_name):
    if template_name not in [
        "login",
        "registration",
        "home",
        "handed",
        "journal",
        "operations",
        "add",
    ]:
        return render_template(
            "404",
            {
                "priority": "student",
                "nfc": "asd"
            },
            request.remote_addr
        )
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
    "name": "RuStr",
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
    "datetimeStr": "str",
}

tempalte_hand = {
    "user": "str",
    "book": "str",
    "datetimeStr": "str",
}

tempalte_journal = {
    "user": "str",
    "book": "str",
    "datetimeStr": "str",
    "action": "srt",
}

tempalte_ip = {
    "ip": "str",
    "attempts": "int",
}

tempalte_invitation = {
    "name": "RuStr",
    "nfc": "str",
    "inviteCode": "str",
    "priority": "str",
}
