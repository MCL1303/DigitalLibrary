#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from flask import Flask, jsonify, request, redirect, make_response
from digital_library.database import DigitalLibraryDatabase
from digital_library.types import Action, ClientType, AccessLevel
from digital_library.resizer import Resize
from datetime import datetime, timedelta
from hashlib import sha512, sha1
import flask
import configparser
import uuid
import random
import string
import urllib.request


app = Flask('DigitalLibraryApplication')


def load_config():
    config = configparser.ConfigParser()
    config.read('config')
    return config['Server']


HASH_BASE = sha512
HASH_SIZE = HASH_BASE().digest_size

def hash(password, salt):
    h = (password + salt).encode()
    for i in range(1024):
        h = HASH_BASE(h).digest()
    return h


@app.route("/")
def home():
    return redirect("/handed")


def password_checker(password):
    a = [0, 0]
    if len(password) < 8:
        return True
    for i in password:
        if i.isalpha():
            a[0] = 1
        if i.isdigit():
            a[1] = 1
    return not(a[0] == a[1] == 1)


@app.route('/api/user/registration', methods=['POST'])
def api_registration():
    db = DigitalLibraryDatabase()
    form = request.form
    if password_checker(form["password"]):
        return jsonify(answer="bad_password")
    if db.users.get({"login": form["login"]}) is not None:
        return jsonify(answer="login_taken")
    if db.users.get({"email": form["email"]}) is not None:
        return jsonify(answer="email_taken")
    user = db.users.get({"inviteCode": form["inviteCode"], "status": "off"})
    if user is None:
        return jsonify(answer="invite_not_found")
    else:
        try:
            salt = "".join(
                random.choice(string.printable) for _ in range(HASH_SIZE)
            )
            user["login"] = form["login"]
            user["email"] = form["email"]
            user["password"] = hash(form["password"], salt)
            user["status"] = "on"
            user["salt"] = salt
        except KeyError:
            return jsonify(answer="fail")
    db.users.remove({"inviteCode" :form["inviteCode"], "nfc": user["nfc"]})
    db.users.insert(user)
    return jsonify(answer="ok")


@app.route('/api/user/login', methods=['POST'])
def api_login():
    db = DigitalLibraryDatabase()
    form = request.form
    salt = db.users.get({"login": form["login"]})
    if salt is None:
         return jsonify(answer="error")
    salt = salt["salt"]
    user = db.users.get({
        "login": form["login"],
        "password": hash(form["password"], salt)
    })
    if user is None:
        return jsonify(answer="error")
    session_id = db.sessions.insert({
        "user": form["login"],
        "datetime": datetime.utcnow(),
        "clienttype": ClientType.User.name,
        "ip": str(request.remote_addr),
        "browser": request.user_agent.browser,
        "version": request.user_agent.version and
        int(request.user_agent.version.split('.')[0]),
        "platform": request.user_agent.platform,
        "uas": request.user_agent.string,
        "remember": form["remember"],
    })
    resp = make_response(jsonify(answer="ok"))
    if form["remember"] == "true":
        resp.set_cookie("session_id", session_id, max_age=int(timedelta(days=4).total_seconds()))
    else:
        resp.set_cookie("session_id", session_id, max_age=int(timedelta(minutes=2).total_seconds()))
    return resp


@app.route('/api/user/exit', methods=['POST'])
def api_exit():
    session_id = request.cookies.get('session_id')
    form = request.form
    db = DigitalLibraryDatabase()
    db.sessions.remove({"_id": session_id})
    return jsonify(answer="ok")


def int_checker(count):
    for i in count:
        if not i.isdigit():
            return True
    return False


@app.route('/api/book/change', methods=['POST'])
def api_book_change():
    form = request.form
    db = DigitalLibraryDatabase()
    if db.books.get({"barcode": form["barcode"]}) is None:
        return jsonify(answer="fail")
    if int_checker(form["count"]):
        return jsonify(answer="fail")
    db.books.update({"barcode": form["barcode"]}, {
        "title": form["title"],
        "author": form["author"],
        "count": int(form["count"]),
    })
    print(db.books.get({"barcode": form["barcode"]}))
    return jsonify(answer="ok")


@app.route('/api/book/delete', methods=['POST'])
def api_book_delete():
    form = request.form
    db = DigitalLibraryDatabase()
    if db.books.get({"barcode": form["barcode"]}) is None:
        print("asd")
        return jsonify(answer="fail")
    db.books.remove({"barcode": form["barcode"]})
    db.handlog.remove({"book": form["barcode"]})
    db.hands.remove({"book_barcode": form["barcode"]})
    return jsonify(answer="ok")


@app.route('/api/book/add', methods=['POST'])
def api_book_add():
    form = request.form
    db = DigitalLibraryDatabase()
    if
    if int_checker(form["count"]):
        return jsonify(answer="fail")
    db.books.insert({
        "title": form["title"],
        "author": form["author"],
        "count": int(form["count"]),
        "barcode": form["code"],
        })
    local_filename, trash = urllib.request.urlretrieve(form["url"])
    resizer(local_filename, "book", form["code"], "jpg")
    return jsonify(answer="ok")


def render_template(page_name, user):
    config = load_config()
    db = DigitalLibraryDatabase()
    page_context = {}
    if user["accesslevel"] == AccessLevel.Student.name:
        if page_name not in config["student_pages"]:
            return redirect("/handed")
        else:
            handed = db.hands.find({"user_id": user["id"]})
            page_context = {
                "user": user,
                "handed": handed,
                "len": len(handed),
                "page_name": page_name,
            }
    if user["accesslevel"] == AccessLevel.Librarian.name:
        if page_name not in config["librarian_pages"]:
            return redirect("/handed")
        else:
            if page_name == "add":
                db = DigitalLibraryDatabase()
                page_context = {
                    "user": user,
                    "page_name": page_name,
                }
            if page_name == "journal":
                db = DigitalLibraryDatabase()
                handlog = db.handlog.find({})
                page_context = {
                    "user": user,
                    "handlog": handlog[-20:],
                    "len": len(handlog[-20:]),
                    "page_name": page_name,
                }
            if page_name == "handed":
                hands = db.hands.find({})
                books = []
                flag = True
                for hand in hands:
                    flag = True
                    for book in books:
                        if book["barcode"] == hand["book_barcode"]:
                            flag = False
                            book["handed"] += 1
                            if book["old_datetime"] < (datetime.utcnow() - hand["datetime"]).days:
                                book["old_datetime"] = (datetime.utcnow() - hand["datetime"]).days
                                book["old_owner_id"] = hand["user_id"]
                                book["old_owner_name"] = hand["user_name"]
                    if flag:
                        book = db.books.get({"barcode": hand["book_barcode"]})
                        new_book = {
                            "barcode": book["barcode"],
                            "title": book["title"],
                            "author": book["author"],
                            "old_datetime": (datetime.utcnow() - hand["datetime"]).days,
                            "old_owner_id": hand["user_id"],
                            "old_owner_name": hand["user_name"],
                            "handed": 1,
                            "count": book["count"]
                        }
                        books += [new_book]
                page_context = {
                    "user": user,
                    "page_name": page_name,
                    "len": len(books),
                    "books": books,
                }
    return flask.render_template(page_name + '.html', **dict(**page_context))


@app.route("/<page_name>")
def cookie_check(page_name):
    config = load_config()
    if page_name not in config["pages"]:
        return flask.render_template("404.html")
    db = DigitalLibraryDatabase()
    session_id = request.cookies.get('session_id')
    session = db.sessions.get({"_id": session_id})
    if session is None:
        if page_name == "registration":
            return flask.render_template("registration.html")
        if page_name == "login":
            return flask.render_template("login.html")
        return redirect("/login")
    else:
        db.sessions.remove(session)
        if (datetime.utcnow() - session["datetime"]).days > 7:
            return redirect("/login")
        client_session = {
            "user": session["user"],  # TODO refactor
            "datetime": session["datetime"],  # TODO refactor
            "clienttype": session["clienttype"],  # TODO refactor
            "ip": str(request.remote_addr),
            "browser": request.user_agent.browser,
            "version": request.user_agent.version and
            int(request.user_agent.version.split('.')[0]),
            "platform": request.user_agent.platform,
            "uas": request.user_agent.string,
            "remember": session["remember"],  # TODO refactor
            "_id": session["_id"],  # TODO refactor
        }
        if client_session == session:  # TODO refactor
            user = db.users.get({"login": session["user"]})
            session["datetime"] = datetime.utcnow()
            db.sessions.insert(client_session)
            if page_name in ["login", "registration"]:
                return redirect("/handed")
            else:
                resp = make_response(render_template(page_name, user))
            if session["remember"] == "true":
                resp.set_cookie("session_id", session_id, max_age=int(timedelta(days=4).total_seconds()))
            else:
                resp.set_cookie("session_id", session_id, max_age=int(timedelta(minutes=2).total_seconds()))
            return resp


def main():
    config = load_config()
    app.run(host=config["host"], debug=True, port=int(config["port"]))


if __name__ == '__main__':
    main()

template_user = {
    "login": "str",
    "password": "str",
    "name": "str",
    "accesslevel": "str",
    "nfc": "str",
    "invitecode": "str",
    "status": "str",
    "email": "str",
    "handed": "int",
    "salt": "str",
}

template_book = {
    "title": "str",
    "author": "str",
    "count": "int",
    "barcode": "str",
}

template_hand = {
    "user_id": "str",
    "user_name": "str",
    "book_barcode": "str",
    "book_title": "str",
    "book_author": "str",
    "datetime": "datetime",
}

template_journal = {
    "user": "str",
    "book": "str",
    "datetime": "datetime",
    "action": "action",
    "action_name": "srt",
    "datetime_str": "str",
    "book_title": "str",
    "user_name": "str",
}

template_session = {
    "user": "str",
    "datetime": "datetime",
    "clienttype": "srt",
    "ip": "str",
    "browser": "str",
    "version":  "str",
    "platform": "str",
    "uas": "str",
    "remember": "str",
}
