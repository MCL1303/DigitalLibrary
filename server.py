#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Digital Library — a digital book management system
# Copyright (C) 2015  Igor Tarakanov <igortarakanov144999usa@gmail.com>,
#                     Yuriy Syrovetskiy <cblp@cblp.su>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from digital_library.database import DigitalLibraryDatabase
from digital_library.typesx import ClientType, AccessLevel, Action
from digital_library.resizer import Resize

import configparser
from datetime import datetime, timedelta
from flask import Flask, jsonify, request, redirect, make_response
import flask
from hashlib import sha512
import random
import string
import urllib.request
from uuid import uuid4
import re


app = Flask('DigitalLibraryApplication')


def fields_are(first, second):
    for item in second.items():
        key = item[0]
        try:
            if first[key] != item[1]:
                return False
        except KeyError:
            return False
    return True


def id_generator(n):
    return ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(n))


def load_config():
    config = configparser.ConfigParser()
    config.read('config')
    return config['Server']


Hash = sha512
HASH_SIZE = Hash().digest_size  # pylint: disable=no-member


def hash(password, salt):
    h = (password + salt).encode()
    for _ in range(1024):
        h = Hash(h).digest()
    return h


def hash2(password):
    h = (password).encode()
    for _ in range(3):
        h = Hash(h).digest()
    return h


def check_permitions(session_id):
    db = DigitalLibraryDatabase()
    session = db.sessions.get({"id": session_id})
    if session is None:
        return True
    user = db.users.get({"login": session["user_login"]})
    if user is None:
        return True
    return not user["accessLevel"] == "Librarian"


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
    return not a[0] == a[1] == 1


def login_checker(login):
    for i in login:
        if (
            not(ord(i) >= ord("A") and ord(i) <= ord("Z")) and
            not(ord(i) >= ord("a") and ord(i) <= ord("z")) and
            not(ord(i) >= ord("0") and ord(i) <= ord("9")) and
            not(ord(i) == ord("-") or ord(i) == ord("_"))
        ):
            return True
    return False


def email_checker(email):
    if len(email) > 7 and not(" " in email):
        if re.match("^.+\\@(\\[?)[a-zA-Z0-9\\-\\.]+\\.([a-zA-Z]{2,3}|[0-9]{1,3})(\\]?)$", email) != None:
            return False
    return True


@app.route('/api/user/registration', methods=['POST'])
def api_registration():
    db = DigitalLibraryDatabase()
    form = request.form
    if login_checker(form["login"]):
        return jsonify(answer="bad_login")
    if password_checker(form["password"]):
        return jsonify(answer="bad_password")
    if email_checker(form["email"]):
        return jsonify(answer="bad_email")
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
            user["name"] = form["name"].replace(" ", "").title() + " " + form["s_name"].replace(" ", "").title()
            user["search_full_name"] = (form["name"].lower() + form["s_name"].lower()).replace(" ", "")
            user["search_r_full_name"] = (form["s_name"].lower() + form["name"].lower()).replace(" ", "")
            user["search_name"] = form["name"].lower().replace(" ", "")
            user["search_s_name"] = form["s_name"].lower().replace(" ", "")
            flag = True
            while flag:
                n_id = id_generator(16)
                if db.users.get({"id": n_id}) is None:
                    user["id"] = id_generator(16)
                    flag = False
        except KeyError:
            return jsonify(answer="fail")
    db.users.remove({"inviteCode": form["inviteCode"], "nfc": user["nfc"]})
    db.users.insert(user)
    local_filename, _ = urllib.request.urlretrieve("https://en.opensuse.org/images/0/0b/Icon-user.png")
    Resize(local_filename, "user", user["id"], "jpg")
    return jsonify(answer="ok")


COOKIE_AGE_REMEMBER = int(timedelta(days=4).total_seconds())

COOKIE_AGE_NOT_REMEMBER = int(timedelta(minutes=10).total_seconds())


@app.route('/api/user/login', methods=['POST'])
def api_login():
    db = DigitalLibraryDatabase()
    form = request.form
    salt = db.users.get({"login": form["login"], "status": "on"})
    if salt is None:
        return jsonify(answer="error")
    salt = salt["salt"]
    user = db.users.get({
        "login": form["login"],
        "password": hash(form["password"], salt)
    })
    if user is None:
        return jsonify(answer="error")
    session_id = str(uuid4())
    db.sessions.insert({
        "user_login": form["login"],
        "datetime": datetime.utcnow(),
        "clienttype": ClientType.User.name,
        "ip": str(request.remote_addr),
        "browser": request.user_agent.browser,
        "version": request.user_agent.version and
        int(request.user_agent.version.split('.')[0]),
        "platform": request.user_agent.platform,
        "uas": request.user_agent.string,
        "remember": form["remember"],
        "id": session_id,
    })
    resp = make_response(jsonify(answer="ok"))
    if form["remember"] == "true":
        resp.set_cookie("session_id", session_id, max_age=COOKIE_AGE_REMEMBER)
    else:
        resp.set_cookie(
            "session_id", session_id, max_age=COOKIE_AGE_NOT_REMEMBER
        )
    return resp


@app.route('/api/user/exit', methods=['POST'])
def api_exit():
    session_id = request.cookies.get('session_id')
    db = DigitalLibraryDatabase()
    db.sessions.remove({"id": session_id})
    return jsonify(answer="ok")


def int_checker(count):
    for i in count:
        if not i.isdigit():
            return True
    return False


@app.route('/api/user/add', methods=['POST'])
def api_user_add():
    form = request.form
    db = DigitalLibraryDatabase()
    if db.users.get({"nfc": form["user"]}) is None:
        flag = True
        while flag:
            inviteCode = id_generator(6)
            if db.users.get({"inviteCode": inviteCode}) is None:
                flag = False
        db.users.insert({
            "login": "asd",
            "password": "asd",
            "name": "asd",
            "accessLevel": "Student",
            "nfc": form["user"],
            "inviteCode": inviteCode,
            "status": "off",
            "email": "asd",
            "handed": 0,
            "salt": "asd"
        })
        return jsonify(answer="Done", code=inviteCode)
    else:
        return jsonify(answer="fail")

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
    if int_checker(form["count"]):
        return jsonify(answer="fail")
    first = form["author"].split(" ")
    second = []
    for word in first:
        print(second)
        second += word.split(".")
    second = sorted(second, key=lambda s: len(s), reverse=True)

    third = form["title"].split(" ")
    fourth = []
    for word in third:
        print(fourth)
        fourth += word.split(".")
    fourth = sorted(fourth, key=lambda s: len(s), reverse=True)

    book = {
        "title": form["title"],
        "author": form["author"],
        "count": int(form["count"]),
        "barcode": form["code"],
        "search_title": form["title"].replace(" ", "").lower(),
    }
    for i in range(4):
        if len(second) == i:
            break
        if len(second[i]) != 1 and len(second[i]) != 0:
            book[str(i) + "_author"] = second[i].lower()
    for i in range(4):
        if len(fourth) == i:
            break
        if len(fourth[i]) != 1 and len(fourth[i]) != 0:
            book[str(i) + "_title"] = fourth[i].lower()
    print(book)
    db.books.insert(book)
    local_filename, _ = urllib.request.urlretrieve(form["url"])
    Resize(local_filename, "book", form["code"], "jpg")
    return jsonify(answer="ok")


@app.route('/api/user/photo', methods=['POST'])
def api_user_photo():
    form = request.form
    db = DigitalLibraryDatabase()
    if check_permitions(session_id = request.cookies.get('session_id')):
        return jsonify(answer="fail")
    user = db.users.get({"id": form["id"]})
    if user is None:
        return jsonify(answer="ok")
    try:
        local_filename, _ = urllib.request.urlretrieve(form["url"])
    except ValueError:
        return jsonify(answer="fail")
    Resize(local_filename, "user", form["id"], "jpg")
    db.users.update({"id": form["id"]}, {"image": "http://localhost:1303/static/images/user/$-covers/" + form["id"] + ".jpg"})
    return jsonify(answer="ok")


@app.route('/api/book/action', methods=['POST'])
def api_book_action():
    form = request.form
    db = DigitalLibraryDatabase()
    user = db.users.get({"nfc": form["user"]})
    book = db.books.get({"barcode": form["book"]})
    if user is None:
        action = Action.Fail
        return jsonify(action=action.name, book=book)
    if book is None:
        action = Action.Fail
        return jsonify(action=action.name, book=book)
    if db.hands.get({
        "user_nfc": form["user"],
        "book_barcode": book["barcode"],
    }) is not None:
        db.users.update({"nfc": form["user"]}, {"handed": db.users.get({"nfc": form["user"]})["handed"] - 1})
        db.hands.remove({
            "user_nfc": form["user"],
            "book_barcode": book["barcode"],
        })
        action = Action.Return
    else:
        db.users.update({"nfc": form["user"]}, {"handed": db.users.get({"nfc": form["user"]})["handed"] + 1})
        db.hands.insert({
            "user_nfc": form["user"],
            "user_id": user["id"],
            "user_name": user["name"],
            "book_barcode": book["barcode"],
            "book_title": book["title"],
            "book_author": book["author"],
            "datetime": datetime.utcnow(),
        })
        action = Action.Take
    db.handlog.insert({
        "user_nfc": form["user"],
        "book_barcode": book["barcode"],
        "datetime": datetime.utcnow(),
        "action": action.name,
        "action_ru_name": (
            "Взял"
            if action.name == "Take"
            else "Вернул"
        ),
        "datetime_str": str(datetime.utcnow())[:-7],
        "book_title": book["title"],
        "user_name": user["name"],
        "user_id": user["id"],
    })
    book["_id"] = ""
    return jsonify(action=action.name, book=book)


@app.route('/api/user/get', methods=['POST'])
def api_user_get():
    form = request.form
    db = DigitalLibraryDatabase()
    results = []
    for word in form["request"].lower().split(" "):
        results += db.users.find({"search_name": word})
        results += db.users.find({"search_s_name": word})
        results += db.users.find({"search_full_name": word})
        results += db.users.find({"search_r_full_name": word})
    results += db.users.find({"search_full_name": form["request"].lower().replace(" ", "")})
    results += db.users.find({"search_r_full_name": form["request"].lower().replace(" ", "")})
    public_results = []
    for i in results:
        new_reqult = {
            "handed": i["handed"],
            "name": i["name"],
            "id": i["id"],
        }
        if i.get("image") is None:
            new_reqult["image"] = "https://en.opensuse.org/images/0/0b/Icon-user.png"
        else:
            new_reqult["image"] = i["image"].replace("$", "small")
        if not (new_reqult in public_results):
            public_results += [new_reqult]
    return jsonify(results=public_results)


@app.route('/api/user/up', methods=['POST'])
def api_user_up():
    if check_permitions(session_id = request.cookies.get('session_id')):
        return jsonify(answer="fail")
    form = request.form
    db = DigitalLibraryDatabase()
    user = db.users.get({"id": form["id"]})
    if user is None:
        return jsonify(answer="fail")
    else:
        db.users.update({"id": form["id"]}, {"accessLevel": "Librarian"})
        return jsonify(answer="ok")


@app.route('/api/user/down', methods=['POST'])
def api_user_down():
    if check_permitions(session_id = request.cookies.get('session_id')):
        return jsonify(answer="fail")
    form = request.form
    db = DigitalLibraryDatabase()
    user = db.users.get({"id": form["id"]})
    if user is None:
        return jsonify(answer="fail")
    else:
        db.users.update({"id": form["id"]}, {"accessLevel": "Student"})
        return jsonify(answer="ok")


@app.route('/api/book/get', methods=['POST'])
def api_book_get():
    form = request.form
    db = DigitalLibraryDatabase()
    results = []
    print(form["request"])
    for word in form["request"].lower().split(" "):
        results += db.books.find({"search_title": word})
        results += db.books.find({"title": word})
        results += db.books.find({"author": word})
        results += db.books.find({"0_title": word})
        results += db.books.find({"1_title": word})
        results += db.books.find({"2_title": word})
        results += db.books.find({"3_title": word})
        results += db.books.find({"0_author": word})
        results += db.books.find({"1_author": word})
        results += db.books.find({"2_author": word})
        results += db.books.find({"3_author": word})
        results += db.books.find({"search_r_full_name": word})
    results += db.books.find({"search_title": form["request"].lower().replace(" ", "")})
    results += db.books.find({"title": form["request"].lower().replace(" ", "")})
    public_results = []
    for i in results:
        new_reqult = {
            "title": i["title"],
            "author": i["author"],
            "barcode": i["barcode"],
        }
        new_reqult["image"] = "http://localhost:1303/static/images/book/small-covers/" + i["barcode"] + ".jpg"
        if not (new_reqult in public_results):
            public_results += [new_reqult]
    return jsonify(results=public_results)


def render_template(page_name, user):
    if page_name in ["login", "registration"]:
        return flask.render_template(page_name + ".html")
    if user is None:
        return redirect("/login")
    config = load_config()
    db = DigitalLibraryDatabase()
    page_context = {"user": user}
    if user["accessLevel"] == AccessLevel.Student.name:
        if page_name not in config["student_pages"]:
            return redirect("/handed")
        else:
            handed = db.hands.find({"user_nfc": user["nfc"]})
            page_context = {
                "user": user,
                "handed": handed,
                "len": len(handed),
                "page_name": page_name,
            }
    elif user["accessLevel"] == AccessLevel.Librarian.name:
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
                    been_handed_days = (
                        (datetime.utcnow() - hand["datetime"]).days
                    )
                    for book in books:
                        if book["barcode"] == hand["book_barcode"]:
                            flag = False
                            book["handed"] += 1
                            if book["old_datetime"] < been_handed_days:
                                book["old_datetime"] = been_handed_days
                                book["old_owner_nfc"] = hand["user_nfc"]
                                book["old_owner_id"] = hand["user_id"]
                                book["old_owner_name"] = hand["user_name"]
                    if flag:
                        book = db.books.get({"barcode": hand["book_barcode"]})
                        new_book = {
                            "barcode": book["barcode"],
                            "title": book["title"],
                            "author": book["author"],
                            "old_datetime": been_handed_days,
                            "old_owner_id": hand["user_id"],
                            "old_owner_name": hand["user_name"],
                            "handed": 1,
                            "count": book["count"]
                        }
                        books += [new_book]
                page_context = {
                    "user": user,
                    "page_name": page_name,
                    "len": len(books[:10]),
                    "books": books[:10],
                }
    return flask.render_template(page_name + '.html', **dict(**page_context))


class Session:
    __slots__ = []


@app.route("/books/<barcode>")
def book_barcode(barcode):
    db = DigitalLibraryDatabase()
    session_id = request.cookies.get('session_id')
    session = db.sessions.get({"id": session_id})
    if session is None:
        return redirect("/login")
    else:
        if (datetime.utcnow() - session["datetime"]).days > 7:
            db.sessions.remove(session)
            return redirect("/login")
        if fields_are(session, {
            "ip" : str(request.remote_addr),
            "browser" : request.user_agent.browser,
            "version" : (
                request.user_agent.version
                and int(request.user_agent.version.split('.')[0])
            ),
            "platform" : request.user_agent.platform,
            "uas" : request.user_agent.string,
        }):
            # start
            book = db.books.get({"barcode": barcode})
            if book is None:
                return redirect("/handed")
            s_book = {
                "title" : book["title"],
                "author": book["author"],
                "count": book["count"],
                "barcode": book["barcode"],
            }
            hands = db.hands.find({"book_barcode": barcode})
            n_users = []
            for hand in hands:
                c_user = db.users.get({"nfc": hand["user_nfc"]})
                if c_user is None:
                    continue
                n_user = {
                    "name": c_user["name"],
                    "id": c_user["id"],
                    "time": (datetime.utcnow() - hand["datetime"]).days,
                }
                n_users += [n_user]
            if hands is None:
                s_book["handed"] = 0
            else:
                s_book["handed"] = len(hands)
            # end
            resp = make_response(flask.render_template(
                "book.html",
                user=db.users.get({"login": session["user_login"]}),
                book=s_book,
                users=n_users,
            ))
            resp.set_cookie(
                "session_id",
                session_id,
                max_age = (
                    COOKIE_AGE_REMEMBER
                    if session["remember"] == "true"
                    else COOKIE_AGE_NOT_REMEMBER
                ),
            )
            return resp
        else:
            return redirect("/login")



@app.route("/users/<user_id>")
def user_page(user_id):
    db = DigitalLibraryDatabase()
    s_user = db.users.get({"id": user_id})
    if check_permitions(request.cookies.get('session_id')):
        return redirect('/login')
    if s_user is None:
        return flask.render_template("404.html")
    ss_user = {}
    ss_user["handed"] = len(db.hands.find({"user_id": user_id}))
    ss_user["name"] = s_user["name"]
    ss_user["id"] = s_user["id"]
    ss_user["accessLevel"] = s_user["accessLevel"]
    if s_user.get("image") is None:
        ss_user["image"] = "https://en.opensuse.org/images/0/0b/Icon-user.png"
    else:
        ss_user["image"] = s_user["image"].replace("$", "large")
    session_id = request.cookies.get('session_id')
    session = db.sessions.get({"id": session_id})
    if session is None:
        return redirect("/login")
    else:
        if (datetime.utcnow() - session["datetime"]).days > 7:
            db.sessions.remove(session)
            return redirect("/login")
        if fields_are(session, {
            "ip" : str(request.remote_addr),
            "browser" : request.user_agent.browser,
            "version" : (
                request.user_agent.version
                and int(request.user_agent.version.split('.')[0])
            ),
            "platform" : request.user_agent.platform,
            "uas" : request.user_agent.string,
        }):
            user = db.users.get({"login": session["user_login"]})
            db.sessions.update(session, {"@set": {"datetime": datetime.utcnow()}})
            s_user_hands = db.hands.find({"user_nfc": s_user["nfc"]})
            s_user_books = []
            for hand in s_user_hands:
                n_book = {}
                n_book["title"] = hand["book_title"]
                n_book["author"] = hand["book_author"]
                n_book["barcode"] = hand["book_barcode"]
                n_book["days"] = (datetime.utcnow() - hand["datetime"]).days
                s_user_books += [n_book]
            ss_user["books"] = s_user_books
            resp = make_response(flask.render_template(
                "user.html",
                user=user,
                s_user=ss_user
            ))
            resp.set_cookie(
                "session_id",
                session_id,
                max_age = (
                    COOKIE_AGE_REMEMBER
                    if session["remember"] == "true"
                    else COOKIE_AGE_NOT_REMEMBER
                ),
            )
            return resp


@app.route("/<page_name>")
def cookie_check(page_name):
    config = load_config()
    if page_name not in config["pages"]:
        return flask.render_template("404.html")
    db = DigitalLibraryDatabase()
    session_id = request.cookies.get('session_id')
    session = db.sessions.get({"id": session_id})
    if session is None:
        if page_name == "registration":
            return flask.render_template("registration.html")
        if page_name == "login":
            return render_template("login", None)
        return redirect("/login")
    else:
        if (datetime.utcnow() - session["datetime"]).days > 7:
            db.sessions.remove(session)
            return redirect("/login")
        if fields_are(session, {
            "ip" : str(request.remote_addr),
            "browser" : request.user_agent.browser,
            "version" : (
                request.user_agent.version
                and int(request.user_agent.version.split('.')[0])
            ),
            "platform" : request.user_agent.platform,
            "uas" : request.user_agent.string,
        }):
            user = db.users.get({"login": session["user_login"]})
            session["datetime"] = datetime.utcnow()
            if page_name in ["login", "registration"]:
                return redirect("/handed")
            resp = make_response(render_template(page_name, user))
            resp.set_cookie(
                "session_id",
                session_id,
                max_age = (
                    COOKIE_AGE_REMEMBER
                    if session["remember"] == "true"
                    else COOKIE_AGE_NOT_REMEMBER
                ),
            )
            return resp


def main():
    config = load_config()
    app.run(host=config["host"], debug=True, port=int(config["port"]))


if __name__ == '__main__':
    main()
