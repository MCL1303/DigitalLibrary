#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from flask import Flask, jsonify, request, redirect, make_response, send_from_directory
from digital_library.sessions import add_session, remove_session, session_user
from digital_library.database import Database
from digital_library import validators
from bson.objectid import ObjectId
from hashlib import sha512
import configparser
import random
import string


app = Flask(__name__, static_url_path='')
HASH = sha512


def load_config(header):
	config = configparser.ConfigParser()
	config.read('config')
	return config[header]


def remove_cookie():
	resp = make_response(redirect('/login'))
	resp.set_cookie('session_id', '',)
	return resp


def hash(password, salt):
    h = (password + salt).encode()
    for _ in range(1024):
        h = HASH(h).digest()
    return h


@app.route('/api/form/email', methods=['POST'])
def api_form_email():
	config = load_config('Server')
	db = Database(config['database_name'], ['users'])
	form = request.get_json()
	user = db.users.get({'email': form['email']})
	if user is None:
		return jsonify(answer='unused')
	else:
		return jsonify(answer='used')


@app.route('/api/form/login', methods=['POST'])
def api_form_login():
	config = load_config('Server')
	db = Database(config['database_name'], ['users'])
	form = request.get_json()
	user = db.users.get({'login': form['login']})
	if user is None:
		return jsonify(answer='unused')
	else:
		return jsonify(answer='used')


@app.route('/api/form/invite', methods=['POST'])
def api_form_invite():
	config = load_config('Server')
	db = Database(config['database_name'], ['users'])
	form = request.get_json()
	user = db.users.get({'invite': form['invite'].upper(), 'status': 'off'})
	if user is not None:
		return jsonify(answer='unused')
	else:
		return jsonify(answer='used')


@app.route('/api/user/signup', methods=['POST'])
def api_user_signup():
	config = load_config('Server')
	db = Database(config['database_name'], ['users', 'sessions'])
	form = request.get_json()
	if not validators.email(form['email']):
		return jsonify(answer='fail')
	if not validators.login(form['login']):
		return jsonify(answer='fail')
	if not validators.password(form['password']):
		return jsonify(answer='fail')
	user = db.users.get({'invite': form['invite'].upper(), 'status': 'off'})
	if user is None:
		return jsonify(answer='fail')
	salt = "".join(
		random.choice(string.printable) for _ in range(HASH().digest_size)
	) 
	print(form['password'], salt)
	db.users.update(user, {
		'salt': salt,
		'status': 'on',
		'email': form['email'],
		'login': form['login'],
		'password': hash(form['password'], salt),
	})
	session_id = add_session(request, 'no_remember', str(user['_id']))
	return jsonify(answer='ok', session_id=session_id)


@app.route('/api/user/signin', methods=['POST'])
def api_user_signin():
	config = load_config('Server')
	db = Database(config['database_name'], ['users', 'sessions'])
	form = request.get_json()
	user = db.users.get({
		'login': form['login']
	})
	if user is None:
		print('1')
		return jsonify(answer='fail')
	if user['password'] != hash(form['password'], user['salt']):
		print('2')
		return jsonify(answer='fail')
	print(form['password'], user['salt'])
	if form['remember']:
		remember = 'remember'
	else:
		remember = 'no_remember'
	session_id = add_session(request, remember, str(user['_id']))
	return jsonify(answer='ok', session_id=session_id)


@app.route('/api/info/user', methods=['POST'])
def api_info_user():
	config = load_config('Server')
	db = Database(config['database_name'], ['users', 'sessions'])
	try:
		session = db.sessions.get({
			'_id': ObjectId(request.cookies.get('session_id'))
		})
	except:
		return jsonify(answer='fail')
	if session is None:
		return jsonify(answer='fail')
	try:
		user = db.users.get({
			'_id': ObjectId(session['user'])
		})
	except:
		return jsonify(answer='fail')
	public_user = {
		'name': user['name'],
		'handed': user['handed']
	}
	# print(jsonify(answer='ok', user=public_user))
	return jsonify(answer='ok', user=public_user)


@app.route('/')
def root():
	session_id = request.cookies.get('session_id')
	config = load_config('Server')
	db = Database(config['database_name'], ['users', 'sessions'])
	try:
		session_object_id = ObjectId(session_id)
	except:
		return remove_cookie()
	session = db.sessions.get({'_id': session_object_id})
	if session is None:
		return remove_cookie()
	user_id = session['user']
	try:
		user_object_id = ObjectId(user_id)
	except:
		return remove_session()
	user = db.users.get({'_id': ObjectId(user_id)})
	if user is None:
		return remove_cookie()
	if user['priority'] == 'librarian':
		return send_from_directory('static', 'librarian.html')
	else:
		return send_from_directory('static', 'student.html') 


@app.route('/login')
def login():
	return send_from_directory('static', 'auth.html')


def main():
	config = load_config('Server')
	app.run(host=config['host'], port=int(config['port']))


if __name__ == '__main__':
	main()

