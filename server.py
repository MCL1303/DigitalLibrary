#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from digital_library.sessions import add_session, remove_session, session_user, session_priority
from flask import Flask, jsonify, request, redirect, make_response, send_from_directory
from digital_library.database import Database
from digital_library import validators
from bson.objectid import ObjectId
from datetime import datetime
from hashlib import sha512
import fill_handlog
import configparser
import fill_books
import fill_users
import random
import string


app = Flask(__name__, static_url_path='')
HASH = sha512


def load_config(header):
	config = configparser.ConfigParser()
	config.read('config')
	return config[header]


def remove_cookie():
	resp = make_response(redirect('/signin'))
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
		return jsonify(answer='fail')
	if user['password'] != hash(form['password'], user['salt']):
		return jsonify(answer='fail')
	if form['remember']:
		remember = 'remember'
	else:
		remember = 'no_remember'
	session_id = add_session(request, remember, str(user['_id']))
	return jsonify(answer='ok', session_id=session_id)


@app.route('/api/info/init_user', methods=['POST'])
def api_info_init_user():
	if session_priority(request.cookies.get('session_id')) is None:
		return jsonify(answer='fail')
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
	return jsonify(answer='ok', user=public_user)


@app.route('/api/books/search', methods=['POST'])
def api_books_search():
	if session_priority(request.cookies.get('session_id')) is None:
		return jsonify(answer='fail')
	config = load_config('Server')
	db = Database(config['database_name'], ['books'])
	form = request.get_json()
	user_request = form['request'].split(' ')
	results = []
	used = set()
	for word in user_request:
		sub_results = db.books.search('personality', word.lower(), int(form['page']))
		for result in sub_results:
			if str(result['_id']) not in used:
				used.add(str(result['_id']))
				result['_id'] = str(result['_id'])
				results += [result]
	return jsonify(answer='ok', results=results)


@app.route('/api/users/search', methods=['POST'])
def api_users_search():
	if session_priority(request.cookies.get('session_id')) is None:
		return jsonify(answer='fail')
	config = load_config('Server')
	db = Database(config['database_name'], ['users'])
	form = request.get_json()
	user_request = form['request'].split(' ')
	results = []
	used = set()
	for word in user_request:
		sub_results = db.users.search('personality', word.lower(), int(form['page']))
		for result in sub_results:
			if str(result['_id']) not in used:
				used.add(str(result['_id']))
				result['_id'] = str(result['_id'])
				result['login'] = ''
				result['password'] = ''
				result['invite'] = ''
				result['nfc'] = ''
				results += [result]
	return jsonify(answer='ok', results=results)


@app.route('/api/info/user', methods=['POST'])
def api_info_user():
	if session_priority(request.cookies.get('session_id')) != 'librarian':
		return jsonify(answer='fail')
	config = load_config('Server')
	db = Database(config['database_name'], ['users', 'hands'])
	form = request.get_json()
	try:
		user = db.users.get({
			'_id': ObjectId(form['user'])
		})
	except:
		return jsonify(answer='fail')
	hands = db.hands.find({'user_id': str(user['_id'])})
	for hand in hands:
		hand['_id'] = ''
	public_user = {
		'name': user['name'],
		'handed': user['handed'],
		'books': hands,
		'image': user['image']
	}
	return jsonify(answer='ok', user=public_user)


@app.route('/api/info/handlog', methods=['POST'])
def api_info_handlog():
	config = load_config('Server')
	db = Database(config['database_name'], ['handlog'])
	if session_priority(request.cookies.get('session_id')) != 'librarian':
		return jsonify(answer='fail')
	form = request.get_json()
	try:
		page = db.handlog.get_page({}, int(form['page']))
		for line in page:
			line['book_id'] = str(line['book_id'])
			line['_id'] = ''
		return jsonify(answer='ok', page=page, more=(len(page) == 30))
	except:
		return jsonify(answer='fail') 



@app.route('/api/info/book', methods=['POST'])
def api_info_book():
	config = load_config('Server')
	db = Database(config['database_name'], ['books', 'hands'])
	if session_priority(request.cookies.get('session_id')) is None:
		return jsonify(answer='fail')
	form = request.get_json()
	try:
		book = db.books.get({
			'_id': ObjectId(form['book'])
		})
		if book is None:
			return jsonify(answer='not_found')
		handers = db.hands.find({'book_id': str(book['_id'])})
		book['_id'] = ''
		for hander in handers:
			hander['_id'] = ''
		book['handers'] = handers
		return jsonify(answer='ok', book=book)
	except:
		return jsonify(answer='fail')


@app.route('/api/info/handed', methods=['POST'])
def api_info_handed():
	config = load_config('Server')
	db = Database(config['database_name'], ['books', 'hands'])
	if session_priority(request.cookies.get('session_id')) is None:
		return jsonify(answer='fail')
	form = request.get_json()
	try:
		hands = db.hands.find({'user_id': session_user(request.cookies.get('session_id'))})
		for hand in hands:
			hand['_id'] = ''
		return jsonify(answer='ok', results=hands)
	except:
		return jsonify(answer='fail')


@app.route('/api/operations', methods=['POST'])
def api_operations():
	config = load_config('Server')
	db = Database(config['database_name'], ['books', 'hands', 'users', 'handlog'])
	if session_priority(request.cookies.get('session_id')) != 'terminal':
		return jsonify(answer='fail')
	form = request.get_json()
	book = db.books.get({'code': form['book']})
	if book is None:
		book = {
			'image': '/book_not_found.jpg',
			'title': 'Книга не найдена',
		}
		return jsonify(answer='book_not_found', book=book)
	try:
		user = db.users.get({'_id': ObjectId(form['user'])})
	except:
		return jsonify(answer='user_not_found')
	if user is None:
		return jsonify(answer='user_not_found')
	old_hand = db.hands.get({'book_code': book['code'], 'user_id': str(user['_id'])})
	if old_hand is None:
		if book['handed'] >= book['count']:
			book = {
				'image': '/book_not_found.jpg',
				'title': 'Книга не найдена',
			}
			return jsonify(answer='book_not_found', book=book) 
		db.handlog.insert({
			'book_id': str(book['_id']),
			'user_name': user['name'],
			'user_id': str(user['_id']),
			'book_title': book['title'],
			'action': 'Взял',
			'datetime': datetime.utcnow(),
		})
		db.hands.insert({
			'user_image': user['image'],
			'user_name': user['name'],
			'book_id': str(book['_id']),
			'user_id': str(user['_id']),
			'book_id': str(book['_id']),
			'book_code': book['code'],
			'user_nfc': user['nfc'],
			'datetime': datetime.utcnow(),
			'book_image': book['image'],
			'book_title': book['title'],
		})
		db.users.update(user, {'handed': user['handed'] + 1})
		db.books.update(book, {'handed': book['handed'] + 1})
		librarian = db.users.get({'priority': 'librarian'})
		try:
			db.users.update({'priority': 'librarian'}, {'handed': librarian['handed'] + 1})
		except:
			pass
		book['_id'] = ''
		return jsonify(answer='ok', action='взяли', book=book)
	else:
		db.handlog.insert({
			'book_id': str(book['_id']),
			'user_name': user['name'],
			'user_id': str(user['_id']),
			'book_title': book['title'],
			'action': 'Вернул',
			'datetime': datetime.utcnow(),
		})
		db.hands.remove({'user_id': str(user['_id']), 'book_id': str(book['_id'])})
		db.users.update(user, {'handed': user['handed'] - 1})
		db.books.update(book, {'handed': book['handed'] - 1})
		librarian = db.users.get({'priority': 'librarian'})
		try:
			db.users.update({'priority': 'librarian'}, {'handed': librarian['handed'] - 1})
		except:
			pass
		book['_id'] = ''
		return jsonify(answer='ok', action='вернули', book=book)


@app.route('/api/terminal/user', methods=['POST'])
def api_terminal_user():
	config = load_config('Server')
	db = Database(config['database_name'], ['users'])
	if session_priority(request.cookies.get('session_id')) != 'terminal':
		return jsonify(answer='fail')
	form = request.get_json()
	user = db.users.get({'nfc': form['user']})
	if user is None:
		return jsonify(answer='not_found')
	user['_id'] = str(user['_id'])
	user['password'] = ''
	user['salt'] = ''
	user['login'] = ''
	if user['status'] == 'on':
		return jsonify(answer='active', user=user)
	else:
		return jsonify(answer='inactive', invite=user['invite'], user=user)


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
	user = db.users.get({'_id': user_object_id})
	if user is None:
		return remove_cookie()
	if user['priority'] == 'librarian':
		return send_from_directory('static', 'librarian.html')
	elif user['priority'] == 'student':
		return send_from_directory('static', 'student.html')
	elif user['priority'] == 'terminal':
		return send_from_directory('static', 'terminal.html')


@app.route('/signin')
def signin():
	if session_user(request.cookies.get('session_id')) is not None:
		return redirect('/')
	return send_from_directory('static', 'auth.html')


def main():
	config = load_config('Server')
	db = Database(config['database_name'], ['users', 'books'])
	if len(db.books.get_page({}, 1)) == 0:
		fill_books.fill()
	if len(db.users.get_page({}, 1)) == 0:
		fill_users.fill()
	# if len(db.handlog.get_page({}, 1)) == 0:
	# 	fill_handlog.fill()
	app.run(host=config['host'], port=int(config['port']), debug=True)


if __name__ == '__main__':
	main()

