#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from digital_library.sessions import add_session, remove_session, session_user
from flask import Flask, jsonify, request, redirect, make_response, send_from_directory
from bson.objectid import ObjectId
from digital_library.database import Database
import configparser


app = Flask(__name__, static_url_path='')


def load_config(header):
	config = configparser.ConfigParser()
	config.read('config')
	return config[header]


def remove_cookie():
	resp = make_response(redirect('/login'))
	resp.set_cookie('session_id', '',)
	return resp


@app.route('/')
def root():
	session_id = request.cookies.get('session_id')
	config = load_config('Server')
	db = Database(config['database_name'], ['users', 'sessions'])
	print('asd')
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
	return send_from_directory('static', 'librarian.html')


def main():
	config = load_config('Server')
	app.run(host=config['host'], port=int(config['port']))


if __name__ == '__main__':
	main()

