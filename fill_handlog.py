#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from digital_library.database import Database
from datetime import datetime, timedelta
from bson.objectid import ObjectId
from random import random
import configparser


def load_config(header):
	config = configparser.ConfigParser()
	config.read('config')
	return config[header]


def fill():
	user_names = [
		'Иван',
		'Петр',
		"Владимир",
		"Игорь",
		"Юрий",
		"Алексей",
		"Данила",
		"Павел",
		"Даниил",
	]
	user_s_names = [
		'Иванов',
		'Петров',
		'Сидоров',
		'Иваненков',
		'Мишин',
		'Лебедев',
		'Андреев',
		'Болычевцев',
		'Семенов',
		'Тараканов',
		'Старостин',
		'Федоров',
		'Старунов',
	]
	books = [
		'Война и мир',
		'Преступление и наказание',
		'Толстый и тонкий',
		'Вини пух и все все все',
		'Алгоритмы. Построение и анализ',
		'Что такое математика?',
		'Гарри потер и философский камень',
		'Матемматические регаты',
		'История 9 класс',
		'Задачник по математике',
	]
	actions = ['Взял', 'Вернул']
	config = load_config('Server')
	db = Database(config['database_name'], ['handlog', 'books'])
	books = db.books.find({})
	len_books = len(books)
	for i in range(1000):
		book = books[int(random() * len_books * 10) % len_books]
		db.handlog.insert({
			'user_name': user_names[int(random() * 200) % len(user_names)] + ' ' + user_s_names[int(random() * 200) % len(user_s_names)],
			'book_title': book['title'],
			'datetime': (datetime.utcnow() + timedelta(minutes=int(random() * 200))),
			'user_id': 'asdasd',
			'book_id': str(book['_id']),
			'action': actions[int((random() * 10) % 2)]
		})