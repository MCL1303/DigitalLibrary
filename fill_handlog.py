#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from digital_library.database import Database
from datetime import datetime, timedelta
import configparser
from random import random


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
	db = Database(config['database_name'], ['handlog'])
	for i in range(100):
		db.handlog.insert({
			'student_name': user_names[int(random() * 200) % len(user_names)] + ' ' + user_s_names[int(random() * 200) % len(user_s_names)],
			'book_title': books[int(random() * 200) % len(books)],
			'datetime': (datetime.utcnow() + timedelta(minutes=int(random() * 200))),
			'student_id': 'asdasd',
			'book_id': 'asdasdasd',
			'action': actions[int((random() * 10) % 2)]
		})