#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from digital_library.database import Database
from bson.objectid import ObjectId
from json import load


def fill():
	books = load(open('books'))
	db = Database('digital_library', ['books'])
	for book in books:
		db.books.insert(book)
