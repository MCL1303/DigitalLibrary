# Digital Library — a digital book management system
# Copyright (C) 2015  Igor Tarakanov <igortarakanov144999usa@gmail.com>,
#                     Yuriy Syrovetskiy <cblp@cblp.su>
#                     Pavel Fedorov <pfedorovs18@gmail.com>
#                     Danila Starostin <starostindanila@yandex.ru>
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


from pymongo import MongoClient


class Database:
    # pylint: disable=too-few-public-methods
    def __init__(self, name: str):
        self._db = MongoClient()[name]

    def __getitem__(self, collection_name: str):
        return self._db[collection_name]


class Collection:
    def __init__(self, db: Database, name: str):
        self._collection = db[name]

    def insert(self, query):
        self._collection.insert(query)

    def get(self, query):
        return self._collection.find_one(query)

    def find(self, query):
        return list(self._collection.find(query))

    def exists(self, query):
        return self._collection.find_one(query) is not None

    def remove(self, query):
        self._collection.remove(query)

    def update(self, query, update):
        self._collection.update(query, {"$set": update})


class DigitalLibraryDatabase(Database):
    # pylint: disable=too-few-public-methods
    def __init__(self):
        super().__init__('digital_library')
        self.hands = Collection(self, "hands")
        self.handlog = Collection(self, "handlog")
        self.users = Collection(self, "users")
        self.books = Collection(self, "books")
        self.sessions = Collection(self, "sessions")
        self.ips = Collection(self, "ips")
        self.invitations = Collection(self, "invitations")
