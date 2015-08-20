from digital_library.types import Action

from datetime import datetime
from pymongo import MongoClient


class Database:
    # pylint: disable=too-few-public-methods

    def __init__(self, name: str):
        self._db = MongoClient()[name]

    def __getitem__(self, collection_name: str):
        return self._db[collection_name]


class Collection:
    # pylint: disable=too-few-public-methods

    def __init__(self, db: Database, name: str):
        self._collection = db[name]

    def _insert(self, doc):
        return self._collection.insert(doc)

    def _find_one(self, query):
        return self._collection.find_one(query)

    def _remove(self, query):
        return self._collection.remove(query)


class Terminals(Collection):
    def __init__(self, db):
        super().__init__(db, 'terminals')

    def add(self, client_ip, terminal_uuid):
        self._insert({"ip": client_ip, "uuid": str(terminal_uuid)})

    def get(self, client_ip):
        return self._find_one({'ip': client_ip})


class Hands(Collection):
    def __init__(self, db):
        super().__init__(db, 'hands')

    def add(self, user, book):
        now = datetime.utcnow()
        self._insert({
            "user": user,
            "book": book,
            "datetime": now,
        })

    def get(self, user, book):
        return self._find_one({'user': user, 'book': book})

    def exists(self, user, book):
        return self.get(user, book) is not None

    def delete(self, user, book):
        self._remove({"user": user, "book": book})


class HandLog(Collection):
    # pylint: disable=too-few-public-methods

    def __init__(self, db):
        super().__init__(db, 'handlog')

    def log(self, action: Action, user, book):
        now = datetime.utcnow()
        self._insert({
            "action": action.name,
            "user": user,
            "book": book,
            "datetime": now,
        })


class DigitalLibraryDatabase(Database):
    # pylint: disable=too-few-public-methods

    def __init__(self):
        super().__init__('digital_library')
        self.terminals = Terminals(self)
        self.hands = Hands(self)
        self.handlog = HandLog(self)
