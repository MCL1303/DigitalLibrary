import pymongo
from datetime import datetime


class Collection():
    pass


class Terminals(Collection):
    def __init__(self):
        super().__init__()
        self.db = pymongo.MongoClient().digital_library

    def add(self, client_ip, terminal_uuid):
        self.db.terminals.insert({"ip": client_ip, "uuid": str(terminal_uuid)})

    def get(self, client_ip):
        return self.db.terminals.find_one({'ip': client_ip})


class Hands(Collection):
    def __init__(self):
        super().__init__()
        self.db = pymongo.MongoClient().digital_library

    def add(self, user, book):
        now = datetime.utcnow()
        self.db.hands.insert({
            "user": user,
            "book": book,
            "datetime": now,
        })

    def get(self, user, book):
        return self.db.hands.find_one({'user': user, 'book': book})

    def exists(self, user, book):
        return self.get(user, book) is not None

    def delete(self, user, book):
        self.db.hands.remove({"user": user, "book": book})


class Handlog(Collection):
    def __init__(self):
        super().__init__()
        self.db = pymongo.MongoClient().digital_library

    def add(self, user, book, event):
        assert event in {'take', 'return'}
        now = datetime.utcnow()
        self.db.handlog.insert({
            "user": user,
            "book": book,
            "datetime": now,
            "event": event,
        })


class Database:
    def __init__(self):
        super().__init__()
        self.terminals = Terminals()
        self.hands = Hands()
        self.handlog = Handlog()