from pymongo import MongoClient


class DB:
    # pylint: disable=too-few-public-methods
    def __init__(self, name: str):
        self._db = MongoClient()[name]

    def __getitem__(self, collection_name: str):
        return self._db[collection_name]


class Collection:
    def __init__(self, db: DB, name: str):
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


class Database(DB):
    def __init__(self, db, collections):
        super().__init__(db)
        for collection in collections:
            setattr(self, collection, Collection(self, collection))
