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
        return self._collection.insert(query)

    def get(self, query):
        return self._collection.find_one(query)

    def find(self, query):
        return list(self._collection.find(query))

    def get_page(self, query, page):
        return list(self._collection.find(query).sort([('datetime', -1)]).skip((int(page) - 1) * 30).limit(30))

    def remove(self, query):
        self._collection.remove(query)

    def update(self, query, update):
        self._collection.update(query, {"$set": update})

    def search(self, field, request, page):
        return list(self._collection.find({field: {'$regex': request}, 'visible': 'on'}).skip((int(page) - 1) * 30).limit(30))


class Database(DB):
    def __init__(self, db, collections):
        super().__init__(db)
        for collection in collections:
            setattr(self, collection, Collection(self, collection))
