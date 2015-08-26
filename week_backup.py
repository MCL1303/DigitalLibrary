import pymongo

base = pymongo.MongoClient()

base.twoWeek = base.oneWeek
base.oneWeek = base.digital_library