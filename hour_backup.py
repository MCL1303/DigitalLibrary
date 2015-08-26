import pymongo

base = pymongo.MongoClient()

base.twoHour = base.oneHour
base.oneHour = base.digital_library