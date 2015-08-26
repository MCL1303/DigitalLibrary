from threading import Thread
import pymongo

db = MongoClient().digital_library

def hour_users():
	db.oneHourUsers = db.users
	sleep(3600)
	hour_users()
		