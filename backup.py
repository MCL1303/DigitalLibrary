from optparse import OptionParser
import pymongo

base = pymongo.MongoClient()

parser = OptionParser()
parser.add_option("-h")
parser.add_option("-d")
parser.add_option("-w")

(options, args) = parser.parse_args()

if "-h" in options:
	base.twoHour = base.oneHour
	base.oneHour = base.digital_library

if "-d" in options:
	base.twoDay = base.oneDay
	base.oneDay = base.digital_library

if "-w" in options:
	base.twoWeek = base.oneWeek
	base.oneWeek = base.digital_library