from digital_library.database import Database
from datetime import datetime, timedelta
from bson.objectid import ObjectId

def main():
	users = [
		{'nfc': '66D6A16140CE34', 'name': 'Егор Марков', 'priority': 'student', 'direction': 'Хим', 'personality': 'егор марков 10 хим', 'handed': 0, 'image': '/user.png', 'invite': 'LFLBGIFR', 'visible': 'on', 'number': 10, 'status': 'off'},
		{'nfc': '76506B4155C434', 'name': 'Жанна Александрова', 'priority': 'student', 'direction': 'Физмат', 'personality': 'жанна александрова 10 физ мат', 'handed': 0, 'image': '/user.png', 'invite': 'ORIESMEI', 'visible': 'on', 'number': 10, 'status': 'off'},
		{'nfc': '7619C1C9ACDF34', 'name': 'Евгений Герасимов', 'priority': 'student', 'direction': 'Хим', 'personality': 'евгений герасимов 10 хим', 'handed': 0, 'image': '/user.png', 'invite': 'TRMKQGFA', 'visible': 'on', 'number': 10, 'status': 'off'},
		{'nfc': '767644B9ACDF34', 'name': 'Татьяна Карпова', 'priority': 'student', 'direction': 'Физмат','personality': 'татьяна карпова 10 физ мат', 'handed': 0, 'image': '/user.png', 'invite': 'LPICODKA', 'visible': 'on', 'number': 10, 'status': 'off'},
		{'nfc': '668FFB3185C234', 'name': 'Софья Аксенова', 'priority': 'student', 'direction': 'Хим', 'personality': 'софья аксенова 10 хим', 'handed': 0, 'image': '/user.png', 'invite': 'BJFAJIKS', 'visible': 'on', 'number': 10, 'status': 'off'},
		{'direction': 'Физмат',
		  'email': 'asd@mail.com',
		  'handed': 0,
		  'image': '/user.png',
		  'invite': 'ASDASDAS',
		  'login': 'admin',
		  'name': 'Админ Админов',
		  'nfc': 'ADMIN',
		  'password': b'z\xb2\xb6oD\x00O\xdd\x1e\x11\x985\xc5\xe3*B\xf7O#\xac\xe1~VA:n\xeb\x06\xa3\x1c\x8e\x1c\x8f\xf0\xd4 \x81a\x92o\x9f\xeb\x82Q\xbe%\x8f\x8dG\xd5\xa1\x03\xad\x96p\x19+\xc7\xc2c\x1fqg\xa8',
		  'personality': '',
		  'priority': 'librarian',
		  'salt': 'B0\x0b;g.L>\\7of7,N>m=tY3=-!-WjH#{Zg-E9\nK:xf~-X94u\x0b>Xf\r\x0cogFoF_,M\\B!O',
		  'status': 'on',
		  'visible': 'off',
		 },
		 {'direction': 'Физмат',
		  'email': 'qwe@mail.com',
		  'handed': 0,
		  'image': '/user.png',
		  'invite': 'ASDASDAS',
		  'login': 'terminal',
		  'name': 'Терминал Терминалов',
		  'password': b'y\'AY\x96\x93\xb2_(T:\xd9\xe9\xb6\x8a\x8c\xd3\x03\xc4=b\xe2 \x0b\x9d\xd8"W\x99J\xed3\xd85\x8f\x8a\x99\xa4\x02w\xdc\n8\xe8\xe5\xd4\xb9\xff\xec3\x99h\xfdi\xc6\x1c\x1e6\xfc\xd5Rq\r\x18',
		  'personality': '',
		  'priority': 'terminal',
		  'salt': '\rwEo<gTVe}{?g?V[/W/85`W /\rwq)7s^5o %@h@ZTunm0`t.*!3618.APu \t8p}=',
		  'status': 'on',
		  'visible': 'off',
		  }
	]
	db = Database('digital_library', ['users'])
	for user in users:
		db.users.insert(user)


if __name__ == '__main__':
	main()