from digital_library.database import Database
from bson.objectid import ObjectId
import configparser


def load_config(header):
	config = configparser.ConfigParser()
	config.read('config')
	return config[header]


def add_session(request, remember, user):
	config = load_config('Authorization')
	db = Database(config['database_name'], ['sessions'])
	session = {
			"user": user,
			"datetime": datetime.utcnow(),
			"ip": str(request.remote_addr),
			"browser": request.user_agent.browser,
			"version": request.user_agent.version and
			int(request.user_agent.version.split('.')[0]),
			"platform": request.user_agent.platform,
			"uas": request.user_agent.string,
			"remember": remember,
		}
	db.sessions.insert(session)
	return str(db.sessions.get(session)['_id'])


def remove_session(id):
	config = load_config('Authorization')
	db = Database(config['database_name'], ['sessions'])
	db.sessions.remove({'_id': ObjectId('id')})


def session_user(id):
	config = load_config('Authorization')
	db = Database(config['database_name'], ['sessions'])
	session = db.sessions.get({'_id': ObjectId('id')})
	if session is None:
		return None
	if session['remember']:
		if (datetime.utcnow() - session['datetime']).seconds > config['remember']:
			session = db.sessions.remove({'_id': ObjectId('id')})
			return None
		else:
			return session['user']
	else:
		if (datetime.utcnow() - session['datetime']).seconds > config['no_remember']:
			session = db.sessions.remove({'_id': ObjectId('id')})
			return None
		else:
			return session['user']