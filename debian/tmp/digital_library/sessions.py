from digital_library.database import Database
from bson.objectid import ObjectId
from datetime import datetime
import configparser


def load_config(header):
	config = configparser.ConfigParser()
	config.read('config')
	return config[header]


def add_session(request, remember, user):
	config = load_config('Authorization')
	db = Database(config['database_name'], ['sessions'])
	last_session = db.sessions.get({
		'user': user,
		'ip': str(request.remote_addr),
		'browser': request.user_agent.browser,
		'version': request.user_agent.version and
		int(request.user_agent.version.split('.')[0]),
		'platform': request.user_agent.platform,
		'uas': request.user_agent.string,
	})
	if last_session is not None:
		if last_session['remember'] == remember:
			return str(last_session['_id'])
		db.sessions.update(last_session, {'remember': remember})
		return str(last_session['_id'])
	session = {
		'user': user,
		'datetime': datetime.utcnow(),
		'ip': str(request.remote_addr),
		'browser': request.user_agent.browser,
		'version': request.user_agent.version and
		int(request.user_agent.version.split('.')[0]),
		'platform': request.user_agent.platform,
		'uas': request.user_agent.string,
		'remember': remember,
	}
	return str(db.sessions.insert(session))


def remove_session(id):
	config = load_config('Authorization')
	db = Database(config['database_name'], ['sessions'])
	db.sessions.remove({'_id': ObjectId('id')})


def session_user(id):
	config = load_config('Authorization')
	db = Database(config['database_name'], ['sessions'])
	try:
		session = db.sessions.get({'_id': ObjectId(id)})
	except:
		return None
	if session is None:
		return None
	if session['remember']:
		if (datetime.utcnow() - session['datetime']).seconds > int(config['remember']):
			session = db.sessions.remove({'_id': ObjectId('id')})
			return None
		else:
			return session['user']
	else:
		if (datetime.utcnow() - session['datetime']).seconds > int(config['no_remember']):
			session = db.sessions.remove({'_id': ObjectId('id')})
			return None
		else:
			return session['user']


def session_priority(id):
	config = load_config('Authorization')
	db = Database(config['database_name'], ['users'])
	user_id = session_user(id)
	try:
		user = db.users.get({'_id': ObjectId(user_id)})
	except:
		return None
	if user is None:
		return None
	return user.get('priority')

