import re


def email(text):
	if len(text) > 7 and not(" " in text):
		if re.match(
			"^.+\\@(\\[?)[a-zA-Z0-9\\-\\.]+" +
			"\\.([a-zA-Z]{2,3}|[0-9]{1,3})(\\]?)$",
			text
		) != None:
			return True
	return False
	

def login(text):
	for i in text:
		if (not(ord(i) >= ord("!") and ord(i) <= ord("~"))):
			return False
	return True


def password(text):
	if len(text) < 8 or len(text) > 30:
		return False
	alpha = 0
	digit = 0
	char = 0
	for i in text:
		if i.isalpha():
			alpha = 1
		elif i.isdigit():
			digit = 1
		else:
			char = 1
	return (alpha == digit == 1) or (char == digit == 1) or (alpha == char == 1)