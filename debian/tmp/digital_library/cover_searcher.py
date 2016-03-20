import requests


def get_cover(isbn):
	page = requests.get('http://www.isbnsearch.org/isbn/' + isbn)
	first = page.text.find('<img src="')
	if first == -1:
		return None
	first += 10
	second = -1
	for i in range(first, len(page.text)):
		if page.text[i] == '"':
			second = i
			break
	if second == -1:
		return None
	print(isbn, end=' ')
	return page.text[first:second]