#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import authorization
from flask import Flask, jsonify, request, redirect, make_response, send_from_directory
import configparser


app = Flask(__name__, static_url_path='')


def load_config(header):
	config = configparser.ConfigParser()
	config.read('config')
	return config[header]


@app.route('/')
def root():
    return send_from_directory('static', 'librarian.html')


def main():
    config = load_config('Server')
    app.run(host=config["host"], port=int(config["port"]))


if __name__ == '__main__':
    main()

