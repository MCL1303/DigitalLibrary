#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from digital_library.database import Database
from datetime import datetime, timedelta
from random import random
import configparser
import json


def load_config(header):
	config = configparser.ConfigParser()
	config.read('config')
	return config[header]

def fill():
	data = [
		{
		"image": "/book.png", 
		"title" : "Четырехмерная геометрия", 
		"author" : "Смирнова И.М., Смирнов В.А.", 
		"count" : "1", 
		"code" : "9785940576761", 
		"tags" : ["Стереометрия", "Задачник"], 
		"subject" : "Геометрия", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Энциклопедия аналитических поверхностей", 
		"author" : "Кривошапко С.Н., Иванов В.Н.", 
		"count" : "1", 
		"code" : "9785397009850", 
		"tags" : ["Стереометрия", "Топология", "Аналитические поверхности"], 
		"subject" : "Геометрия", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Геометрия радиолярий", 
		"author" : "Мордухай-Болтовской Д.Д.", 
		"count" : "1", 
		"code" : "9785397030830", 
		"tags" : ["Стереометрия", "Биология", "Многогранники", "Теория групп", "Картинки"], 
		"subject" : "Математическая биология", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Геометрия в картинках", 
		"author" : "Акопян А.", 
		"count" : "1", 
		"code" : "9785940579496", 
		"tags" : ["Планиметрия", "Задачник", "Наглядная геометрия", "Картинки"], 
		"subject" : "Геометрия", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Классические средние в арифметике и геометрии", 
		"author" : "Блинков А.Д.", 
		"count" : "2", 
		"code" : "9785443906034", 
		"tags" : ["Алгебра", "Геометрия", "Теория чисел", "Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Вещественная алгебраическая геометрия", 
		"author" : "Арнольд В.И.", 
		"count" : "1", 
		"code" : "9785940574439", 
		"tags" : ["Алгебра", "Геометрия"], 
		"subject" : "Алгебраическая геометрия", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Афинная и проективная геометрия", 
		"author" : "Понарин Я.П.", 
		"count" : "1", 
		"code" : "9785940574019", 
		"tags" : ["Задачник", "Планиметрия"], 
		"subject" : "Геометрия", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Задачи с изюминкой", 
		"author" : "Тригг Ч.", 
		"count" : "1", 
		"code" : "5030033726", 
		"tags" : ["Задачник", "Олимпиадные задачи", "Геометрия"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Как решать задачу", 
		"author" : "Пойа Д.", 
		"count" : "1", 
		"code" : "9785397012416", 
		"tags" : ["Как решать задачу", "Обучение"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Как решают нестандартные", 
		"author" : "Канель-Белов А.Я., Ковальджи А.К.", 
		"count" : "1", 
		"code" : "9785443903248", 
		"tags" : ["Как решать задачу", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Лингвистические задачи", 
		"author" : "Зализняк А.А.", 
		"count" : "1", 
		"code" : "9785443900940", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Лингвистика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Сборник задача по математике", 
		"author" : "Арлазаров В.В., Татаринцев А.В.", 
		"count" : "1", 
		"code" : "9785382000817", 
		"tags" : ["Задачник", "Школьные задачи", "Алгебра"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Задачи Санкт-Петербургской олимпиады школьников по математике 2002", 
		"author" : "Кохась К.П., Иванов С.В.", 
		"count" : "1", 
		"code" : "9795794001081", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Задачи Санкт-Петербургской олимпиады школьников по математике 2003", 
		"author" : "Кохась К.П., Иванов С.В.", 
		"count" : "1", 
		"code" : "9785794001228", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Простая одержимость", 
		"author" : "Дербишир Дж.", 
		"count" : "1", 
		"code" : "9785271254222", 
		"tags" : ["История", "Теорема", "Личности"], 
		"subject" : "Алгебра", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Математика выборов", 
		"author" : "Клима Р., Ходж Дж.", 
		"count" : "1", 
		"code" : "9785940573173", 
		"tags" : ["Теория вероятностей", "Задачник"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Математический дивертисмент", 
		"author" : "Фукс Д.В., Табачников С.Л.", 
		"count" : "1", 
		"code" : "9785940577317", 
		"tags" : ["Стереометрия", "Лекции", "Алгебра"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Математический аквариум", 
		"author" : "Уфнаровский В.А.", 
		"count" : "1", 
		"code" : "9785443906157", 
		"tags" : ["Лекции", "Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Математическое понимание природы (второе издание)", 
		"author" : "Арнольд В.И.", 
		"count" : "1", 
		"code" : "9785940576747", 
		"tags" : ["Картинки", "Алгебра", "Физика"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Математическое понимание природы (четвертое издание)", 
		"author" : "Арнольд В.И.", 
		"count" : "1", 
		"code" : "9785443900698", 
		"tags" : ["Картинки", "Алгебра", "Физика"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Занимательные задачи", 
		"author" : "Гамов Г., Стерн М.", 
		"count" : "1", 
		"code" : "9785354003556", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Наглядная топология", 
		"author" : "Прасолов В.В.", 
		"count" : "1", 
		"code" : "9785940579038", 
		"tags" : ["Топология", "Планиметрия", "Стереометрия"], 
		"subject" : "Геометрия", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Правильный, полуправильные звездчатые многогранники", 
		"author" : "Смирнов В.А., Смирнова И.М.", 
		"count" : "1", 
		"code" : "9785940576785", 
		"tags" : ["Многогранники", "Стереометрия", "Картинки"], 
		"subject" : "Геометрия", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Задачи по информатика (Туймаада 1994 - 2012)", 
		"author" : "Павлов А.В., Антонов Ю.С.", 
		"count" : "1", 
		"code" : "9785443903187", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Олимпиады по криптографии и математике", 
		"author" : "Зубов А.Ю.", 
		"count" : "2", 
		"code" : "97854439900162", 
		"tags" : ["Олимпиадные задачи", "Задачник", "Криптография"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Готовимся к олимпиадам по математике", 
		"author" : "Фарков А.В.", 
		"count" : "1", 
		"code" : "9785472026529", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Теория графов в занимательных задачах", 
		"author" : "Мельников О.И.", 
		"count" : "1", 
		"code" : "9785397039062", 
		"tags" : ["Теория графов", "Задачник"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Математика : Всероссийские олимпиады", 
		"author" : "Агаханов Н.Х.", 
		"count" : "1", 
		"code" : "9785090171823", 
		"tags" : ["Задачник", "Олимпиданые задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Олимпиада МГУ Ломоносов по математике", 
		"author" : "Бегунц А.В., Бородин А.П.", 
		"count" : "1", 
		"code" : "9785443900599", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Графы и их применение", 
		"author" : "Оре О.", 
		"count" : "1", 
		"code" : "9785382005447", 
		"tags" : ["Теория графов", "Теория множеств"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Взвешивания и алгоритмы: от головоломок к задачам", 
		"author" : "Кноп К.А.", 
		"count" : "1", 
		"code" : "9785940577027", 
		"tags" : ["Олимпиданые задачи", "Задачник"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Делимость и простые числа", 
		"author" : "Сгибнев А.И.", 
		"count" : "1", 
		"code" : "9785443900797", 
		"tags" : ["Олимпиадные задачи", "Задачник", "Теория чисел"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Введние в современную логику", 
		"author" : "Гладкий А.В.", 
		"count" : "1", 
		"code" : "9785397005074", 
		"tags" : ["Логика"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Диаграммы Юнга, плоские разбиеия и знаочередующиеся матрицы", 
		"author" : "Смирнов Е.Ю.", 
		"count" : "1", 
		"code" : "9785443901374", 
		"tags" : ["Алгебра"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Мифы об эволюции человека", 
		"author" : "Соколов А.", 
		"count" : "1", 
		"code" : "9785916714036", 
		"tags" : ["Генетика", "Эволюция", "Мифы"], 
		"subject" : "Биология", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Бабочка и ураган", 
		"author" : "Мадрид К.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Теория вероятностей", "Историю"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Женщины - математики", 
		"author" : "Новарро Х.", 
		"count" : "1", 
		"code" : "978577406826", 
		"tags" : ["Мир математики", "История", "Личности"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Существуют ли неразрешимые проблемы?", 
		"author" : "Ареан Л.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Теорема"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Пока алгебра не разлечит нас", 
		"author" : "Фресан Х.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Алгебра", "История"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Творчество в математике", 
		"author" : "Альберти М.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Теория чисел", "Алгебра"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Экспериментальное наблюдение математических фактов", 
		"author" : "Арнольд В.И.", 
		"count" : "1", 
		"code" : "9785940579014", 
		"tags" : ["Комбинаторика", "Теория вероятностей", "Теорема"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Рассказы о множествах", 
		"author" : "Виленкин Н.Я.", 
		"count" : "1", 
		"code" : "9785443900667", 
		"tags" : ["Задачник", "Теория множеств"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Элегантная математика", 
		"author" : "Жуков А.В.", 
		"count" : "1", 
		"code" : "9785397036221", 
		"tags" : ["Алгебра", "Планиметрия"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Основы дискртеной математики", 
		"author" : "Деза Е.И., Модель Д.Л.", 
		"count" : "1", 
		"code" : "9785397015882", 
		"tags" : ["Теория графов", "Комбинаторика", "Задачник"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Великая физика", 
		"author" : "Пиковер К.", 
		"count" : "1", 
		"code" : "9785996305179", 
		"tags" : ["Энциклопедия", "История"], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Великая медицина", 
		"author" : "Пиковер К.", 
		"count" : "1", 
		"code" : "9785996307289", 
		"tags" : ["Энциклопедия", "История"], 
		"subject" : "Биология", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Мечта об идеальной карте", 
		"author" : "Ибаньес Р.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Картография", "География", "Планиметрия"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Неуловимые идеи и вечные теоремы", 
		"author" : "Наварро Х.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "История", "Теорема"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Абсолютная точность и другие иллюзии", 
		"author" : "Грима П.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Теория вероятностей", "Статистика"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Новый взгляд на мир", 
		"author" : "Бинимелис Басс М.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Фракталы"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Математика, шпионы и хакеры", 
		"author" : "Гомес Ж.", 
		"count" : "1", 
		"code" : "9785977406390", 
		"tags" : ["Мир математики", "Криптография", "История"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Разум, машина и математика", 
		"author" : "Белда И.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Машинное обучение", "История"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Искусство подсчета", 
		"author" : "Руэ Х.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Комбинаторика", "Теория графов"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Бесконечная мозаика", 
		"author" : "Альберти М.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Замощения"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Загадка Ферма", 
		"author" : "Виолант-и-Хольц А.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "История", "Теорема"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Замечательные числа", 
		"author" : "Гарсия дель Сид Л.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "Нумерология"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Математический клуб", 
		"author" : "Курбера Г.", 
		"count" : "1", 
		"code" : "9785977406826", 
		"tags" : ["Мир математики", "История", "Теорема"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Удивительная физика", 
		"author" : "Асламазов Л.Г., Варламов А.А.", 
		"count" : "1", 
		"code" : "978579130096", 
		"tags" : ["", "", ""], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Новый ум короля", 
		"author" : "Пенроуз Р.", 
		"count" : "1", 
		"code" : "978582012667", 
		"tags" : ["Логика", "Разные темы"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Физика (школьный иллюстрированный справочник)", 
		"author" : "Окслед К., Стокли К.", 
		"count" : "1", 
		"code" : "9785257003127", 
		"tags" : ["Картинки", "Справочник"], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Высший замысел", 
		"author" : "Хокинг С., Млодинов Л.", 
		"count" : "1", 
		"code" : "9785367022186", 
		"tags" : ["История", "Разные темы"], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Задачи московских физический олимпиад", 
		"author" : "Буздин А.И., Ильин В.А.", 
		"count" : "1", 
		"code" : "12980", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Физика : всероссиские олимпиады", 
		"author" : "Козел С.М., Слободянин В.П.", 
		"count" : "1", 
		"code" : "9785090194907", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Физика будущего", 
		"author" : "Каку М.", 
		"count" : "1", 
		"code" : "9785916713381", 
		"tags" : ["История", "Будущее", "Разные темы"], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Физика невозможного", 
		"author" : "Каку М.", 
		"count" : "1", 
		"code" : "9785916712827", 
		"tags" : ["Будущее", "Разные темы"], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Памятникик науки и техники (1 выпуск)", 
		"author" : "Григорян Г.Г.", 
		"count" : "1", 
		"code" : "", 
		"tags" : ["История", "Картинки"], 
		"subject" : "История науки", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Памятникик науки и техники (2 выпуск)", 
		"author" : "Григорян Г.Г.", 
		"count" : "1", 
		"code" : "", 
		"tags" : ["История", "Картинки"], 
		"subject" : "История науки", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Сказки и подсказки", 
		"author" : "Козлова Е.Г.", 
		"count" : "1", 
		"code" : "9785443901350", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Занимательная механика", 
		"author" : "Перельман Я.И.", 
		"count" : "1", 
		"code" : "9785170447244", 
		"tags" : ["Механика", "Задачник", "Теория"], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Графы", 
		"author" : "Гуровиц В.М., Ховрина В.В.", 
		"count" : "1", 
		"code" : "9785443901305", 
		"tags" : ["Теория графов", "Задачник", "Теория"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Математический кружок", 
		"author" : "Спивак А.В.", 
		"count" : "1", 
		"code" : "9785443900834", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Нужна ли в школе математика?", 
		"author" : "Арнольд В.И.", 
		"count" : "1", 
		"code" : "9785443900711", 
		"tags" : ["Методика"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Средневековые головоломки", 
		"author" : "Дьюдени Г.", 
		"count" : "1", 
		"code" : "9785367036176", 
		"tags" : ["Задачник", "Олимпиадные задачи", "Картинки"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Приглашение на математический праздник", 
		"author" : "Ященко И.В.", 
		"count" : "1", 
		"code" : "9785940573647", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Малыш и математика", 
		"author" : "Звонкин А.К.", 
		"count" : "1", 
		"code" : "9785940573159", 
		"tags" : ["Рассказы", "Картинки"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Алгебра в комиксах", 
		"author" : "Гоник Л.", 
		"count" : "1", 
		"code" : "9785389089044", 
		"tags" : ["Картинки", "Задачник"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Страна математических чудес", 
		"author" : "Акияма Д., Руис М.-Д.", 
		"count" : "1", 
		"code" : "9785443901329", 
		"tags" : ["Картинки", "Лекции"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Задачи и головомки", 
		"author" : "Перельман Я.", 
		"count" : "1", 
		"code" : "9785170501496", 
		"tags" : ["Задачник", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Квантик альманах для любознательных выпуск 1", 
		"author" : "Бердников А.С.", 
		"count" : "1", 
		"code" : "9785443906218", 
		"tags" : ["Задачник", "Картинки", "Рассказы", "Квантик"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Квантик альманах для любознательных выпуск 2", 
		"author" : "Бердников А.С.", 
		"count" : "1", 
		"code" : "9785443902838", 
		"tags" : ["Задачник", "Картинки", "Рассказы", "Квантик"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Квантик альманах для любознательных выпуск 3", 
		"author" : "Бердников А.С.", 
		"count" : "1", 
		"code" : "9785443906140", 
		"tags" : ["Задачник", "Картинки", "Рассказы", "Квантик"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Квантик альманах для любознательных выпуск 4", 
		"author" : "Бердников А.С.", 
		"count" : "2", 
		"code" : "9785443906201", 
		"tags" : ["Задачник", "Картинки", "Рассказы", "Квантик"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Квантик альманах для любознательных выпуск 5", 
		"author" : "Бердников А.С.", 
		"count" : "1", 
		"code" : "9785443903439", 
		"tags" : ["Задачник", "Картинки", "Рассказы", "Квантик"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Четность", 
		"author" : "Медников Л.Э.", 
		"count" : "1", 
		"code" : "9785443903347", 
		"tags" : ["Теория чисел", "Задачник"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Бабушкины сказки", 
		"author" : "Козлова Е.Г.", 
		"count" : "1", 
		"code" : "9785443900346", 
		"tags" : ["Задачник", "Олимпиадные задачи", "Картинки"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "ЕГЭ - 2014 по русскому языку", 
		"author" : "Бисеров А.Ю.", 
		"count" : "1", 
		"code" : "9785170793709", 
		"tags" : ["ЕГЭ", "Русский язык"], 
		"subject" : "Русский язык", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "ЕГЭ - 2015 по информатике и ИКТ", 
		"author" : "Крылов С.С.", 
		"count" : "1", 
		"code" : "9785445405382", 
		"tags" : ["ЕГЭ", "Информатика"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "ЕГЭ - 2011 по математике", 
		"author" : "Семенова А.Л.", 
		"count" : "1", 
		"code" : "9785905084195", 
		"tags" : ["ЕГЭ", "Математика"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "C & C++", 
		"author" : "Голуб А.", 
		"count" : "1", 
		"code" : "5893500229", 
		"tags" : ["Программирование", "Языки программирования"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Апология математики", 
		"author" : "Успенский В.", 
		"count" : "1", 
		"code" : "9785367019674", 
		"tags" : ["Лекции", "Теория", "История"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Ньютон и фальшивомонетчик", 
		"author" : "Левенсон Т.", 
		"count" : "1", 
		"code" : "9785170778140", 
		"tags" : ["Рассказы", "Теория", "История"], 
		"subject" : "Физика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Как написать математическую статью по-английски", 
		"author" : "Сосинский А.Б.", 
		"count" : "2", 
		"code" : "9785886880700", 
		"tags" : ["Методика", "Теория"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Краткая алгебра", 
		"author" : "Киселев А.П.", 
		"count" : "1", 
		"code" : "9785971019077", 
		"tags" : ["Алгебра", "Задачник", "Школьные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Собрание геометрических теорем и задач", 
		"author" : "Пржевальский Е.М.", 
		"count" : "1", 
		"code" : "9785971019794", 
		"tags" : ["Задачник", "Школьные задачи"], 
		"subject" : "Геометрия", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Очерки по истории математики", 
		"author" : "Бурбаки Н.", 
		"count" : "1", 
		"code" : "9785397010603", 
		"tags" : ["Теория", "Теорема", "История"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Пути и лабиринты", 
		"author" : "Пейффер Ж.", 
		"count" : "1", 
		"code" : "", 
		"tags" : ["Разные темы", "Задачник", "Школьные задачи", "Олимпиадные задачи"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Лекции по истории науки", 
		"author" : "Смирнов С.Г.", 
		"count" : "1", 
		"code" : "9785443900391", 
		"tags" : ["Лекции"], 
		"subject" : "История науки", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Сюрреальные числа", 
		"author" : "Кнут Д.", 
		"count" : "1", 
		"code" : "9785996315413", 
		"tags" : ["Рассказы", "Теория", "Лекции"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Знакомство с теорией вероятностей", 
		"author" : "Мякишев А.Г.", 
		"count" : "1", 
		"code" : "", 
		"tags" : ["Теория вероятностей", "Лекции", "Задачник"], 
		"subject" : "Математика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Corel Draw 10 учебный курс", 
		"author" : "Миронов Д.", 
		"count" : "1", 
		"code" : "9785318000454", 
		"tags" : ["Программное обеспечение"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Как программировать на C (седьмое издание)", 
		"author" : "Дейтел П., Дейтел Х.", 
		"count" : "1", 
		"code" : "9785951805591", 
		"tags" : ["Языки программирования", "Задачи"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Набор и верстка в Latex", 
		"author" : "Львовский С.М.", 
		"count" : "1", 
		"code" : "9785443902395", 
		"tags" : ["Программное обеспечение"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Введение в теорию алгоритмов и структур данных", 
		"author" : "Бабенко М.А., Левин М.В.", 
		"count" : "1", 
		"code" : "9785443902401", 
		"tags" : ["Олимпиадные заачи", "Алгоритмы", "Структуры данных"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "E-maxx", 
		"author" : "Иванов М.", 
		"count" : "1", 
		"code" : "", 
		"tags" : ["Алгоритмы", "Структуры данных"], 
		"subject" : "Инофматика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Лекции о сложности алгоритмов", 
		"author" : "Абрамов С.А.", 
		"count" : "1", 
		"code" : "9785443902043", 
		"tags" : ["Алгоритмы", "Лекции"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Програмирование: теоремы и задачи", 
		"author" : "Шень А.", 
		"count" : "2", 
		"code" : "9785443901145", 
		"tags" : ["Олимпиадные задачи", "Лекции", "Алгоритмы"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Алгоритмы: построение и анализ (второе издание)", 
		"author" : "Кормен Т.", 
		"count" : "1", 
		"code" : "9785845908575", 
		"tags" : ["Алгоритмы", "Структуры данных", "Лекции"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Алгоритмы: построение и анализ (третье издание)", 
		"author" : "Кормен Т.", 
		"count" : "1", 
		"code" : "9785845917942", 
		"tags" : ["Алгоритмы", "Структуры данных", "Лекции"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Прикладная криптография", 
		"author" : "Шнайнер Б.", 
		"count" : "1", 
		"code" : "9785893920550", 
		"tags" : ["Криптография", "Кодирование", "Теория"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
		{
		"image": "/book.png", 
		"title" : "Язык программирования С (ANSI C)", 
		"author" : "Керниган Б., Ритчи Д.", 
		"count" : "1", 
		"code" : "9785947769357", 
		"tags" : ["Языки программирования", "Задачник"], 
		"subject" : "Информатика", 
		"recommendations" : "", 
		"annotation" : ""
		}, 
	]
	config = load_config('Server')
	db = Database(config['database_name'], ['books'])
	for book in data:
		book['handed'] = 0
		book['personality'] = book['title'].lower() + ' ' +  book['author'].lower() + ' ' + book['subject'].lower()
		for tag in book['tags']:
			book['personality'] += ' ' + tag.lower()
		book['visible'] = 'on'
		print(book['personality'])
		db.books.insert(book)


9785845908919