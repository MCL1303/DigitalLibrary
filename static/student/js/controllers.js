'use strict';


var DigitalLibraryControllers = angular.module('DigitalLibraryControllers', []);


DigitalLibraryControllers.controller('MainCtrl', ['$scope', '$http', '$cookies',
	function($scope, $http, $cookies) {
		document.title = 'Библиотека МХЛ';
		$scope.user = {'name': 'Загрузка'};
		$http.post('/api/info/init_user', {}).then(function(data) {
			if(data.data.answer == 'ok') {
				$scope.user = data.data.user;
			} else {
				$cookies.put('session_id', '');
				location.reload();
			}
		});
		$scope.signout = function() {
			$cookies.put('session_id', '');
			location.reload();
		};
		$scope.handlog_loaded = false;
}]);


DigitalLibraryControllers.controller('BookCtrl', ['$scope', '$rootScope', '$routeParams', '$http', '$cookies',
	function($scope, $rootScope, $routeParams, $http, $cookies) {
		$rootScope.page = 2;
		$scope.book = {'title': 'Ошибка'};
		$('[data-toggle="tooltip"]').tooltip()
		$http.post('/api/info/book', {
			'book': $routeParams.book_id
		}).then(function(data) {
			console.log(data.data);
			if(data.data.answer == 'ok') {
				$scope.book = data.data.book;
				document.title = $scope.book.title;
			} else if(data.data.answer == 'not_found') {
				window.location.replace("/404");
			} else {
				$cookies.put('session_id', '');
				location.reload();
			}
		});
		$scope.image = 'http://book2.me/f/classic.jpg';
}]);


DigitalLibraryControllers.controller('BooksCtrl', ['$scope', '$rootScope', '$http',
	function($scope, $rootScope, $http) {
		document.title = 'Каталог книг';
		$rootScope.page = 2;
		$scope.request = '';
		$scope.page = 1;
		$scope.more = false;
		if ($rootScope.lastBookReq === undefined) {
			$rootScope.lastBookReq = 'Математика';
			$http.post('/api/books/search', {'request': $rootScope.lastBookReq, 'page': 1}).then(function(data) {
				if(data.data.answer == 'ok') {
					$scope.results = data.data.results;
					$rootScope.lastBookRes = $scope.results;
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		} else {
			$scope.results = $rootScope.lastBookRes;
			$scope.request = $rootScope.lastBookReq;
		}
		$scope.set_ex = function(ex) {
			$scope.request = ex;
			$scope.search();
		}
		$scope.search = function() {
			$scope.page = 1;
			$rootScope.lastBookReq = $scope.request;
			$http.post('/api/books/search', {'request': $scope.request, 'page': $scope.page}).then(function(data) {
				if(data.data.answer == 'ok') {
					$scope.results = data.data.results;
					$rootScope.lastBookRes = $scope.results;
					if(data.data.results.length == 30) {
						$scope.more = true;
					} else {
						$scope.more = false;
					}
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		};
		$scope.show_more = function() {
			$scope.page = $scope.page + 1;
			$http.post('/api/books/search', {'request': $scope.request, 'page': $scope.page}).then(function(data) {
				if(data.data.answer == 'ok') {
					for(var i = 0; i < data.data.results.length; i++) {
						$scope.results.push(data.data.results[i]);
					}
					$rootScope.lastBookRes = $scope.results;
					if(data.data.results.length != 30) {
						$scope.more = false;
					}
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		};
		$(document).keypress(function(e) {
			if(e.which == 13) {
				$scope.search();
			}
		});
}]);


DigitalLibraryControllers.controller('HandedCtrl', ['$scope', '$rootScope', '$http',
	function($scope, $rootScope, $http) {
		document.title = 'На руках';
		$rootScope.page = 1;
		$scope.books = [];
		$http.post('/api/info/handed', {}).then(function(data) {
			console.log(data.data);
			if(data.data.answer == 'ok') {
				for(var i = 0; i < data.data.results.length; i++) {
					$scope.books.push(data.data.results[i]);
				}
				if(data.data.results.length != 30) {
					$scope.more = false;
				}
			} else {
				$cookies.put('session_id', '');
				location.reload();
			}
		});
}]);