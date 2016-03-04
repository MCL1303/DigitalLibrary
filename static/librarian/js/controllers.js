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


DigitalLibraryControllers.controller('AddCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
		document.title = 'Добавление книги';
		$rootScope.page = 1;
}]);


DigitalLibraryControllers.controller('BooksCtrl', ['$scope', '$rootScope', '$http',
	function($scope, $rootScope, $http) {
		document.title = 'Каталог книг';
		$rootScope.page = 1;
		$scope.request = '';
		$scope.page = 1;
		$scope.more = false;
		$http.post('/api/books/search', {'request': 'Математика', 'page': 1}).then(function(data) {
			if(data.data.answer == 'ok') {
				$scope.results = data.data.results;
			} else {
				$cookies.put('session_id', '');
				location.reload();
			}
		});
		$scope.search = function() {
			$scope.page = 1;
			$http.post('/api/books/search', {'request': $scope.request, 'page': $scope.page}).then(function(data) {
				if(data.data.answer == 'ok') {
					$scope.results = data.data.results;
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


DigitalLibraryControllers.controller('UsersCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
		document.title = 'Ученики';
		$rootScope.page = 2;
}]);


DigitalLibraryControllers.controller('HandlogCtrl', ['$scope', '$rootScope', '$http',
	function($scope, $rootScope, $http) {
		document.title = 'Журнал';
		$rootScope.page = 3;
		$scope.rootScope = angular.element(document.body).scope().$root;
		$scope.rootScope.handlog = [
			{
				'datetime': 'Загрузка...',
				'user': 'Загрузка...',
				'book': 'Загрузка...',
				'action': 'Загрузка...'
			}
		];
		$scope.rootScope.handlog_page = 0;
		$scope.rootScope.handlog_more = true;
		$scope.get_older = function() {
			if($scope.rootScope.handlog_page == -1) {
				$scope.rootScope.handlog_more = false;
				return;
			}
			$scope.rootScope.handlog_page++;
			$http.post('/api/info/handlog', {'page': $scope.rootScope.handlog_page}).then(function(data) {
				if(data.data.answer == 'ok') {
					console.log(data.data.page);
					if($scope.rootScope.handlog[0]['user'] == 'Загрузка...') {
						$scope.rootScope.handlog = [];
					}
					for(var i = 0; i < data.data.page.length; i++) {
						$scope.rootScope.handlog.push(data.data.page[i])
					}
					if(!data.data.more) {
						$scope.rootScope.handlog_page = -1;
						$scope.rootScope.handlog_more = false;
					}
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		};
		// $scope.try_get_older = function() {
		// 	var last = $scope.handlog.length, now = last;
		// 	while(last == now) {
		// 		$scope.get_older();
		// 		now = $scope.handlog.length;
		// 	}
		// };
		if($scope.rootScope.handlog[0]['user'] == 'Загрузка...'){
			$scope.get_older();
		}
}]);


DigitalLibraryControllers.controller('BookCtrl', ['$scope', '$rootScope', '$routeParams', '$http', '$cookies',
	function($scope, $rootScope, $routeParams, $http, $cookies) {
		$rootScope.page = 1;
		$scope.book = {'title': 'Ошибка'};
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


DigitalLibraryControllers.controller('UserCtrl', ['$scope', '$rootScope', '$routeParams', '$http', '$cookies',
	function($scope, $rootScope, $routeParams, $http, $cookies) {
		$rootScope.page = 1;
		$scope.book = {'name': 'Ошибка'};
		$http.post('/api/info/user', {
			'user': $routeParams.user_id
		}).then(function(data) {
			console.log(data.data);
			if(data.data.answer == 'ok') {
				$scope.user = data.data.user;
				document.title = $scope.user.name;
			} else if(data.data.answer == 'not_found') {
				window.location.replace("/404");
			} else {
				$cookies.put('session_id', '');
				location.reload();
			}
		});
		$scope.image = '/user.png';
}]);