'use strict';


var DigitalLibraryControllers = angular.module('DigitalLibraryControllers', []);


DigitalLibraryControllers.controller('MainCtrl', ['$scope', '$http', '$cookies',
	function($scope, $http, $cookies) {
		document.title = 'Библиотека МХЛ';
		$scope.user = {'name': 'Загрузка'};
		$http.post('/api/info/user', {}).then(function(data) {
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


DigitalLibraryControllers.controller('BooksCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
		document.title = 'Каталог книг';
		$rootScope.page = 1;
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
}]);