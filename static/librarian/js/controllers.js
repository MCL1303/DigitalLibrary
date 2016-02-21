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
}]);


DigitalLibraryControllers.controller('HandedCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
		document.title = 'На руках';
		$rootScope.page = 1;
}]);


DigitalLibraryControllers.controller('AddCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
		document.title = 'Добавление книги';
		$rootScope.page = 2;
}]);


DigitalLibraryControllers.controller('BooksCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
		document.title = 'Каталог книг';
		$rootScope.page = 2;
}]);


DigitalLibraryControllers.controller('StudentsCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
		document.title = 'Ученики';
		$rootScope.page = 3;
}]);


DigitalLibraryControllers.controller('HandlogCtrl', ['$scope', '$rootScope', '$http',
	function($scope, $rootScope, $http) {
		document.title = 'Журнал';
		$rootScope.page = 4;
		$scope.rootScope = angular.element(document.body).scope().$root;  
		$scope.rootScope.handlog = [
			{
				'datetime': 'Загрузка...',
				'student': 'Загрузка...',
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
			$http.post('/api/handlog/get', {'page': $scope.rootScope.handlog_page}).then(function(data) {
				if(data.data.answer == 'ok') {
					console.log(data.data.page);
					if($scope.rootScope.handlog[0]['student'] == 'Загрузка...') {
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
		$scope.get_older();
}]);