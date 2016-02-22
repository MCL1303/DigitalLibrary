'use strict';


var DigitalLibraryControllers = angular.module('DigitalLibraryControllers', []);


DigitalLibraryControllers.controller('MainCtrl', ['$scope', '$http', '$cookies',
	function($scope, $http, $cookies) {
		document.title = 'Библиотека МХЛ';
		$scope.active = false;
		$scope.user = {'name': ''};
}]);


DigitalLibraryControllers.controller('StartCtrl', ['$scope', '$rootScope', '$http', '$interval',
	function($scope, $rootScope, $http, $interval) {
		document.title = 'Приложите карту';
		$interval.cancel($rootScope.inter);
		$rootScope.active = false;
		$rootScope.user = {'name': ''};
		$scope.sended = false;
		$scope.send_user = function(user) {
			if($rootScope.user.name != '') {
				window.location.hash = '/start';
				return;
			}
			$scope.sended = true;
			$http.post('/api/terminal/user', {
				'user': user
			}).then(function(data) {
				if(data.data.answer == 'active') {
					$rootScope.active = true;
					$rootScope.user = data.data.user;
				} else if(data.data.answer == 'inactive') {
					$rootScope.invite = data.data.invite;
					$rootScope.active = false;
					$rootScope.user = data.data.user;
				} else if(data.data.answer == 'not_found') {
					$scope.sended = false;
					return;
				}
				console.log(data.data);
				$rootScope.user = data.data.user;
				window.location.hash = '/user';
				$scope.sended = false;
			});
		};
}]);


DigitalLibraryControllers.controller('UserCtrl', ['$scope', '$rootScope', '$interval', '$http',
	function($scope, $rootScope, $interval, $http) {
		document.title = 'Отсканируйте штрих-код';
		$scope.rootScope = $rootScope;
		$scope.sended = false;
		if($rootScope.user === undefined) {
			window.location.hash = '/start';
		}
		$rootScope.inter = $interval(function() {
			if(Number($('#sec')[0].innerHTML) == 0) {
				$interval.cancel($rootScope.inter);
				window.location.hash = '/start';
			} else {
				$('#sec')[0].innerHTML = String(Number($('#sec')[0].innerHTML) - 1);
			}
		}, 1000);
		$scope.send_book = function(book) {
			$scope.sended = true;
			$interval.cancel($rootScope.inter);
			console.log($rootScope.user);
			$http.post('/api/operations', {
				'book': book,
				'user': $rootScope.user['_id']
			}).then(function(data) {
				$rootScope.operation = data.data;
				window.location.hash = '/operate';
				$scope.sended = false;
			});
		}
}]);


DigitalLibraryControllers.controller('OperateCtrl', ['$scope', '$rootScope', '$interval', '$http',
	function($scope, $rootScope, $interval, $http) {
		document.title = 'Вы вернули книгу';
		$scope.rootScope = $rootScope;
		if($rootScope.operation === undefined) {
			window.location.hash = '/start';
		}
		setTimeout(function() {
			window.location.hash = '/start';
		}, 6000)
}]);