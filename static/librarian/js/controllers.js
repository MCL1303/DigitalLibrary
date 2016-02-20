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


DigitalLibraryControllers.controller('HandlogCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
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
		]
}]);