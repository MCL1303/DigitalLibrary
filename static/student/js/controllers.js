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


DigitalLibraryControllers.controller('HandedCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
		document.title = 'На руках';
		$rootScope.page = 1;
}]);