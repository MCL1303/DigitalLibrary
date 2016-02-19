'use strict';


var DigitalLibraryControllers = angular.module('DigitalLibraryControllers', []);


DigitalLibraryControllers.controller('MainCtrl', ['$scope',
	function($scope) {
		document.title = 'Библиотека МХЛ';
}]);


DigitalLibraryControllers.controller('SigninCtrl', ['$scope',
	function($scope) {
		document.title = 'Авторизация';
}]);


DigitalLibraryControllers.controller('SignupCtrl', ['$scope',
	function($scope) {
		document.title = 'Регистрация';
}]);