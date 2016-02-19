'use strict';


var DigitalLibraryControllers = angular.module('DigitalLibraryControllers', []);


DigitalLibraryControllers.controller('MainCtrl', ['$scope',
	function($scope) {
		document.title = 'Библиотека МХЛ';
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
}]);