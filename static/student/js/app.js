'use strict';


var DigitalLibraryApp = angular.module('DigitalLibraryApp', [
  'DigitalLibraryControllers',
  'ngRoute',
  'ngCookies'
]);


DigitalLibraryApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/handed', {
        templateUrl: 'student/templates/handed.html',
        controller: 'HandedCtrl'
      }).
      when('/books/:book_id', {
        templateUrl: 'student/templates/book.html',
        controller: 'BookCtrl'
      }).
      otherwise({
        redirectTo: '/handed'
      });
  }]);