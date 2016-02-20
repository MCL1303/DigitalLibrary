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
        templateUrl: 'librarian/templates/handed.html',
        controller: 'HandedCtrl'
      }).
      when('/books', {
        templateUrl: 'librarian/templates/books.html',
        controller: 'BooksCtrl'
      }).
      when('/add', {
        templateUrl: 'librarian/templates/add.html',
        controller: 'AddCtrl'
      }).
      when('/students', {
        templateUrl: 'librarian/templates/students.html',
        controller: 'StudentsCtrl'
      }).
      when('/handlog', {
        templateUrl: 'librarian/templates/handlog.html',
        controller: 'HandlogCtrl'
      }).
      otherwise({
        redirectTo: '/handed'
      });
  }]);