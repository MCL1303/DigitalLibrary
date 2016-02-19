'use strict';


var DigitalLibraryApp = angular.module('DigitalLibraryApp', [
  'DigitalLibraryControllers',
  'ngRoute'
]);


DigitalLibraryApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/handed', {
        templateUrl: 'templates/handed.html',
        controller: 'HandedCtrl'
      }).
      when('/books', {
        templateUrl: 'templates/books.html',
        controller: 'BooksCtrl'
      }).
      when('/add', {
        templateUrl: 'templates/add.html',
        controller: 'AddCtrl'
      }).
      when('/students', {
        templateUrl: 'templates/students.html',
        controller: 'StudentsCtrl'
      }).
      when('/handlog', {
        templateUrl: 'templates/handlog.html',
        controller: 'HandlogCtrl'
      }).
      otherwise({
        redirectTo: '/handed'
      });
  }]);