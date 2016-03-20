'use strict';


var DigitalLibraryApp = angular.module('DigitalLibraryApp', [
  'DigitalLibraryControllers',
  'ngRoute',
  'ngCookies'
]);


DigitalLibraryApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/books', {
        templateUrl: 'librarian/templates/books.html',
        controller: 'BooksCtrl'
      }).
      when('/add', {
        templateUrl: 'librarian/templates/add.html',
        controller: 'AddCtrl'
      }).
      when('/users', {
        templateUrl: 'librarian/templates/users.html',
        controller: 'UsersCtrl'
      }).
      when('/handlog', {
        templateUrl: 'librarian/templates/handlog.html',
        controller: 'HandlogCtrl'
      }).
      when('/books/:book_id', {
        templateUrl: 'librarian/templates/book.html',
        controller: 'BookCtrl'
      }).
      when('/users/:user_id', {
        templateUrl: 'librarian/templates/user.html',
        controller: 'UserCtrl'
      }).
      otherwise({
        redirectTo: '/books'
      });
  }]);