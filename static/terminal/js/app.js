'use strict';


var DigitalLibraryApp = angular.module('DigitalLibraryApp', [
  'DigitalLibraryControllers',
  'ngRoute',
  'ngCookies'
]);


DigitalLibraryApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/start', {
        templateUrl: 'terminal/templates/start.html'
      }).
      when('/user', {
        templateUrl: 'terminal/templates/user.html'
      }).
      when('/operate', {
        templateUrl: 'terminal/templates/operate.html'
      }).
      otherwise({
        redirectTo: '/start'
      });
  }]);