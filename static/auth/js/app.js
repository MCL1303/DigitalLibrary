'use strict';


var DigitalLibraryApp = angular.module('DigitalLibraryApp', [
  'DigitalLibraryControllers',
  'ngRoute',
  'ngCookies'
]);


DigitalLibraryApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/signin', {
        templateUrl: 'auth/templates/signin.html',
        controller: 'SigninCtrl'
      }).
      when('/signup', {
        templateUrl: 'auth/templates/signup.html',
        controller: 'SignupCtrl'
      }).
      otherwise({
        redirectTo: '/signin'
      });
  }]);