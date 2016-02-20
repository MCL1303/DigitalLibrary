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
		$scope.progress = 0;
		$scope.inputInvite = '#ffffff';
		$scope.inputLogin = '#ffffff';
		$scope.inputPassword = '#ffffff';
		$scope.inputEmail = '#ffffff';
		$scope.email = '';
		$scope.f = function() {alert('asdasd')};
		String.prototype.replaceAll = function(search, replacement) {
			var target = this;
			return target.replace(new RegExp(search, 'g'), replacement);
		};
		function validateEmail(email) {
			var re = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
			return re.test(email);
		}
		$(document).keypress(function(e) {
			if(e.which == 13) {
				alert('You pressed enter!');
			}
		});
		$scope.check_email = function() {
			if(validateEmail($scope.email)) {
				console.log($scope.email);
				$scope.inputEmail = '#dff0d8';
			} else {
				$scope.inputEmail = '#ffffff';								
			}
		};
		$scope.check_password = function() {
			var password = $scope.password;
			// console.log($scope.password);
			$scope.progress = 0;
			if(password.length < 8) {
				$scope.progress = password.length * 4;
				$scope.inputPassword = '#ffffff';
				return;
			} else {
				$scope.progress = 10;
			}
			var i;
			for(i = 0; i < 10; i++) {
				if(password.search(i.toString()) != -1) {
					$scope.progress += 15;
					break;
				}
			}
			for(i = 0; i < 25; i++) {
				if(password.search(String.fromCharCode(65 + i)) != -1) {
					$scope.progress += 15;
					break;
				}
			}
			for(i = 0; i < 25; i++) {
				if(password.search(String.fromCharCode(97 + i)) != -1) {
					$scope.progress += 15;
					break;
				}
			}
			for(i = 0; i < 33; i++) {
				if(password.search(String.fromCharCode(1072 + i)) != -1) {
					$scope.progress += 15;
					break;
				}
			}
			for(i = 0; i < 33; i++) {
				if(password.search(String.fromCharCode(1040 + i)) != -1) {
					$scope.progress += 15;
					break;
				}
			}


			for(i = 0; i < 10; i++) {
				password = password.replaceAll(i.toString(), '')
			}
			for(i = 0; i < 25; i++) {
				password = password.replaceAll(String.fromCharCode(65 + i), '')
			}
			for(i = 0; i < 25; i++) {
				password = password.replaceAll(String.fromCharCode(97 + i), '')
			}
			for(i = 0; i < 33; i++) {
				password = password.replaceAll(String.fromCharCode(1072 + i), '')
			}
			for(i = 0; i < 33; i++) {
				password = password.replaceAll(String.fromCharCode(1040 + i), '')
			}
			if(password != '') {
				$scope.progress += 15;
			}
			if($scope.progress > 100) {
				$scope.progress = 100;
			}
			if($scope.progress >= 40) {
				$scope.inputPassword = '#dff0d8';
			} else {
				$scope.inputPassword = '#ffffff';
			}
		};
}]);