'use strict';


var DigitalLibraryControllers = angular.module('DigitalLibraryControllers', []);


DigitalLibraryControllers.controller('MainCtrl', ['$scope',
	function($scope) {
		document.title = 'Библиотека МХЛ';
}]);


DigitalLibraryControllers.controller('SigninCtrl', ['$scope', '$http', '$cookies',
	function($scope, $http, $cookies) {
		document.title = 'Авторизация';
		$scope.login = '';
		$scope.password = '';
		$scope.remember = false;
		$scope.error = false;
		$scope.signin = function() {
			$scope.error = false;
			if($scope.login.length < 4 || $scope.password.length < 8) {
				$scope.error = true;
				console.log('fail');
				return;
			}
			$http.post('/api/user/signin', {
				'login': $scope.login,
				'password': $scope.password,
				'remember': $scope.remember
			}).then(function(data) {
				console.log(data.data);
				if(data.data.answer == 'ok') {
					$cookies.put('session_id', data.data.session_id);
					window.location.replace("/");
				} else {
					$scope.error = true;
				}
			});
		};
		$(document).keypress(function(e) {
			if(e.which == 13) {
				$scope.signin();
			}
		});
}]);


DigitalLibraryControllers.controller('SignupCtrl', ['$scope', '$http', '$cookies',
	function($scope, $http, $cookies) {
		document.title = 'Регистрация';
		$scope.progress = 0;
		$scope.inputInvite = '#ffffff';
		$scope.inputLogin = '#ffffff';
		$scope.inputPassword = '#ffffff';
		$scope.inputEmail = '#ffffff';
		$scope.email = '';
		String.prototype.replaceAll = function(search, replacement) {
			var target = this;
			return target.replace(new RegExp(search, 'g'), replacement);
		};
		function validateEmail() {
			var email = $scope.email;
			var re = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
			return re.test(email);
		}
		function validateLogin() {
			var login = $scope.login;
			if(login.length < 4) {
				return false;
			}
			var login = login, i;
			for(i = 0; i < 25; i++) {
				login = login.replaceAll(String.fromCharCode(65 + i), '');
			}
			for(i = 0; i < 25; i++) {
				login = login.replaceAll(String.fromCharCode(97 + i), '');
			}
			for(i = 0; i < 10; i++) {
				login = login.replaceAll(i.toString(), '');
			}
			return login == '';
		}
		function validateInvite() {
			var invite = $scope.invite;
			if(invite.length != 8) {
				return false;
			}
			var c_invite = invite, i;
			for(i = 0; i < 25; i++) {
				c_invite = c_invite.replaceAll(String.fromCharCode(65 + i), '');
			}
			for(i = 0; i < 25; i++) {
				c_invite = c_invite.replaceAll(String.fromCharCode(97 + i), '');
			}
			for(i = 0; i < 10; i++) {
				c_invite = c_invite.replaceAll(i.toString(), '');
			}
			return c_invite == '';
		}
		function validatePassword() {
			var password = $scope.password;
			$scope.progress = 0;
			if(password == '') {
				$scope.inputPassword = '#ffffff';
				return false;
			}
			if(password.length < 8) {
				$scope.progress = password.length * 4;
				$scope.inputPassword = '#f2dede';
				return false;
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
				password = password.replaceAll(i.toString(), '');
			}
			for(i = 0; i < 25; i++) {
				password = password.replaceAll(String.fromCharCode(65 + i), '');
			}
			for(i = 0; i < 25; i++) {
				password = password.replaceAll(String.fromCharCode(97 + i), '');
			}
			for(i = 0; i < 33; i++) {
				password = password.replaceAll(String.fromCharCode(1072 + i), '');
			}
			for(i = 0; i < 33; i++) {
				password = password.replaceAll(String.fromCharCode(1040 + i), '');
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
				$scope.inputPassword = '#f2dede';
				return false;
			}
			return true;
		};
		$(document).keypress(function(e) {
			if(e.which == 13) {
				$scope.signup();
			}
		});
		$scope.check_email = function() {
			if(validateEmail()) {
				$http.post('/api/form/email', {'email': $scope.email}).then(function(data) {
					console.log(data.data);
					if(data.data.answer == 'unused') {
						$scope.inputEmail = '#dff0d8';
					} else {
						$scope.inputEmail = '#f2dede';
					}
				});
			} else {
				$scope.inputEmail = '#ffffff';								
			}
		};
		$scope.check_login = function() {
			if(validateLogin()) {
				$http.post('/api/form/login', {'login': $scope.login}).then(function(data) {
					console.log(data.data);
					if(data.data.answer == 'unused') {
						$scope.inputLogin = '#dff0d8';
					} else {
						$scope.inputLogin = '#f2dede';
					}
				});
			} else {
				$scope.inputLogin = '#ffffff';								
			}
		};
		$scope.check_invite = function() {
			if(validateInvite()) {
				$http.post('/api/form/invite', {'invite': $scope.invite}).then(function(data) {
					console.log(data.data);
					if(data.data.answer == 'unused') {
						$scope.inputInvite = '#dff0d8';
					} else {
						$scope.inputInvite = '#f2dede';
					}
				});
			} else {
				$scope.inputInvite = '#ffffff';								
			}
		};
		$scope.check_password = function() {
			validatePassword();
		};
		$scope.signup = function() {
			var check = true;
			check = check && validatePassword();
			check = check && validateLogin();
			check = check && validateEmail();
			check = check && validateInvite();
			if(check) {
				$http.post('/api/user/signup', {
					'invite': $scope.invite,
					'password': $scope.password,
					'email': $scope.email,
					'login': $scope.login
				}).then(function(data) {
					if(data.data.answer == 'ok') {
						$cookies.put('session_id', data.data.session_id);
						window.location.replace("/");
					}
				});
			}
		};
}]);