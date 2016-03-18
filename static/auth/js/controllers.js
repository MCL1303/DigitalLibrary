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
		$scope.signin = function() {
			if($scope.login.length < 4 || $scope.password.length < 8) {
				$('[data-toggle="popover"]').popover('show');
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
					$('[data-toggle="popover"]').popover('show');
				}
			});
		};
		$(document).keypress(function(e) {
			if(e.which == 13) {
				$scope.signin();
			}
		});
		$(document).ready(function(){
			$('[data-toggle="popover"]').popover();   
		});
		$scope.hider = function() {
			$('[data-toggle="popover"]').popover('hide');
		};
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
		$scope.password = '';
		$scope.login = '';
		$scope.invite = '';

		$scope.corrCode = false;
		$scope.corrEmail = false;
		$scope.notUsedEmail = false;
		$scope.corrLogin = false;
		$scope.notUsedLogin = false;
		$scope.corrPassd = false;

		$scope.login_timeout = setTimeout(function(){}, 100000);
		$scope.email_timeout = setTimeout(function(){}, 100000);

		$scope.cheking= true;

		$(document).ready(function(){
			$('[data-toggle="popover"]').popover();   
		});
		$scope.hider = function() {
			$('[data-toggle="popover"]').popover('hide');
		};
		String.prototype.replaceAll = function(search, replacement) {
			var target = this;
			return target.replace(new RegExp(search, 'g'), replacement);
		};
		$scope.blink = function() {
			if(!$scope.corrEmail && $scope.email != '') {
				$('#emailBad[data-toggle="popover"]').popover('show');
			} else {
				$('#emailBad[data-toggle="popover"]').popover('hide');
			}
			if(!$scope.corrCode && $scope.invite != '') {
				$('#invite[data-toggle="popover"]').popover('show');
			} else {
				$('#invite[data-toggle="popover"]').popover('hide');
			}
			if(!$scope.corrLogin && $scope.login != '') {
				$('#loginBad[data-toggle="popover"]').popover('show');
			} else {
				$('#loginBad[data-toggle="popover"]').popover('hide');
			}
			if(!$scope.corrPassd && $scope.password != '') {
				$('#password[data-toggle="popover"]').popover('show');
			} else {
				$('#password[data-toggle="popover"]').popover('hide');
			}
			if($scope.corrEmail && !$scope.notUsedEmail && $scope.email != '') {
				$('#emailUsed[data-toggle="popover"]').popover('show');
			} else {
				$('#emailUsed[data-toggle="popover"]').popover('hide');
			}
			if($scope.corrLogin && !$scope.notUsedLogin && $scope.login != '') {
				$('#loginUsed[data-toggle="popover"]').popover('show');
			} else {
				$('#loginUsed[data-toggle="popover"]').popover('hide');
			}
		};
		$scope.showIfNo = function() {
			var showed = $('.popover-content')
			var WCode = false;
			var WEmail = false;
			var WPass = false;
			var WLogin = false;
			var UEmail = false;
			var ULogin = false;

			for(var i = 0; i < showed.length; i++) {
				if(showed[i].innerHTML == 'Введён неверный код') {
					WCode = true;
				}
				if(showed[i].innerHTML == 'Неправильный адрес электронной почты') {
					WEmail = true;
				}
				if(showed[i].innerHTML == 'Логин может состоять из цифр, латинских букв, знака подчеркивания и дефиса. А так же быть не короче 4 символов') {
					WLogin = true;
				}
				if(showed[i].innerHTML == 'Пароль должен содержать хотя бы 8 символов, включать буквы и цифры') {
					WPass = true;
				}
				if(showed[i].innerHTML == 'Логин занят') {
					ULogin = true;
				}
				if(showed[i].innerHTML == 'Этот адрес занят') {
					UEmail = true;
				}
			}
			if(!$scope.corrEmail && $scope.email != '') {
				if(!WEmail) {
					$('#emailBad[data-toggle="popover"]').popover('show');
				}
			} else {
				$('#emailBad[data-toggle="popover"]').popover('hide');
			}
			if(!$scope.corrCode && $scope.invite != '') {
				if(!WCode) {
					$('#invite[data-toggle="popover"]').popover('show');
				}
			} else {
				$('#invite[data-toggle="popover"]').popover('hide');
			}
			if(!$scope.corrLogin && $scope.login != '') {
				if(!WLogin) {
					$('#loginBad[data-toggle="popover"]').popover('show');
				}
			} else {
				$('#loginBad[data-toggle="popover"]').popover('hide');
			}
			if(!$scope.corrPassd && $scope.password != '') {
				if(!WPass) {
					$('#password[data-toggle="popover"]').popover('show');
				}
			} else {
				$('#password[data-toggle="popover"]').popover('hide');
			}
			if($scope.corrEmail && !$scope.notUsedEmail && $scope.email != '') {
				if(!UEmail) {
					$('#emailUsed[data-toggle="popover"]').popover('show');
				}
			} else {
				$('#emailUsed[data-toggle="popover"]').popover('hide');
			}
			if($scope.corrLogin && !$scope.notUsedLogin && $scope.login != '') {
				if(!ULogin) {
					$('#loginUsed[data-toggle="popover"]').popover('show');
				}
			} else {
				$('#loginUsed[data-toggle="popover"]').popover('hide');
			}
		}
		function validateEmail() {
			var email = $scope.email;
			$scope.corrEmail = false;
			$scope.notUsedEmail = false;
			if (email == '') {
				$scope.inputEmail = '#ffffff';
				$('#emailBad[data-toggle="popover"]').popover('hide');
				return false;
			}
			var re = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
			if (!re.test(email)) {
				$scope.inputEmail = '#f2dede';
				$('#emailBad[data-toggle="popover"]').popover('show');
			} else {
				$scope.corrEmail = true;
				$scope.inputEmail = '#dff0d8';
				$('#emailBad[data-toggle="popover"]').popover('hide');
			}
			return re.test(email);
		}
		function validateLogin() {
			var login = $scope.login;
			$scope.corrLogin = false;
			$scope.notUsedLogin = false;
			if (login == '') {
				$scope.inputLogin = '#ffffff';
				$('#loginUsed[data-toggle="popover"]').popover('hide');
				return false;
			}
			if(login.length < 4) {
				$scope.inputLogin = '#f2dede';
				$('#loginBad[data-toggle="popover"]').popover('show');
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
			login = login.replaceAll('-', '');
			login = login.replaceAll('_', '');
			if (login != '') {
				$scope.inputLogin = '#f2dede';
				$('#loginBad[data-toggle="popover"]').popover('show');
			} else {
				$scope.inputLogin = '#dff0d8';
				$('#loginBad[data-toggle="popover"]').popover('hide');
				$scope.corrLogin = true;
			}
			return login == '';
		}
		function validateInvite() {
			$scope.corrCode = false;
			var invite = $scope.invite;
			if (invite == '') {
				$scope.inputInvite = '#ffffff';
				$('#invite[data-toggle="popover"]').popover('hide');
				return false;
			}
			$('#invite[data-toggle="popover"]').popover('show');
			$scope.inputInvite = '#f2dede';
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
			$scope.corrPassd = false;
			if(password == '') {
				$scope.inputPassword = '#ffffff';
				$('#password[data-toggle="popover"]').popover('hide');
				return false;
			}
			if(password.length < 8) {
				$scope.progress = password.length * 4;
				$scope.inputPassword = '#f2dede';
				$('#password[data-toggle="popover"]').popover('show');
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
				$('#password[data-toggle="popover"]').popover('show');
				return false;
			}
			$('#password[data-toggle="popover"]').popover('hide');
			$scope.corrPassd = true;
			return true;
		};
		$(document).keypress(function(e) {
			if(e.which == 13) {
				$scope.signup();
			}
		});
		$scope.p_check_email = function() {
			$http.post('/api/form/email', {'email': $scope.email}).then(function(data) {
				console.log(data.data);
				if(data.data.answer == 'unused') {
					$scope.inputEmail = '#dff0d8';
					$('#emailUsed[data-toggle="popover"]').popover('hide');
					$scope.notUsedEmail = true;
				} else {
					$scope.inputEmail = '#f2dede';
					$('#emailUsed[data-toggle="popover"]').popover('show');
				}
				$scope.cheking = false;
			});
		};
		$scope.check_email = function() {
			clearTimeout($scope.email_timeout);
			$('#emailUsed[data-toggle="popover"]').popover('hide');
			if(validateEmail()) {
				$scope.cheking = true;
				email_timeout = setTimeout($scope.p_check_email, 1500);
			}
			$scope.showIfNo();
		};
		$scope.p_check_login = function () {
			$http.post('/api/form/login', {'login': $scope.login}).then(function(data) {
				console.log(data.data);
				if(data.data.answer == 'unused') {
					$('#loginUsed[data-toggle="popover"]').popover('hide');
					$scope.inputLogin = '#dff0d8';
					$scope.notUsedLogin = true;
				} else {
					$('#loginUsed[data-toggle="popover"]').popover('show');
					$scope.inputLogin = '#f2dede';
				}
				$scope.cheking = false;
			});
		};
		$scope.check_login = function() {
			clearTimeout($scope.login_timeout);
			$('#loginUsed[data-toggle="popover"]').popover('hide');
			if(validateLogin()) {
				$scope.cheking = true;
				login_timeout = setTimeout($scope.p_check_login, 1500);
			}
			$scope.showIfNo();
		};
		$scope.check_invite = function() {
			$('#invite[data-toggle="popover"]').popover('show');
			if(validateInvite()) {
				$http.post('/api/form/invite', {'invite': $scope.invite}).then(function(data) {
					console.log(data.data);
					if(data.data.answer == 'unused') {
						$scope.inputInvite = '#dff0d8';
						$('#invite[data-toggle="popover"]').popover('hide');
						$scope.corrCode = true;
					} else {
						$scope.inputInvite = '#f2dede';
						$('#invite[data-toggle="popover"]').popover('show');
					}
				});
			}
			$scope.showIfNo();
		};
		$scope.check_password = function() {
			validatePassword();
			$scope.showIfNo();
		};
		$scope.signup = function() {
			var check = true;
			check = check && !$scope.cheking;
			check = check && $scope.corrPassd;
			check = check && $scope.corrLogin;
			check = check && $scope.corrEmail;
			check = check && $scope.corrCode;
			check = check && $scope.notUsedLogin;
			check = check && $scope.notUsedEmail;
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
					} else {
						location.reload();
					}
				});
			} else {
				$scope.blink();
			}
		};
}]);