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
		String.prototype.replaceAll = function(search, replacement) {
			var target = this;
			return target.replace(new RegExp(search, 'g'), replacement);
		};
		$('#password').on('input', function() {
			var password = $("#password")[0].value
			$scope.progress = 0;
			if(password.length < 8) {
				$scope.progress = password.length * 4;
				return;
			} else {
				$scope.progress = 10;
			}
			var i;
			for(i = 0; i < 10; i++) {
				if(password.search(i.toString()) != -1) {
					$scope.progress += 20;
					break;
				}
			}
			for(i = 0; i < 25; i++) {
				if(password.search(String.fromCharCode(65 + i)) != -1) {
					$scope.progress += 20;
					break;
				}
			}
			for(i = 0; i < 25; i++) {
				if(password.search(String.fromCharCode(97 + i)) != -1) {
					$scope.progress += 20;
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
			if(password != '') {
				$scope.progress = 100;
				console.log(password);
			}
		});
}]);