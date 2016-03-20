'use strict';


var DigitalLibraryControllers = angular.module('DigitalLibraryControllers', []);


DigitalLibraryControllers.controller('MainCtrl', ['$scope', '$http', '$cookies',
	function($scope, $http, $cookies) {
		document.title = 'Библиотека МХЛ';
		$scope.user = {'name': 'Загрузка'};
		$http.post('/api/info/init_user', {}).then(function(data) {
			if(data.data.answer == 'ok') {
				$scope.user = data.data.user;
			} else {
				$cookies.put('session_id', '');
				location.reload();
			}
		});
		$scope.signout = function() {
			$cookies.put('session_id', '');
			location.reload();
		};
		$scope.handlog_loaded = false;
}]);


DigitalLibraryControllers.controller('AddCtrl', ['$scope', '$rootScope',
	function($scope, $rootScope) {
		document.title = 'Добавление книги';
		$rootScope.page = 1;
}]);


DigitalLibraryControllers.controller('BooksCtrl', ['$scope', '$rootScope', '$http',
	function($scope, $rootScope, $http) {
		document.title = 'Каталог книг';
		$rootScope.page = 1;
		$scope.request = '';
		$scope.page = 1;
		$scope.more = false;
		if ($rootScope.lastBookReq === undefined) {
			$rootScope.lastBookReq = 'Математика';
			$http.post('/api/books/search', {'request': $rootScope.lastBookReq, 'page': 1}).then(function(data) {
				if(data.data.answer == 'ok') {
					$scope.results = data.data.results;
					$rootScope.lastBookRes = $scope.results;
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		} else {
			$scope.results = $rootScope.lastBookRes;
			$scope.request = $rootScope.lastBookReq;
		}
		$scope.set_ex = function(ex) {
			$scope.request = ex;
			$scope.search();
		}
		$scope.search = function() {
			$scope.page = 1;
			$rootScope.lastBookReq = $scope.request;
			$http.post('/api/books/search', {'request': $scope.request, 'page': $scope.page}).then(function(data) {
				if(data.data.answer == 'ok') {
					console.log(data.data.results);
					$scope.results = data.data.results;
					$rootScope.lastBookRes = $scope.results;
					if(data.data.results.length == 30) {
						$scope.more = true;
					} else {
						$scope.more = false;
					}
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		};
		$scope.show_more = function() {
			$scope.page = $scope.page + 1;
			$http.post('/api/books/search', {'request': $scope.request, 'page': $scope.page}).then(function(data) {
				if(data.data.answer == 'ok') {
					for(var i = 0; i < data.data.results.length; i++) {
						$scope.results.push(data.data.results[i]);
					}
					$rootScope.lastBookRes = $scope.results;
					if(data.data.results.length != 30) {
						$scope.more = false;
					}
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		};
		$(document).keypress(function(e) {
			if(e.which == 13) {
				$scope.search();
			}
		});
}]);


DigitalLibraryControllers.controller('UsersCtrl', ['$scope', '$rootScope', '$http',
	function($scope, $rootScope, $http) {
		document.title = 'Пользователи';
		$rootScope.page = 2;
		$scope.request = '';
		$scope.page = 1;
		$scope.more = false;
		if ($rootScope.lastUserReq === undefined) {
			$rootScope.lastUserReq = '10';
			$http.post('/api/users/search', {'request': $rootScope.lastUserReq, 'page': 1}).then(function(data) {
				if(data.data.answer == 'ok') {
					$scope.results = data.data.results;
					$rootScope.lastUserRes = $scope.results;
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		} else {
			$scope.request = $rootScope.lastUserReq;
			$scope.results = $rootScope.lastUserRes;
		}
		$scope.set_ex = function(ex) {
			$scope.request = ex;
			$scope.search();
		}
		$scope.search = function() {
			$rootScope.lastUserReq = $scope.request;
			$scope.page = 1;
			$http.post('/api/users/search', {'request': $scope.request, 'page': $scope.page}).then(function(data) {
				if(data.data.answer == 'ok') {
					$scope.results = data.data.results;
					$rootScope.lastUserRes = $scope.results; 
					if(data.data.results.length == 30) {
						$scope.more = true;
					} else {
						$scope.more = false;
					}
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		};
		$scope.show_more = function() {
			$scope.page = $scope.page + 1;
			$http.post('/api/users/search', {'request': $scope.request, 'page': $scope.page}).then(function(data) {
				if(data.data.answer == 'ok') {
					for(var i = 0; i < data.data.results.length; i++) {
						$scope.results.push(data.data.results[i]);
					}
					$rootScope.lastUserRes = $scope.results; 
					if(data.data.results.length != 30) {
						$scope.more = false;
					}
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		};
		$(document).keypress(function(e) {
			if(e.which == 13) {
				$scope.search();
			}
		});
}]);


DigitalLibraryControllers.controller('HandlogCtrl', ['$scope', '$rootScope', '$http',
	function($scope, $rootScope, $http) {
		document.title = 'Журнал';
		$rootScope.page = 3;
		$scope.rootScope = angular.element(document.body).scope().$root;
		$scope.rootScope.handlog = [
			{
				'datetime': 'Загрузка...',
				'user': 'Загрузка...',
				'book': 'Загрузка...',
				'action': 'Загрузка...'
			}
		];
		$scope.rootScope.handlog_page = 0;
		$scope.rootScope.handlog_more = true;
		$scope.get_older = function() {
			if($scope.rootScope.handlog_page == -1) {
				$scope.rootScope.handlog_more = false;
				return;
			}
			$scope.rootScope.handlog_page++;
			$http.post('/api/info/handlog', {'page': $scope.rootScope.handlog_page}).then(function(data) {
				if(data.data.answer == 'ok') {
					if($scope.rootScope.handlog[0]['user'] == 'Загрузка...') {
						$scope.rootScope.handlog = [];
					}
					for(var i = 0; i < data.data.page.length; i++) {
						$scope.rootScope.handlog.push(data.data.page[i])
					}
					if(!data.data.more) {
						$scope.rootScope.handlog_page = -1;
						$scope.rootScope.handlog_more = false;
					}
				} else {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		};
		if($scope.rootScope.handlog[0]['user'] == 'Загрузка...') {
			$scope.get_older();
		}
}]);


DigitalLibraryControllers.controller('BookCtrl', ['$scope', '$rootScope', '$routeParams', '$http', '$cookies',
	function($scope, $rootScope, $routeParams, $http, $cookies) {
		$rootScope.page = 1;
		$scope.book = {'title': 'Ошибка'};
		$scope.titleEditing = false;
		$scope.authorEditing = false;
		$('[data-toggle="tooltip"]').tooltip()
		$http.post('/api/info/book', {
			'book': $routeParams.book_id
		}).then(function(data) {
			if(data.data.answer == 'ok') {
				$scope.book = data.data.book;
				$scope.book.count = parseInt($scope.book.count);
				$scope.book.handed = parseInt($scope.book.handed);
				$scope.titleEdited = $scope.book.title;
				$scope.authorEdited = $scope.book.author;
				document.title = $scope.book.title;
			} else if(data.data.answer == 'not_found') {
				window.location.replace("/404");
			} else {
				$cookies.put('session_id', '');
				location.reload();
			}
		});
		$(document).keypress(function(e) {
			if(e.which == 13) {
				if($scope.titleEditing) {
					$scope.saveTitle();
				}
				if($scope.authorEditing) {
					$scope.saveAuthor();
				}
			}
		});
		$scope.change_info = function() {
			$http.post('/api/book/change', {
				'title': $scope.book.title,
				'author': $scope.book.author,
				'count': $scope.book.count,
				'book': $routeParams.book_id
			}).then(function(data) {
				if(data.data.answer != 'ok') {
					$cookies.put('session_id', '');
					location.reload();
				}
			});
		};
		$scope.editTitle = function() {
			$scope.titleEditing = !$scope.titleEditing;
			setTimeout(function(){$('#title')[0].focus();}, 1);
		};
		$scope.editAuthor = function() {
			$scope.authorEditing = !$scope.authorEditing;
			setTimeout(function(){$('#author')[0].focus();}, 1);
		};
		$scope.saveTitle = function() {
			if($scope.titleEdited != '') {
				$scope.titleEditing = !$scope.titleEditing;
				$scope.book.title = $scope.titleEdited;
				$scope.change_info();
			}
		};
		$scope.saveAuthor = function() {
			if($scope.authorEdited != '') {
				$scope.authorEditing = !$scope.authorEditing;
				$scope.book.author = $scope.authorEdited;
				$scope.change_info();
			}
		};
		$scope.change_count = function(value) {
			if($scope.book.count + parseInt(value) >= 0 && $scope.book.count + parseInt(value) >= $scope.book.handed) {
				$scope.book.count += parseInt(value);
				$scope.change_info();
			}
		}
}]);


DigitalLibraryControllers.controller('UserCtrl', ['$scope', '$rootScope', '$routeParams', '$http', '$cookies',
	function($scope, $rootScope, $routeParams, $http, $cookies) {
		$rootScope.page = 1;
		$scope.book = {'name': 'Ошибка'};
		$http.post('/api/info/user', {
			'user': $routeParams.user_id
		}).then(function(data) {
			if(data.data.answer == 'ok') {
				$scope.user = data.data.user;
				document.title = $scope.user.name;
			} else if(data.data.answer == 'not_found') {
				window.location.replace("/404");
			} else {
				$cookies.put('session_id', '');
				location.reload();
			}
		});
		$scope.image = '/user.png';
}]);