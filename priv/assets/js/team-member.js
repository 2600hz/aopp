'use strict';

angular.module('team-member', ['ngWebsocket', 'xeditable', 'angular-growl', 'ngAnimate'])
    .config(['growlProvider', function(growlProvider) {
        growlProvider.globalTimeToLive(3000);
        growlProvider.onlyUniqueMessages(false);
    }])
    .factory('CardsService', function() {
        return {
            cards: [
                {"display":"0","value":0,"cssClass":"card-blue"},
                {"display":"1","value":1,"cssClass":"card-blue"},
                {"display":"2","value":2,"cssClass":"card-blue"},
                {"display":"3","value":3,"cssClass":"card-blue"},
                {"display":"5","value":5,"cssClass":"card-blue"},
                {"display":"8","value":8,"cssClass":"card-blue"},
                {"display":"13","value":13,"cssClass":"card-green"},
                {"display":"20","value":20,"cssClass":"card-green"},
                {"display":"40","value":40,"cssClass":"card-green"},
                {"display":"100","value":100,"cssClass":"card-green"},
                {"display":"?","value":"?","cssClass":"card-orange"},
                {"display":"Pass","value":"Pass","cssClass":"card-drk-grey"},
                {"display":"break","value":"break","cssClass":"card-drk-grey"}
            ]
        };
    })
    .factory('ws', function ($websocket, growl) {
        return $websocket.$new('ws://10.26.0.139:8080/websocket');
    })
    .run(function(editableOptions) {
        editableOptions.theme = 'bs3';
    })
    .run(function(ws, growl) {
        ws.$on('$open', function () {
            console.log('websocket connected');
            growl.success('websocket connected');
        });
        ws.$on('$close', function () {
            console.log('websocket closed');
            growl.error('websocket closed');
        });
        ws.$on('error', function (event) {
            growl.error(event.message);
        });
        ws.$on('success', function (event) {
            growl.success(event.message);
        });
    })
    .controller('UserCtrl', function($scope, ws, growl) {
        $scope.users = [];
        $scope.my_id = 0;
        $scope.updateUser = function(name, id) {
            ws.$emit('update_name', {'name': name, 'id': id});
        };
        ws.$on('initial', function (event) {
            $scope.users = event.participants;
            angular.forEach($scope.users, function(el) {
                if (el.id == event.myself.id) {
                    el.myself = true;
                }
            });
            $scope.my_id = event.myself.id;
            $scope.$digest();
        });
        ws.$on('new_participant', function (event) {
            $scope.users.push(event);
            $scope.$digest();
        });
        ws.$on('remove_participant', function (event) {
            $scope.users =
                $scope.users.filter(function (el) {
                    return el.id !== event.id;
                });
            $scope.$digest();
        });
        ws.$on('update_participant', function (event) {
            angular.forEach($scope.users, function(el) {
                if (el.id == event.id) {
                    el.name = event.name;
                    el.moderator = event.moderator;
                }
            });
            $scope.$digest();
        });
    })
    .controller('TimerCtrl', function($scope, ws, $interval) {
        $scope.timerHHMMSS = 0;
        ws.$on('tick', function (event) {
            $scope.timerHHMMSS = event.timerHHMMSS;
            $scope.$digest();
        });
    })
    .controller('TicketCtrl', function($scope, ws) {
        $scope.ticket = {
            key: "",
            summary: "",
            description: "",
            acceptance_criteria: ""
        };
        $scope.waiting = true;
        ws.$on('initial', function (event) {
            if (event.ticket != "undefined") {
                $scope.ticket = event.ticket;
                $scope.waiting = false;
                $scope.$digest();
            }
        });
        ws.$on('loading_ticket', function (event) {
            $scope.waiting = true;
            $scope.ticket.key = "";
            $scope.$digest();
        }),
        ws.$on('current_ticket', function (event) {
            $scope.ticket = event;
            $scope.waiting = false;
            $scope.$digest();
        });
    })
    .controller('CardsCtrl', function($scope, ws, growl, CardsService) {
        $scope.cards = CardsService.cards;
        $scope.vote = null;
        $scope.enabled = false;
        $scope.my_id = null;
        $scope.variance = function (x) {
            return Math.random()*2*x - x;
        };
        $scope.spinBack = function (card) {
            var $perspectiveWrapper = $('.perspective-wrapper', card);
            var $animationWrapper = $('.animation-wrapper', card);
            TweenMax.to($animationWrapper, 0.2, {
                y: 0,
                rotationX: 0,
                //rotationY: 180,
                rotationZ: 0,
                ease: 'cubic-bezier(.03,0,1,.03)'
            });
        };
        $scope.spinOut = function (card) {
            var $perspectiveWrapper = $('.perspective-wrapper', card);
            var $animationWrapper = $('.animation-wrapper', card);
            if(window.innerWidth >= 768) {
                TweenMax.to($animationWrapper, 0.2, {
                    y: -120 + $scope.variance(20),
                    rotationX: 0 + $scope.variance(10),
                    //rotationY: 180 + $scope.variance(5),
                    rotationZ: 0 + $scope.variance(15),
                    ease: 'cubic-bezier(.03,0,1,.03)'
                });
            } else {
                TweenMax.to($animationWrapper, 0.2, {
                    y: -70 + $scope.variance(20),
                    rotationX: 0 + $scope.variance(10),
                    //rotationY: 180 + $scope.variance(5),
                    rotationZ: 0 + $scope.variance(15),
                    ease: 'cubic-bezier(.03,0,1,.03)'
                });
            }
        };
        $scope.resetCards = function () {
            var $selectedCard;

            $scope.vote = null;

            //get cards, and determine if a new one was selected
            $selectedCard = $('.card-in-hand.selected');
            if($selectedCard.length <= 0) $selectedCard = null;

            if($selectedCard) {
                $selectedCard.removeClass('selected');
                $scope.spinBack($selectedCard, 'auto', 0);
            }
        };
        $scope.cardSelect = function (card) {
            var votecard, dCard, $selectedCard, $newCard;

            for(var x=0; x < $scope.cards.length; x++){
                if($scope.cards[x].value == card){
                    votecard = $scope.cards[x];
                }
            }
            dCard = votecard.display;

            //get cards, and determine if a new one was selected
            $selectedCard = $('.card-in-hand.selected');
            if($selectedCard.length <= 0) $selectedCard = null;

            // Set new card
            $('.card-in-hand').each(function () {
                if ($(this).find('.player-vote span')[1].innerHTML == dCard) {
                    $newCard = $(this);
                }
            });

            //deselect current card (if there is one)
            if($selectedCard) {
                $selectedCard.removeClass('selected');
                $scope.spinBack($selectedCard, 'auto', 0);
            }

            if (!$scope.enabled) {
                growl.info('Voting is currently disabled!');
                $scope.vote = null;
                return;
            }

            //select new card and change state
            $selectedCard = $newCard;

            if($selectedCard) {
                $selectedCard.addClass('selected');
                $scope.spinOut($selectedCard);
            }

            if (card == $scope.vote) {
                return;
            }

            ws.$emit('vote', {value: card});
            $scope.vote = card;
        };
        ws.$on('initial', function (event) {
            $scope.enabled = event.votes.enabled;
            $scope.my_id = event.myself.id;
            $scope.$digest();
        });
        ws.$on('votes_update', function (event) {
            var found = false;
            angular.forEach(event.votes, function(el) {
                if (el.id == $scope.my_id) {
                    found = true;
                }
            });
            if (!found || !event.enabled) {
                $scope.resetCards();
            }
            $scope.enabled = event.enabled;
            $scope.$digest();
        });
    })
    .controller('VotesCtrl', function($scope, ws, CardsService) {
        $scope.votes = [];
        $scope.enabled = false;

        $scope.update_votes = function(event) {
            var cardsByValue = {};
            $scope.votes = event.votes;
            $scope.enabled = event.enabled;
            angular.forEach(CardsService.cards, function(el) {
                cardsByValue[el.value] = el;
            });
            angular.forEach($scope.votes, function(el) {
                if (cardsByValue[el.value]) {
                    el.cssClass = cardsByValue[el.value].cssClass;
                } else {
                    el.cssClass = 'card-blue';
                }
            });
            console.log($scope.votes);
        };
        ws.$on('initial', function (event) {
            $scope.update_votes(event.votes);
            $scope.$digest();
        });
        ws.$on('votes_update', function (event) {
            $scope.update_votes(event);
            $scope.$digest();
        });
        ws.$on('update_participant', function (event) {
            angular.forEach($scope.votes, function(el) {
                if (el.id == event.id) {
                    el.name = event.name;
                    el.moderator = event.moderator;
                }
            });
            $scope.$digest();
        });
    });
