'use strict';

angular.module('scrum-master', ['team-member'])
    .run(function(ws, growl) {
        ws.$on('$open', function () {
            ws.$emit('promote', {});
        });
    })
    .controller('JiraCtrl', function($scope, ws, growl) {
        $scope.config = {
            username: "",
            password: "",
            base_url: "https://2600hz.atlassian.net",
            jql: ""
        };
        $scope.state = {
            configure: true,
            list: false
        };
        $scope.tickets = [];
        $scope.configure = function(config) {
            $scope.config = angular.copy(config);
            ws.$emit('configure_tickets', $scope.config);
        };
        $scope.loadTickets = function(config) {
            $scope.config =  angular.copy(config);
            ws.$emit('load_tickets', {jql: $scope.config.jql});
        };
        $scope.loadTicket = function(ticket) {
            ws.$emit('load_ticket', {key: ticket.key});
            angular.forEach($scope.tickets, function(el) {
                if (el.key == ticket.key) {
                    el.selected = true;
                } else {
                    el.selected = false;
                }
            });
        };
        ws.$on('jira_logged_in', function (event) {
            growl.success('connected to jira');
            $scope.state = {
                configure: false,
                list: true
            };
            $scope.$digest();
        });
        ws.$on('tickets_loaded', function (event) {
            $scope.tickets = event;
            $scope.$digest();
        });
        ws.$on('update_ticket', function (event) {
            angular.forEach($scope.tickets, function(el) {
                if (el.key == event.key) {
                    el.story_points = event.story_points;
                }
            });
            $scope.$digest();
        });
    })
    .controller('TimerCtrl', function($scope, ws, $interval) {
        var timer;
        $scope.timerRunning = false;
        $scope.timerDuration = 300;
        $scope.timerValue = $scope.timerDuration;
        $scope.timerHHMMSS = $scope.timerValue.toHHMMSS();

        $scope.startTimer = function() {
            if (angular.isDefined(timer)) {
                return;
            }
            if ($scope.timerValue <= 0) {
                return;
            }
            $scope.timerRunning = true;
            timer = $interval(function() {
                $scope.timerValue--;
                $scope.timerHHMMSS = $scope.timerValue.toHHMMSS();
                ws.$emit('tick', {timerHHMMSS: $scope.timerHHMMSS});
                if ($scope.timerValue <= 0) {
                    ws.$emit('votes_enabled', {enable: false});
                    ws.$emit('votes_visibility', {visible: true});
                    $scope.stopTimer();
                }
            }, 1000);
        };
        $scope.stopTimer = function() {
            $scope.timerRunning = false;
            if (angular.isDefined(timer)) {
                $interval.cancel(timer);
                timer = undefined;
            }
        };
        $scope.resetTimer = function() {
            $scope.timerValue = $scope.timerDuration;
            $scope.timerHHMMSS = $scope.timerValue.toHHMMSS();
            ws.$emit('tick', {timerHHMMSS: $scope.timerHHMMSS});
        };
        $scope.updateDuration = function(duration) {
            $scope.timerDuration = parseInt(duration);
            $scope.timerValue = $scope.timerDuration;
            $scope.timerHHMMSS = $scope.timerValue.toHHMMSS();
            ws.$emit('tick', {timerHHMMSS: $scope.timerHHMMSS});
        };
        $scope.toggleTimer = function (){
            $scope.timerRunning = !$scope.timerRunning;
            if ($scope.timerRunning) {
                $scope.startTimer();
            } else {
                $scope.stopTimer();
            }
        };
        ws.$on('current_ticket', function(event){
            $scope.resetTimer();
            if (!$scope.timerRunning) {
                $scope.toggleTimer();
            }
            $scope.$digest();
        });
        ws.$on('votes_update', function (event) {
            if (!event.enabled && event.values_visible) {
                if ($scope.timerRunning) {
                    $scope.stopTimer();
                }
                $scope.resetTimer();
            }
            $scope.$digest();
        });
    })
    .controller('VotesCtrl', function($scope, ws, CardsService) {
        $scope.votes = [];
        $scope.valuesVisible = false;
        $scope.enabled = false;
        $scope.storyPoints = 0;

        $scope.toggleVoting = function() {
            $scope.enabled = !$scope.enabled;
            ws.$emit('votes_enabled', {enable: $scope.enabled});
        };
        $scope.toggleVisible = function() {
            $scope.valuesVisible = !$scope.valuesVisible;
            ws.$emit('votes_visibility', {visible: $scope.valuesVisible});
        };
        $scope.reset = function() {
            ws.$emit('votes_reset', {});
        };
        $scope.setStoryPoints = function () {
            ws.$emit('set_story_points', {story_points: $scope.storyPoints});

        };
        $scope.update_votes = function(event) {
            var cardsByValue = {};
            $scope.votes = event.votes;
            $scope.enabled = event.enabled;
            $scope.valuesVisible = event.values_visible;
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
        };
        $scope.findStoryPoints = function () {
            var values = {}, max_count = 0, majority_value = 0;
            angular.forEach($scope.votes, function(el) {
                if (typeof el.value == 'number') {
                    if (!values[el.value]) {
                        values[el.value] = {
                            count: 1,
                            value: el.value
                        };
                    } else {
                        values[el.value].count++;
                    }
                }
            });
            angular.forEach(values, function(el) {
                if (el.count > max_count) {
                    max_count = el.count;
                    majority_value = el.value;
                }
                if (el.count == max_count && el.value > majority_value) {
                    majority_value = el.value;
                }
            });
            $scope.storyPoints = majority_value;
        };
        ws.$on('initial', function (event) {
            $scope.update_votes(event.votes);
            $scope.findStoryPoints();
            $scope.$digest();
        });
        ws.$on('votes_update', function (event) {
            $scope.update_votes(event);
            $scope.findStoryPoints();
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


Number.prototype.toHHMMSS = function () {
    var seconds = Math.floor(this),
        hours = Math.floor(seconds / 3600);
    seconds -= hours*3600;
    var minutes = Math.floor(seconds / 60);
    seconds -= minutes*60;

    if (hours   < 10) {hours   = "0"+hours;}
    if (minutes < 10) {minutes = "0"+minutes;}
    if (seconds < 10) {seconds = "0"+seconds;}
    return minutes + ':' + seconds;
};
