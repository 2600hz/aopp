-module(aopp_ticket).

-export([from_map/1]).
-export([to_map/1]).
-export([id/1]).
-export([key/1]).
-export([self/1]).
-export([summary/1]).
-export([description/1]).
-export([acceptance_criteria/1]).
-export([story_points/1
        ,story_points_binary/1
        ,set_story_points/2
        ]).

-record(ticket, {id :: binary()
                 ,key :: binary()
                 ,self :: binary()
                 ,summary :: binary()
                 ,description :: binary()
                 ,acceptance_criteria :: binary()
                 ,story_points :: non_neg_integer() | 'undefined'
                }).
-type ticket() :: #ticket{}.
-export_type([ticket/0]).

-spec from_map(map()) -> ticket().
from_map(Map) ->
    Fields = maps:get(<<"fields">>, Map, #{}),
    #ticket{id=maps:get(<<"id">>, Map)
           ,key=maps:get(<<"key">>, Map)
           ,self=maps:get(<<"self">>, Map)
           ,summary=maps:get(<<"summary">>, Fields, 'undefined')
           ,description=maps:get(<<"description">>, Fields, 'undefined')
           ,acceptance_criteria=maps:get(<<"customfield_11530">>, Fields, 'undefined')
           ,story_points=maps:get(<<"customfield_10002">>, Fields, 'undefined')
           }.

-spec to_map(ticket()) -> map().
to_map(#ticket{}=Ticket) ->
    maps:filter(
      fun(_, 'undefined') -> 'false';
         (_, _) -> 'true'
      end,
      #{<<"id">> => id(Ticket)
       ,<<"key">> => key(Ticket)
       ,<<"summary">> => summary(Ticket)
       ,<<"description">> => description(Ticket)
       ,<<"acceptance_criteria">> => acceptance_criteria(Ticket)
       ,<<"story_points">> => story_points(Ticket)
       }
     ).

-spec id(ticket()) -> binary().
id(#ticket{id=Id}) ->
    Id.

-spec key(ticket()) -> binary().
key(#ticket{key=Key}) ->
    Key.

-spec self(ticket()) -> binary().
self(#ticket{self=Self}) ->
    Self.

-spec summary(ticket()) -> binary().
summary(#ticket{summary=Summary}) ->
    Summary.

-spec description(ticket()) -> binary().
description(#ticket{description=Description}) ->
    Description.

-spec acceptance_criteria(ticket()) -> binary().
acceptance_criteria(#ticket{acceptance_criteria=AcceptanceCriteria}) ->
    AcceptanceCriteria.

-spec story_points(ticket()) -> non_neg_integer().
story_points(#ticket{story_points=StoryPoints}) ->
    StoryPoints.

-spec story_points_binary(ticket()) -> binary().
story_points_binary(#ticket{story_points=StoryPoints}) when is_integer(StoryPoints) ->
    integer_to_binary(StoryPoints);
story_points_binary(_) -> <<"0">>.

-spec set_story_points(non_neg_integer(), ticket()) -> ticket().
set_story_points(StoryPoints, Ticket) when is_integer(StoryPoints) ->
    Ticket#ticket{story_points=StoryPoints}.
