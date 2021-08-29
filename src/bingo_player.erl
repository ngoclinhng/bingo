-module(bingo_player).
-include("bingo_player.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/2]).

%% Create a new player with the given `name` and `color`.
-spec new(player_name(), player_color()) -> #bingo_player{}.
new(Name, Color) ->
    #bingo_player{name = Name, color = Color}.

%%
%% TESTS.
%%

-ifdef(TEST).

create_bruno_player_test() ->
    Player = new("Bruno", green),
    Expected = #bingo_player{name = "Bruno", color = green},
    ?assertEqual(Expected, Player).

create_apple_player_test() ->
    Player = new("Apple", magenta),
    Expected = #bingo_player{name = "Apple", color = magenta},
    ?assertEqual(Expected, Player).

-endif.
