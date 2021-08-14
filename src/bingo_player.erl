-module(bingo_player).
-include("bingo_player.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/2]).

%% Create a new player with the given `name` and `color`.
-spec new(string(), string()) -> #bingo_player{}.
new(Name, Color) ->
    #bingo_player{name = Name, color = Color}.

%%
%% TESTS.
%%

-ifdef(TEST).

create_bruno_player() ->
    Player = new("Bruno", "green"),
    Expected = #bingo_player{name = "Bruno", color = "green"},
    Player =:= Expected.

create_apple_player() ->
    Player = new("Apple", "black"),
    Expected = #bingo_player{name = "Apple", color = "black"},
    Player =:= Expected.

new_test_() ->
    [
     ?_assert(create_bruno_player()),
     ?_assert(create_apple_player())
    ].

-endif.
