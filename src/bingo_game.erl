-module(bingo_game).

-include("bingo_game.hrl").

-export([new/2]).

%% Creates a game with a `size` x `size` collection of squares taken
%% randomly from the given list of `buzzwords` where each buzzword is of
%% the form `#{phrase => string(), points => integer()}`.
-spec new(pos_integer(), [bingo_buzzwords:buzzword()]) -> bingo_game().
new(_Size, _Buzzwords) ->
    ok.
