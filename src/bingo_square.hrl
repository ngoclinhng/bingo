%% File: bingo_square.hrl

-include("bingo_player.hrl").

%% Date Type: bingo_square
%% where:
%%     phrase: A string.
%%     points: An integer.
-record(bingo_square, {
    phrase :: string(),
    points :: integer(),
    marked_by = nil :: nil | bingo_player()
}).

-type bingo_square() :: #bingo_square{}.
