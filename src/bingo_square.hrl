%% File: bingo_square.hrl

-include("bingo_player.hrl").

-type bingo_phrase() :: string().
-type bingo_points() :: integer().

%% Date Type: bingo_square
%% where:
%%     phrase: A string.
%%     points: An integer.
-record(bingo_square, {
    phrase :: bingo_phrase(),
    points :: bingo_points(),
    marked_by :: undefined | bingo_player()
}).

-type bingo_square() :: #bingo_square{}.
