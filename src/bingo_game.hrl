-include("bingo_square.hrl").

-type bingo_scores() :: #{player_name() := bingo_points()}.
-type bingo_squares() :: [[bingo_square()]].

-record(bingo_game, {
    squares :: bingo_squares(),
    scores = #{} :: bingo_scores(),
    winner :: bingo_player() | undefined
}).

-type bingo_game() :: #bingo_game{}.
