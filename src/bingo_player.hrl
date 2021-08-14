%% File: bingo_player.hrl

%% Date type: bingo_player
%% where:
%%     name: player's name
%%     color: player's favorite color.
-record(bingo_player, {
    name :: string(),
    color :: string()
}).

-type bingo_player() :: #bingo_player{}.
