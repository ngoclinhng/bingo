%% File: bingo_player.hrl

-type player_name() :: string().
-type player_color() :: red
                        | green
                        | yellow
                        | blue
                        | magenta
                        | cyan.

%% Date type: bingo_player
%% where:
%%     name: player's name
%%     color: player's favorite color.
-record(bingo_player, {
    name :: player_name(),
    color :: player_color()
}).

-type bingo_player() :: #bingo_player{}.
