-module(bingo_buzzwords).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([read_buzzwords/0]).
-export_type([buzzword/0]).

%% TODO: the application name is defined in "./bingo.app.src"
-define(APP_NAME, bingo).

-type buzzword() :: #{phrase => string(), points => integer()}.

%% Reads a CSV file of buzzwords an their respective point values, and
%% returns a list of buzzwords.
-spec read_buzzwords() -> [buzzword()].
read_buzzwords() ->
    PathToCSV = path_to_csv(),
    {ok, Binary} = file:read_file(PathToCSV),
    TBinary = string:trim(Binary),
    Lines = string:split(TBinary, "\n", all),
    [buzzword_from_binary(L) || L <- Lines].

%%
%% HELPERS.
%%

%% constructs a buzzword from a (comma separated) binary.
-spec buzzword_from_binary(binary()) -> buzzword().
buzzword_from_binary(Binary) when is_binary(Binary) ->
    Bin = string:trim(Binary),
    List = string:split(Bin, ",", all),
    [Phrase, Points] = [string:trim(Item) || Item <- List],
    #{
      phrase => binary_to_list(Phrase),
      points => binary_to_integer(Points)
     }.

%% Returns a path to CSV file (inside the priv directory) that contains
%% buzzwords.
-spec path_to_csv() -> file:filename().
path_to_csv() ->
    Priv = priv_dir(),
    filename:join(Priv, "buzzwords.csv").

%% Returns the path to the priv directory (that contains the CSV file).
-spec priv_dir() -> file:filename().
priv_dir() ->
    code:priv_dir(?APP_NAME).

%%
%% TESTS.
%%

-ifdef(TEST).

test_data() ->
    [
     {<<"Foo,100">>, #{phrase => "Foo", points => 100}},
     {<<" Foo, 100 ">>, #{phrase => "Foo", points => 100}},
     {<<" Foo , 100 ">>, #{phrase => "Foo", points => 100}},

     {<<"Foo And Bar,50">>, #{phrase => "Foo And Bar", points => 50}},
     {<<" Foo And Bar , 50 ">>, #{phrase => "Foo And Bar", points => 50}}
    ].

buzzword_from_binary_test_() ->
    TestData = test_data(),
    [?_assertEqual(E, buzzword_from_binary(I)) || {I, E} <- TestData].

-endif.
