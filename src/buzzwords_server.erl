-module(buzzwords_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([get_buzzwords/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(REFRESH_INTERVAL, 3600000).  % 60 minutes

%%
%% Client public API.
%%

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_buzzwords() ->
    gen_server:call(?SERVER, get_buzzwords).

%%
%% Server callbacks.
%%

init([]) ->
    State = load_buzzwords(),
    schedule_refresh(),
    {ok, State}.

handle_call(get_buzzwords, _From, State) ->
    {reply, State, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(refresh, _State) ->
    State = load_buzzwords(),
    schedule_refresh(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Internal functions.
%%

schedule_refresh() ->
    erlang:send_after(?REFRESH_INTERVAL, self(), refresh).

load_buzzwords() ->
    bingo_buzzwords:read_buzzwords().
