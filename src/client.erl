%% Author: kevin.jingqiu
%% Created: Nov 18, 2009
%% Description: TODO: Add description to client
-module(client).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([register/1, info/0]).

%%
%% API Functions
%%
register(Nick) ->
    Reply = server:register(Nick),
    case Reply of
        ok ->
            io:format("Registration successful.~n");
        {name_exists, Nick} ->
            io:format("'~p' has already been registered.", Nick)
    end.

info() ->
    Reply = server:info(),
    case Reply of
        {ok, players, Players} ->
            io:format("Current players: ~p~n", [Players]);
        Any ->
            io:format("Received: ~p~n", Any)
    end.



%%
%% Local Functions
%%

