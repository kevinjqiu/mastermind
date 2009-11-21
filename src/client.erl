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
-export([register/1, show_players/0, whoami/0, 
		 create_game/1, show_games/0, join_game/1,
		 set_code/2]).

%%-export([rpc/4]).

%%
%% API Functions
%%
whoami() ->
	rpc(fun server:whoami/0, [], fun(Result)->io:format("You're ~p~n", [Result]) end).

register(Nick) ->
	rpc(fun server:register/1, [Nick], fun()->io:format("Registration successful") end).

show_players() ->
	rpc(fun server:show_players/0, 
		[], 
		fun(Result) ->
				io:format("~Current Players: ~p~n", lists:map(fun({_Pid, Nick})->Nick end, Result)) end).

create_game(GameName) ->
	rpc(fun server:create_game/1, [GameName], fun(Result)->io:format("~p created~n", [Result]) end).

join_game(GameName) ->
	rpc(fun server:join_game/1, [GameName], fun(Result)->io:format("You're now in game ~p~n", [Result]) end).

show_games() ->
	rpc(fun server:show_games/0, [], fun(Result)->io:format("Games: ~p~n", [Result]) end).

set_code(Game, Code) ->
	rpc(fun server:set_code/2, [Game, Code], fun()->io:format("Your code has been set~n") end).
		
%%
%% Local Functions
%%

print_exception(Response) ->
	case Response of
		{error, Reason} ->
			io:format("Error: ~p~n", [Reason]);
		_ ->
			io:format("Server returns: ~p~n", [Response])
	end.

rpc(Fun, Args, OnSuccess, OnError) ->
	case apply(Fun, Args) of
		ok ->
			OnSuccess();
		{ok, Result} ->
			OnSuccess(Result);
		{error, Reason} ->
			OnError(Reason)
	end.

rpc(Fun, Args, OnSuccess) ->
	rpc(Fun, Args, OnSuccess, fun print_exception/1).
