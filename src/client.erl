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
	case server:whoami() of
		{ok, MyName} ->
			io:format("You're ~p~n", [MyName]);
		Response ->
			print_exception(Response)
	end.

register(Nick) ->
    Reply = server:register(Nick),
    case Reply of
        ok ->
            io:format("Registration successful.~n");
		Response ->
			print_exception(Response)			
    end.

show_players() ->
	case server:show_players() of
		{ok, Players} -> 
			io:format("Current players: ~p~n", lists:map(fun({_Pid, Nick})->Nick end, Players));
		Response ->
			print_exception(Response)
	end.

create_game(GameName) ->
	case server:create_game(GameName) of
		ok ->
			io:format("~p created~n", [GameName]);
		Response ->
			print_exception(Response)
	end.

join_game(GameName) ->
	case server:join_game(GameName) of
		ok ->
			io:format("You're now in game ~p~n", [GameName]);
		Response ->
			print_exception(Response)
	end.

show_games() ->
	case server:show_games() of
		{ok, Games} -> 
			io:format("Games: ~p~n", [Games]);
		Response ->
			print_exception(Response)
	end.

set_code(Game, Code) ->
	case server:set_code(Game, Code) of
		ok ->
			io:format("Your code has been set~n");
		Response ->
			print_exception(Response)
	end.

		
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

