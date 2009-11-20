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
		 create_game/1, show_games/0, join_game/1]).

%%
%% API Functions
%%
whoami() ->
	case server:whoami() of
		{ok, MyName} ->
			io:format("You're ~p~n", [MyName]);
		_ ->
			io:format("The server doesn't know you.~n")
	end.

register(Nick) ->
    Reply = server:register(Nick),
    case Reply of
        ok ->
            io:format("Registration successful.~n");
		{error, Reason} ->
			io:format("~p~n", [Reason])			
    end.

show_players() ->
    {ok, Players} = server:show_players(),
    io:format("Current players: ~p~n", lists:map(fun({_Pid, Nick})->Nick end, Players)).

create_game(GameName) ->
	case server:create_game(GameName) of
		ok ->
			io:format("~p created~n", [GameName]);
		{error, Reason} ->
			io:format("Error: ~p~n", [Reason])
	end.

join_game(GameName) ->
	case server:join_game(GameName) of
		ok ->
			io:format("You're now in game ~p~n", [GameName]);
		{error, Reason} ->
			io:format("Error: ~p~n", [Reason])
	end.

show_games() ->
	{ok, Games} = server:show_games(),
	io:format("Games: ~p~n", [Games]).


%% show_rooms() ->
%% 	{ok, Rooms} = server:show_rooms(),
%% 	io:format("Current rooms: ~p~n", [Rooms]).

%% enter_room(RoomName) ->
%% 	case server:enter_room(RoomName) of
%% 		ok ->
%% 			io:format("You're now in room ~p~n", RoomName);
%% 		{error, Reason} ->
%% 			io:format("Cannot enter room: ~p~n", Reason)
%% 	end.

%%
%% Local Functions
%%

