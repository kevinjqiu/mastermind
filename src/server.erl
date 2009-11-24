%%% -------------------------------------------------------------------
%%% Author  : kevin.jingqiu
%%% Description :
%%%
%%% Created : Nov 18, 2009
%%% -------------------------------------------------------------------
-module(server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0, stop/0, 
		 register/1, show_players/0, whoami/0, 
		 create_game/1, show_games/0, join_game/1,
		 set_code/2, probe/2]).

-export([test/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {players,  %% is a ETS Tab of players {ClientPid, Nick} 
				games}    %% is a ETS Tab of games {GameName, GameDetails}
	                      %% where GameDetails is of record 'game'
	   ).

-record(game, {player1=undefined, %% player1 is of tuple {ClientPid, Nick, Code, Probes} 
			   player2=undefined, %% player2 is of the same type of tuple as player1 
			   status   %% status is of {open, setup, in_progress, completed}
			  }).

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    cast(stop).

register(Nick) ->
    call({register, Nick}).

show_players() ->
    call(show_players).
 
whoami() ->
	call(whoami).

create_game(GameName) ->
	call({create_game, GameName}).

show_games() ->
	call(show_games).

join_game(GameName) ->
	call({join_game, GameName}).

set_code(GameName, Code) ->
	case sanitize_input(Code) of
		{ok, SanitizedCode} ->
			call({set_code, GameName, SanitizedCode});
		Error ->
			Error
	end.

probe(GameName, Code) ->
	case sanitize_input(Code) of
		{ok, SanitizedCode} ->
			call({probe, GameName, SanitizedCode});
		Error ->
			Error
	end.


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{players=ets:new(players, []),
                games=ets:new(games, [])}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({register, Nick}, {ClientPid, _Tag}, State) ->
    Tab = State#state.players,
	case is_client_registered(Tab, ClientPid) of
		{true, _Nick} ->
			fail(already_registered, State);
		false ->
			case ets:match(Tab, {'$1', Nick}) of
				[[_ClientPid]] ->
					fail(nickname_exists, State);
				_ ->
			        ets:insert(Tab, {ClientPid, Nick}),
					ok(State)
			end
	end;

handle_call(show_players, _From, State) ->
	ok(ets:tab2list(State#state.players), State);

handle_call({create_game, GameName}, {ClientPid, _Tag}, State) ->
	PlayersTab = State#state.players,
	case is_client_registered(PlayersTab, ClientPid) of
		{true, Nick} ->
			GamesTab = State#state.games,
			case is_game_registered(GamesTab, GameName) of
				{true, _} ->
					fail(game_name_exists, State);
				_ ->
					GameDetails = #game{player1=new_player(ClientPid, Nick), status=open},
					ets:insert(GamesTab, {GameName, GameDetails}),
					ok(State)
			end;
		_ ->
			{reply, {error, not_registered}, State}
	end;

handle_call(show_games, _From, State) ->
	ok(ets:tab2list(State#state.games), State);

handle_call(whoami, {ClientPid, _Tag}, State) ->
	PlayersTab = State#state.players,
	case is_client_registered(PlayersTab, ClientPid) of
		{true, Nick} ->
			ok(Nick, State);
		_ ->
			fail(i_dont_know, State)
	end;

handle_call({join_game, GameName}, {ClientPid, _Tag}, State) ->
	case get_details(State, GameName, ClientPid) of
		{ok, nick, Nick, game, GameDetails} ->
			case join(GameName, GameDetails, ClientPid, Nick, State) of
				ok ->
					ok(State);
				Error ->
					fail(Error, State)
			end;
		Error ->
			fail(Error, State)
	end;

handle_call({set_code, GameName, Code}, {ClientPid, _Tag}, State) ->
	case get_details(State, GameName, ClientPid) of
		{ok, nick, Nick, game, GameDetails} -> 
			case set_code(State, GameName, GameDetails, ClientPid, Nick, Code) of
				ok ->
					ok(State);
				Error ->
					fail(Error, State)
			end;
		_ ->
			fail(not_registered, State)
	end;

handle_call({probe, GameName, Code}, {ClientPid, _Tag}, State) ->
	case get_details(State, GameName, ClientPid) of
		{ok, nick, Nick, game, GameDetails} ->
			{ok, result, Result} = probe(State, GameName, GameDetails, ClientPid, Nick, Code),
			case Result of
				{4,0} ->
					ok(you_win, State);
				_ ->
					ok(Result, State)
			end;
		_ ->
			fail(not_registered, State)
	end;
			

handle_call(Req, From, _State) ->
	io:format("Unknown request ~p from ~p~n", [Req, From]).


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
call(Request) ->
	gen_server:call({global, ?MODULE}, Request).

cast(Request) ->
	gen_server:cast({global, ?MODULE}, Request).

is_client_registered(PlayersTab, ClientPid) ->
    case ets:lookup(PlayersTab, ClientPid) of
        [{_, Nick}] ->
			{true, Nick};
        _ ->
			false
    end.
	
is_game_registered(GamesTab, GameName) ->
	case ets:lookup(GamesTab, GameName) of
		[{_, GameDetails}] ->
			{true, GameDetails};
		_ ->
			false
	end.

fail(Reason, State) ->
	{reply, {fail, Reason}, State}.

ok(State) ->
	{reply, ok, State}.

ok(Return, State) ->
	{reply, {ok, Return}, State}.

get_details(State, GameName, ClientPid) ->
	PlayersTab = State#state.players,
	case is_client_registered(PlayersTab, ClientPid) of
		false ->
			{error, not_registered};
		{true, Nick} ->
			GamesTab = State#state.games,
			case is_game_registered(GamesTab, GameName) of
				false ->
					{error, game_not_exist};
				{true, GameDetails} ->
					{ok, nick, Nick, game, GameDetails}
			end
	end.

join(GameName, GameDetails, ClientPid, Nick, State)->
	case GameDetails#game.player1 of
		{ClientPid, Nick, _, _} ->
			{error, cant_play_with_self};
		_ ->
			case GameDetails#game.player2 of
				undefined ->
					NewGameDetails = GameDetails#game{player2=new_player(ClientPid, Nick), status=setup},
					ets:insert(State#state.games, {GameName, NewGameDetails}),
					ok;
				_ ->
					{error, game_is_full}
			end
	end.

new_player(ClientPid, Nick) ->
	{ClientPid, Nick, undefined, 0}.

save_gamedetails(State, GameName, NewGameDetails) ->
	ets:insert(State#state.games, {GameName, NewGameDetails}).

alter_gamedetails(GameDetails, Key, Value) ->
	IsPlayerCodeSet = fun(Player) ->
							  case Player of
								  {_, _, undefined, _} ->
									  false;
								  _ ->
									  true
							  end
					  end,	
	case Key of
		player1 ->
			case IsPlayerCodeSet(Value) and IsPlayerCodeSet(GameDetails#game.player2) of
				true ->
					GameDetails#game{player1=Value, status=in_progress};
				_ ->
					GameDetails#game{player1=Value}
			end;
		player2 ->
			case IsPlayerCodeSet(Value) and IsPlayerCodeSet(GameDetails#game.player1) of
				true ->
					GameDetails#game{player2=Value, status=in_progress};
				_ ->
					GameDetails#game{player2=Value}
			end;
		status ->
			GameDetails#game{status=Value}
	end.
	

set_code(State, GameName, GameDetails, ClientPid, Nick, Code) ->
	SetCodeToPlayer = fun(Player) ->
					  case Player of
						  {_ClientPid, _Nick, undefined, 0} ->
							  {_ClientPid, _Nick, Code, 0};
						  _ ->
							  {error, code_already_set}
					  end
			  end,

	case GameDetails#game.status of
		open ->
			{error, waiting_for_opponent};
		setup ->
			case which_player_in_game(GameDetails, ClientPid, Nick) of
				player1 ->
					save_gamedetails(
					  State, GameName, 
					  alter_gamedetails(GameDetails, player1, SetCodeToPlayer(GameDetails#game.player1))),
					%% XXX: do 'case' on the return of ets:insert()?
					ok;
				player2 ->
					save_gamedetails(
					  State, GameName, 
					  alter_gamedetails(GameDetails, player2, SetCodeToPlayer(GameDetails#game.player2))),
					ok;
				{error, Reason} ->
					{error, Reason}
			end;
		_ ->
			{error, game_already_started}
	end.

probe(State, GameName, GameDetails, ClientPid, Nick, Code) ->
	IncProbes = fun(Player) ->
						case Player of
							{_ClientPid, _Nick, _Code, Probes} ->
								{_ClientPid, _Nick, _Code, Probes+1};
							_ ->
								{error, cant_increment_probes}
						end
				end,
	
	case GameDetails#game.status of
		in_progress ->
			case GameDetails#game.player1 of
				{ClientPid, Nick, TargetCode, _NumOfProbes} ->
					save_gamedetails(State, GameName, alter_gamedetails(GameDetails, player1, IncProbes(GameDetails#game.player1))),
					test(TargetCode, Code);
				_ ->
					case GameDetails#game.player2 of
						{ClientPid, Nick, TargetCode, _NumOfProbes} ->
							save_gamedetails(State, GameName, alter_gamedetails(GameDetails, player2, IncProbes(GameDetails#game.player2))),
							test(TargetCode, Code);
						_ ->
							{error, not_a_player}
					end
			end;
		_ ->
			{error, game_not_in_progress}
	end.

test(Target, Code) ->
	{lists:sum([case lists:nth(I, Target)-lists:nth(I, Code)==0 of true->1; false->0 end || I <- lists:seq(1, length(Target))]),
	length(sets:to_list(sets:intersection(sets:from_list(Target), sets:from_list(Code))))}.

sanitize_input(Code) ->
	case is_integer(Code) of
		true ->
			case ((Code > 999) and (Code < 10000)) of
				true ->
					{ok, get_digit_list(Code, [])};
				false ->
					{error, code_must_contain_four_digits}
			end;
		false ->
			{error, code_must_be_integer}
	end.

get_digit_list(Num, DigitList) ->
	case Num =:= 0 of
		true ->
			DigitList;
		_ ->
			get_digit_list(Num div 10, DigitList) ++ [Num rem 10]
	end.

which_player_in_game(GameDetails, ClientPid, Nick) ->
	case GameDetails#game.player1 of
		{ClientPid, Nick, _, _} ->
			player1;
		_ ->
			case GameDetails#game.player2 of
				{ClientPid, Nick, _, _} ->
					player2;
				_ ->
					{error, not_in_game}
			end
	end.

