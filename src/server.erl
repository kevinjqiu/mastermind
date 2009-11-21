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
		 set_code/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {players,  %% is a ETS Tab of players {ClientPid, Nick} 
				games}    %% is a ETS Tab of games {GameName, GameDetails}
	                      %% where GameDetails is of record 'game'
	   ).

-record(game, {player1, %% player1 is of tuple {ClientPid, Nick, Code, Probes} 
			   player2, %% player2 is of the same type of tuple as player1 
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
	call({set_code, GameName, Code}).

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
					GameDetails = #game{player1={ClientPid, Nick}, status=open},
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
	case is_client_registered(State#state.players, ClientPid) of
		{true, Nick} ->
			case is_game_registered(State#state.games, GameName) of
				{true, GameDetails} ->
					case GameDetails#game.player1 of
						{ClientPid, Nick, _, _} ->
							fail(cant_play_with_self, State);
						_ ->
							case GameDetails#game.player2 of
								undefined ->
									NewGameDetails = GameDetails#game{player2={ClientPid, Nick}, status=in_progress},
									ets:insert(State#state.games, {GameName, NewGameDetails}),
									ok(State);
								_ ->
									fail(game_is_full, State)
							end
						end;
				_ ->
					fail(game_not_exist, State)
			end;
		_ ->
			fail(not_registered, State)
	end;

%%
%% Find the GameDetails of the specified game
handle_call({set_code, GameName, Code}, {ClientPid, _Tag}, State) ->
	case is_client_registered(State#state.players, ClientPid) of
		{true, Nick} ->
			case is_game_registered(State#state.games, GameName) of
				{true, GameDetails} ->
					case GameDetails#game.player1 of
						{ClientPid, Nick, _, Probes} ->
							NewGameDetails = GameDetails#game{player1={ClientPid, Nick, Code, Probes}},
							ets:insert(State#state.games, {GameName, NewGameDetails}),
							ok(State);
						_ ->
							case GameDetails#game.player2 of
								{ClientPid, Nick, _, Probes} ->
									NewGameDetails = GameDetails#game{player2={ClientPid, Nick, Code, Probes}},
									ets:insert(State#state.games, {GameName, NewGameDetails}),
									ok(State);
								_ ->
									fail(you_arent_in_the_game, State)
							end
					end;
				_ ->
					fail(game_not_exist, State)
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

