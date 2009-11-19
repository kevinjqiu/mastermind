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
-export([start/0, stop/0, register/1, show_players/0, show_rooms/0, enter_room/1, whoami/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {players, rooms}).
%% -record(room_details, {player1, player2}).

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

show_rooms() ->
	call(show_rooms).
 
enter_room(RoomName) ->
	call({enter_room, RoomName}).

whoami() ->
	call(whoami).

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
                rooms=ets:new(rooms, [])}}.

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
    case ets:lookup(Tab, ClientPid) of
        [{_, _Nick}] ->
			{reply, {fail, "You already registered"}, State};
        _ ->
			case ets:match(Tab, {'$1', Nick}) of
				[[_ClientPid]] ->
					{reply, {fail, string:concat(Nick, " has been taken")}, State};
				_ ->
			        ets:insert(Tab, {ClientPid, Nick}),
            		{reply, ok, State}
			end
    end;

handle_call(show_players, _From, State) ->
    {reply, {ok, ets:tab2list(State#state.players)}, State};

%% handle_call(show_rooms, _From, State) ->
%% 	RoomList = ets:tab2list(State#state.rooms),
%% 	Reply = {ok, lists:map(fun({Room, _})->Room end, RoomList)},
%% 	{reply, Reply, State};

%% handle_call({enter_room, RoomName}, _From, State) ->
%% 	ok;

handle_call(whoami, {ClientPid, _Tag}, State) ->
	PlayersTab = State#state.players,
	case ets:lookup(PlayersTab, ClientPid) of
		[{_, Nick}] ->
			{reply, {ok, Nick}, State};
		_ ->
			{reply, i_dont_know, State}
	end.

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

