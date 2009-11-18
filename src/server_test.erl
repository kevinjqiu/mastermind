%% Author: kevin.jingqiu
%% Created: Nov 18, 2009
%% Description: TODO: Add description to server_test
-module(server_test).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
register_nick_test() ->
    {setup,
     fun() ->
        server:start(),
        server:register(jack),
        server:register(kate),
        server:register(hurley)
     end,
     fun(_) ->
        server:stop()
     end,
     fun(_) -> [?_assertMatch(ok, server:register(john)),
                 ?_assertMatch({name_exists, jack}, server:register(jack))]
     end}.




