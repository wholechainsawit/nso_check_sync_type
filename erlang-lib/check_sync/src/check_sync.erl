-module(check_sync).

-behaviour(application).

%% API
%% -export([ping/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% ping() ->
%%     tailf_hcc_server:ping().

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case check_sync_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
