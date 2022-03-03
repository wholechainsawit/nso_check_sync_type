%% check sync server.
-module(check_sync_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% internal exports
-export([notif_init/0, cdb_init/0]).

-include("econfd.hrl").

-define(SERVER, ?MODULE).

-define(ns, 'http://cisco.com/pkg/tailf-hcc').
-define(ncs_ns, 'http://tail-f.com/ns/ncs').
-define(hcc_ikp, [[?ns|'hcc']]).
-define(ncs_ha_ikp, [[?ncs_ns|'high-availability']]).

-record(state,
        {role=disabled  :: master | slave | disabled,
         vips=[]        :: [string()], %% VIP address list (IPv4/IPv6)
         vipctl=""      :: string(), %% path to vipctl helper script
         on_exit_vipctl :: undefined | port() %% to run vipctl down
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true), % Triggers call to terminate/2
    econfd:log(?CONFD_LEVEL_TRACE, "~p: Server starting...", [?SERVER]),
%%    proc_lib:start_link(?MODULE, notif_init, []),
%%    proc_lib:start_link(?MODULE, cdb_init, []),
    init_confd_callbacks(),
    State = #state{},
    {ok, State}.

%% Helper process to convert synchronous cluster status events into
%% asynchronous casts.
notif_init() ->
    register(tailf_viperl_server_notif, self()),
    {ok, Sock} = econfd_notif:connect({127,0,0,1}, ?NCS_PORT,
                                      ?CONFD_NOTIF_HA_INFO bor
                                          ?CONFD_NOTIF_UPGRADE_EVENT),
    proc_lib:init_ack(ok),
    notif_loop(Sock).

notif_loop(Sock) ->
    Msg = econfd_notif:recv(Sock),
    cast({event, Msg}),
    notif_loop(Sock).

%% Helper process to convert synchronous cdb update events into
%% asynchronous casts.
cdb_init() ->
    register(tailf_viperl_server_cdb, self()),
    {ok, S} = econfd_cdb:connect({127,0,0,1}, ?NCS_PORT),
    econfd_cdb:wait_start(S),
    {ok, Sub} = econfd_cdb:subscribe_session(S),
    {ok, _Point1} = econfd_cdb:subscribe(Sub, 100, ?ns, "/hcc"),
    ok = econfd_cdb:subscribe_done(Sub),
    proc_lib:init_ack(ok),
    cdb_loop(Sub).

cdb_loop(Sub) ->
    F = fun(_) -> cast({event, cdb_update}), ?CDB_DONE_SOCKET end,
    ok = econfd_cdb:wait(Sub, infinity, F),
    cdb_loop(Sub).

get_services(_, [_|Cmd], _, _)->
    econfd:log(?CONFD_LEVEL_TRACE, "Get service: ~p", [Cmd]),
    ServicePoints = ncs_servmgr:service_points(),
    Result = [
               [
                {service, start},
                {path, list_to_binary(cs:ikeypath2strpath(IKeypath, true))},
                {service, stop}
               ]
                       || {IKeypath, _ServicePoint, _ServiceInterface,
                           _HasPlan, _HasPlanHistory, _Nano}
                              <- ServicePoints
             ],
    {ok, lists:flatten(Result) }.

init_confd_callbacks() ->
    {ok, Daemon} = econfd:init_daemon(?SERVER, ?CONFD_TRACE, user, none,
                                      {127,0,0,1}, ?CONFD_PORT),
    ActCB2 = #confd_action_cb {
               actionpoint = 'check-sync-get-services',
               action = fun get_services/4
             },
    ok = econfd:register_action_cb(Daemon, ActCB2),
    ok = econfd:register_done(Daemon),
    econfd:log(?CONFD_LEVEL_INFO, "~p: Registered econfd callbacks",
               [?SERVER]),
    ok.
%%
handle_call(Req, _From, State) ->
    econfd:log(?CONFD_LEVEL_ERROR, "~p: Got unexpected call: ~p",
               [?SERVER, Req]),
    Reply = error,
    {reply, Reply, State}.


%%
handle_cast({event, What}, State) ->
    econfd:log(?CONFD_LEVEL_TRACE, "handle cast: ~p", [What]),
    %% React to any external event by polling the current state of the world.
    {noreply, State};

handle_cast({force_status, Status}, State) ->
    %% for debugging, force Status = master | slave | undefined
    put(force_status, Status),
    {noreply, State}.

handle_info({_Port, {data, _Data}}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    econfd:log(?CONFD_LEVEL_ERROR, "~p: Got unexpected info: ~p~n",
               [?SERVER, Info]),
    {noreply, State}.

%%
terminate(Reason, _State) ->
    econfd:log(?CONFD_LEVEL_TRACE, "~p: Server stopped - ~p",
               [?SERVER, Reason]),
    ok.

%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




