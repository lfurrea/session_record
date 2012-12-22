
-module(session_record_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SERVER, ?MODULE). 
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RecordCore = {record_core, {record_core, start_link, []},
		  permanent, 2000, worker, [record_core]},
    EncodeSup = {encode_sup, {encode_sup, start_link, []}, 
	      permanent, 2000, supervisor, [encode_sup]},
    DBStoreSup = {db_store_sup, {db_store_sup, start_link, []}, 
	      permanent, 2000, supervisor, [db_store_sup]},
    Children = [RecordCore, EncodeSup, DBStoreSup],
    RestartStrategy = {one_for_one, 0 , 1},
    {ok, { RestartStrategy, Children }}.

