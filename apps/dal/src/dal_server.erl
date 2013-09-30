%%%---------------------------------------------------------------------------------------------
%%% File	: dal_server.erl
%%% Author	: Thomas Nyongesa
%%% Description:
%%%
%%% Created	: September 19, 2013 by Thomas Nyongesa
%%%---------------------------------------------------------------------------------------------
-module (dal_server).
-behaviour (gen_server).

%% API
-export ([start_link/0, select/1, insert/1, delete/1, update/1]).

%% gen_server callbacks
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%==============================================================================================
%% API
%%==============================================================================================
%%----------------------------------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%----------------------------------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

select(Qlc) ->
	gen_server:call(?MODULE, {select, Qlc}).

insert(Data) ->
	gen_server:call(?MODULE, {insert, Data}).

delete(Data) ->
	gen_server:call(?MODULE, {delete, Data}).

update({{Tab}, Key, Index, NewValue}) ->
	gen_server:call(?MODULE, {update, {{Tab}, Key, Index, NewValue}}).

%%==============================================================================================
%% gen_server callbacks
%%==============================================================================================
init([]) -> 
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	{ok, 0}.

%%----------------------------------------------------------------------------------------------
%% Function: handle_call(_Request, _From, State) -> {reply, Reply, State} | 
%%													{reply, Reply, State, Timeout} |
%%													{noreply, State},
%%													{noreply, State, Timeout},
%%													{stop, Reason, Reply, State}
%%													{stop, Reason, State}
%% Description: Handling call messages
%%----------------------------------------------------------------------------------------------
handle_call({select, Qlc}, _From, N) -> 
	Data = select1(Qlc),
	{reply, {ok, Data}, N};
handle_call({insert, Data}, _From, N) -> 
	Status = insert1(Data),
	{reply, Status, N};
handle_call({delete, Data}, _From, N) -> 
	Status = delete1(Data),
	{reply, Status, N};
handle_call({update, Data}, _From, N) -> 
 	Status = update1(Data),
	{reply, Status, N}.

%%----------------------------------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> 	{noreply, State} | 
%%											{noreply, State, Timeout} |
%%											{stop, Reason, State}
%% Description: Handling cast messages
%%----------------------------------------------------------------------------------------------
handle_cast(_Msg, State) -> 
	{noreply, State}.

%%----------------------------------------------------------------------------------------------
%% Function: handle_info(Info, State) -> 	{noreply, State} | 
%%											{noreply, State, Timeout} |
%%											{stop, Reason, State}
%% Description: Handling all non call/cast messages
%%----------------------------------------------------------------------------------------------
handle_info(_Info, State) -> 
	{noreply, State}.

%%----------------------------------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen-server when it is about to 
%% terminate.  It should be the opposite of Module:init/1 and do any necessary
%% cleaning up.  When it returns, the gen-server terminates with Reason.
%% The return value is ignored.
%%----------------------------------------------------------------------------------------------
terminate(_Reason, _State) -> 
	ok.

%%----------------------------------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> 	{ok, NewState}
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.

%%----------------------------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------------------------

%%----------------------------------------------------------------------------------------------
%% Function: select1(Qlc) -> 	{ok, NewState}
%% Description: Executes a select query with Q, a query list comprehension
%%----------------------------------------------------------------------------------------------
select1(Qlc) ->
	F = fun()-> qlc:e(Qlc) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

%%----------------------------------------------------------------------------------------------
%% Function: insert1(Q) -> 	{ok, NewState}
%% Description: Executes a select query with Q, a query list comprehension
%%----------------------------------------------------------------------------------------------
insert1(Data) ->
    F = fun() ->
		mnesia:write(Data)
	end,
    mnesia:transaction(F).

%%----------------------------------------------------------------------------------------------
%% Function: delete1(Q) -> 	{ok, NewState}
%% Description: Executes a select query with Q, a query list comprehension
%%----------------------------------------------------------------------------------------------
delete1(Data) ->
    F = fun() ->
		mnesia:delete(Data)
	end,
    mnesia:transaction(F).

%%----------------------------------------------------------------------------------------------
%% Function: update(Criteria) -> 	{ok, NewState}
%% Description: Executes an update query with Criteria, a list of fields to update
%%----------------------------------------------------------------------------------------------

update1(Data)->
	{{Tab}, Key, Index, NewValue} = Data,

	F = fun()-> mnesia:wread({Tab, Key}) end,

	{atomic, [P]} = mnesia:transaction(F),

 	NewRec = setelement(Index, P, NewValue),

	F1 = fun() -> mnesia:write(NewRec) end,

	mnesia:transaction(F1).	