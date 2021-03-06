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
-export ([start_link/0, select/1, insert/1, delete/1, update/1, insert_list/1, delete_list/1, update_list/1]).

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

insert_list(Data) ->
	gen_server:call(?MODULE, {insert_list, Data}).

delete_list(Data) ->
	gen_server:call(?MODULE, {delete_list, Data}).

update_list(Data) ->
	gen_server:call(?MODULE, {update_list, Data}).

%%==============================================================================================
%% gen_server callbacks
%%==============================================================================================
init([]) -> 
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	{ok, 0}.

%%----------------------------------------------------------------------------------------------
%% Function: handle_call(_Request, _From, State)
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
	{reply, Status, N};
handle_call({insert_list, Data}, _From, N) ->
	Status = insert_list1(Data),
	{reply, Status, N};
handle_call({delete_list, Data}, _From, N) ->
	Status = delete_list1(Data),
	{reply, Status, N};
handle_call({update_list, Data}, _From, N) ->
	Status = update_list1(Data),
	{reply, Status, N}.

%%----------------------------------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Description: Handling cast messages
%%----------------------------------------------------------------------------------------------
handle_cast(_Msg, State) -> 
	{noreply, State}.

%%----------------------------------------------------------------------------------------------
%% Function: handle_info(Info, State) 
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
%% Function: select1(Qlc) -> 	{ok, Data}
%% Description: Executes a select query with Qlc, a query list comprehension
%%----------------------------------------------------------------------------------------------
select1(Qlc) ->
	F = fun()-> qlc:e(Qlc) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

%%----------------------------------------------------------------------------------------------
%% Function: insert1(Q) -> 	{atomic, ok}
%% Description: Executes an insert query with Data.  Data is an erlang record.
%%----------------------------------------------------------------------------------------------
insert1(Data) ->
    F = fun() ->
		mnesia:write(Data)
	end,
    mnesia:transaction(F).

%%----------------------------------------------------------------------------------------------
%% Function: delete1(Data) -> 	{atomic, ok}
%% Description: Executes a delete query with Data.  Data is a {Table, Key} erlang term
%%----------------------------------------------------------------------------------------------
delete1(Data) ->
    F = fun() ->
		mnesia:delete(Data)
	end,
    mnesia:transaction(F).

%%----------------------------------------------------------------------------------------------
%% Function: update(Criteria) -> 	{atomic, ok}
%% Description: Executes an update query with Criteria, a list of fields to update
%%----------------------------------------------------------------------------------------------
update1(Data)->
	{{Tab}, Key, Index, NewValue} = Data,

	F = fun()-> mnesia:wread({Tab, Key}) end,

	{atomic, [P]} = mnesia:transaction(F),

 	NewRec = setelement(Index, P, NewValue),

	F1 = fun() -> mnesia:write(NewRec) end,

	mnesia:transaction(F1).	

%%----------------------------------------------------------------------------------------------
%% Function: insert_list1(Data) -> 	{atomic, ok}
%% Description: Executes an insert query with a list of records
%%----------------------------------------------------------------------------------------------
insert_list1(Data) ->
    F = fun() ->
		lists:foreach(fun(Item) -> mnesia:write(Item) end, Data)
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------------------------------------
%% Function: delete_list1(Data) -> 	{atomic, ok}
%% Description: Executes a delete query on a list Data.  Data is a list of {Table, Key} erlang term
%%--------------------------------------------------------------------------------------------------
delete_list1(Data) ->
    F = fun() ->
		lists:foreach(fun(Item) -> mnesia:delete(Item) end, Data)
	end,
    mnesia:transaction(F).

%%----------------------------------------------------------------------------------------------
%% Function: update_list1(Data) -> 	{atomic, ok}
%% Description: Executes a update query n a list Data.  Data is a list of fields to update 
%%----------------------------------------------------------------------------------------------
update_list1(Data) ->
    F = fun() ->
		lists:foreach(fun update1/1, Data)
	end,
    mnesia:transaction(F).
