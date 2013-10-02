erldal
======

OTP Data Access Server For Mnesia

USAGE
======
This example assume an existing mnesia table with the following structure {shop, {item, quantity, cost}}.  
The dal_server is also assumed to be running.


select/1, 
===============================================================================================
Qlc=qlc:q([{X}||X <- mnesia:table(shop)])
dal_server:select(Qlc).

REMOTE EXAMPLE(dal_server running on a different node tomilay@dev).
-------------------------------------------------------------------
N = 'tomilay@dev'.
rpc:call(N, dal_server, select, [Qlc]).


insert/1, 
===============================================================================================
dal_server:insert({shop, avocados, 20, 10.0}).

REMOTE EXAMPLE(dal_server running on a different node tomilay@dev).
-------------------------------------------------------------------
N = 'tomilay@dev'.
rpc:call(N, dal_server, insert, [{shop, avocados, 20, 10.0}]).


delete/1,
===============================================================================================
dal_server:delete({shop, avocado}).

REMOTE EXAMPLE(dal_server running on a different node tomilay@dev).
-------------------------------------------------------------------
N = 'tomilay@dev'.
rpc:call(N, dal_server, delete, [{shop, avocados}]).


update/1,
===============================================================================================
%% Changing the key will create a new record.  Not recommended.

rr(shop, {item, quantity, cost}).

dal_server:update({{shop}, avocados, #shop.item, avocado}). 
dal_server:update({{shop}, avocado, #shop.quantity, 20}).
dal_server:update({{shop}, avocado, #shop.cost, 11.50}).

REMOTE EXAMPLE(dal_server running on a different node tomilay@dev).
-------------------------------------------------------------------
N = 'tomilay@dev'.

rpc:call(N, dal_server, update, [{{shop}, avocados, #shop.item, avocado}]).
rpc:call(N, dal_server, update, [{{shop}, avocado, #shop.quantity, 20}]).
rpc:call(N, dal_server, update, [{{shop}, avocado, #shop.cost, 11.50}]).


insert_list/1, 
===============================================================================================
dal_server:insert_list([{shop, avocados, 20, 10.0}, {shop, plums, 20, 10.0}, {shop, potatoes, 20, 10.0}]).

REMOTE EXAMPLE(dal_server running on a different node tomilay@dev).
-------------------------------------------------------------------
N = 'tomilay@dev'.

rpc:call(N, dal_server, insert_list, [[{shop, avocados, 20, 10.0}, {shop, plums, 20, 10.0}, {shop, potatoes, 20, 10.0}]]).


delete_list/1,
=============================================================================================== 
dal_server:insert_list([{shop, avocados, 20, 10.0}, {shop, plums, 20, 10.0}, {shop, potatoes, 20, 10.0}]).

REMOTE EXAMPLE(dal_server running on a different node tomilay@dev).
-------------------------------------------------------------------
N = 'tomilay@dev'.

rpc:call(N, dal_server, insert_list, [[{shop, avocados, 20, 10.0}, {shop, plums, 20, 10.0}, {shop, potatoes, 20, 10.0}]]).


update_list/1
===============================================================================================
%% Changing the key will create a new record.  Not recommended.

dal_server:update_list([{{shop}, plums, #shop.quantity, 10}, {{shop}, avocado, #shop.quantity, 20}, {{shop}, avocado, #shop.cost, 11.50}]).

REMOTE EXAMPLE(dal_server running on a different node tomilay@dev).
-------------------------------------------------------------------
N = 'tomilay@dev'.

rpc:call(N, dal_server, update_list, [[{{shop}, plums, #shop.quantity, 10}, {{shop}, avocado, #shop.quantity, 20}, {{shop}, avocado, #shop.cost, 11.50}]]).
