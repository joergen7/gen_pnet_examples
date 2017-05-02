%% -*- erlang -*-
%%
%% An example collection for the gen_pnet behavior.
%%
%% Copyright 2017 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.0
%% @copyright 2017 Jörgen Brandt

-module( cvm ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/3] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/2,
	        fire/3] ).

-export( [start_link/0, insert_coin/1, remove_cookie_box/1] ).

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_pnet:start_link( ?MODULE, [], [] ).

insert_coin( Pid ) ->
  gen_pnet:call( Pid, insert_coin ).

remove_cookie_box( Pid ) ->
  gen_pnet:call( Pid, remove_cookie_box ).

%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

handle_call( insert_coin, _From, _NetState ) ->
  {reply, ok, #{}, #{ coin_slot => [coin] }};

handle_call( remove_cookie_box, _From, NetState ) ->

  case gen_pnet:ls_place( compartment, NetState ) of
    []    -> {reply, {error, empty_compartment}};
    [_|_] -> {reply, ok, #{ compartment => [cookie_box] }, #{}}
  end;

handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.

handle_cast( _Request, _NetState ) -> noreply.

handle_info( _Request, _NetState ) -> noreply.

init( _Args ) -> {ok, gen_pnet:new( ?MODULE, [] )}.

terminate( _Reason, _NetState ) -> ok.

trigger( _Place, _Token, _UsrInfo ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

place_lst() ->
  [coin_slot, cash_box, signal, storage, compartment].

trsn_lst() ->
   [a, b].

init_marking( storage, _UsrInfo ) -> [cookie_box, cookie_box, cookie_box];
init_marking( _Place, _UsrInfo )  -> [].

preset( a ) -> [coin_slot];
preset( b ) -> [signal, storage].

is_enabled( a, #{ coin_slot := [coin] } )                      -> true;
is_enabled( b, #{ signal := [sig], storage := [cookie_box] } ) -> true;
is_enabled( _Trsn, _Mode )                                     -> false.

fire( a, _Mode, _UsrInfo ) ->
  {produce, #{ cash_box => [coin], signal => [sig] }};

fire( b, _Mode, _UsrInfo ) ->
  {produce, #{ compartment => [cookie_box] }}.




