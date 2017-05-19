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

-module( dinner ).

-behaviour( gen_pnet ).

-export( [start/0, start_link/0] ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/3] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3,
          fire/3] ).

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).

%%====================================================================
%% API functions
%%====================================================================

start() ->

  F = fun
        F( P, 0 ) -> gen_pnet:stop( P );
        F( P, N ) ->
          timer:sleep( 2000 ),
          #stats{ current = Current } = gen_pnet:stats( P ),
          

          io:format( "~p~n", [Current] ),
          F( P, N-1 )
      end,

  {ok, Pid} = start_link(),
  F( Pid, 4 ).

start_link() ->
  gen_pnet:start_link( ?MODULE, [], [] ).


%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

handle_call( _Request, _From, _NetState ) -> {reply, ok}.

handle_cast( _Request, _NetState ) -> noreply.

handle_info( _Request, _NetState ) -> noreply.

init( _Args ) -> {ok, gen_pnet:new( ?MODULE, [] )}.

terminate( _Reason, _NetState ) -> ok.

trigger( _Place, _Token, _NetState ) -> pass.

%%====================================================================
%% Petri net callback functions
%%====================================================================

place_lst() ->
  [eat1, eat2, eat3, eat4, eat5,
   fork1, fork2, fork3, fork4, fork5].

trsn_lst() ->
   [take1, release1,
    take2, release2,
    take3, release3,
    take4, release4,
    take5, release5].

init_marking( fork1, _ ) -> [fork];
init_marking( fork2, _ ) -> [fork];
init_marking( fork3, _ ) -> [fork];
init_marking( fork4, _ ) -> [fork];
init_marking( fork5, _ ) -> [fork];
init_marking( _, _ )     -> [].

preset( take1 )    -> [fork1, fork2];
preset( take2 )    -> [fork2, fork3];
preset( take3 )    -> [fork3, fork4];
preset( take4 )    -> [fork4, fork5];
preset( take5 )    -> [fork5, fork1];
preset( release1 ) -> [eat1];
preset( release2 ) -> [eat2];
preset( release3 ) -> [eat3];
preset( release4 ) -> [eat4];
preset( release5 ) -> [eat5].

is_enabled( take1, #{ fork1 := [fork], fork2 := [fork] }, _ ) -> true;
is_enabled( take2, #{ fork2 := [fork], fork3 := [fork] }, _ ) -> true;
is_enabled( take3, #{ fork3 := [fork], fork4 := [fork] }, _ ) -> true;
is_enabled( take4, #{ fork4 := [fork], fork5 := [fork] }, _ ) -> true;
is_enabled( take5, #{ fork5 := [fork], fork1 := [fork] }, _ ) -> true;
is_enabled( release1, #{ eat1 := [eating] }, _ )              -> true;
is_enabled( release2, #{ eat2 := [eating] }, _ )              -> true;
is_enabled( release3, #{ eat3 := [eating] }, _ )              -> true;
is_enabled( release4, #{ eat4 := [eating] }, _ )              -> true;
is_enabled( release5, #{ eat5 := [eating] }, _ )              -> true;
is_enabled( _, _, _ )                                         -> false.

fire( take1, _, _ )    -> {produce, #{ eat1 => [eating] }};
fire( take2, _, _ )    -> {produce, #{ eat2 => [eating] }};
fire( take3, _, _ )    -> {produce, #{ eat3 => [eating] }};
fire( take4, _, _ )    -> {produce, #{ eat4 => [eating] }};
fire( take5, _, _ )    -> {produce, #{ eat5 => [eating] }};
fire( release1, _, _ ) -> {produce, #{ fork1 => [fork], fork2 => [fork] }};
fire( release2, _, _ ) -> {produce, #{ fork2 => [fork], fork3 => [fork] }};
fire( release3, _, _ ) -> {produce, #{ fork3 => [fork], fork4 => [fork] }};
fire( release4, _, _ ) -> {produce, #{ fork4 => [fork], fork5 => [fork] }};
fire( release5, _, _ ) -> {produce, #{ fork5 => [fork], fork1 => [fork] }}.




