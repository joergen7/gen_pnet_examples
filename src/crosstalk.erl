-module( crosstalk ).
-behaviour( gen_pnet ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, trigger/2] ).

-export( [place_lst/0, trsn_lst/0, init_marking/1, preset/1, is_enabled/2,
        fire/2] ).

-export( [start/0, start_link/0] ).

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).

%%====================================================================
%% API functions
%%====================================================================

start() ->

  F = fun
        F( P, 0 ) -> gen_pnet:stop( P );
        F( P, N ) ->
          timer:sleep( 2000 ),
          #stats{ current = Current } = gen_pnet:get_stats( P ),
          

          io:format( "~p~n", [Current] ),
          F( P, N-1 )
      end,

  {ok, Pid} = gen_pnet:start_link( ?MODULE, [] ),
  F( Pid, 4 ).


start_link() ->
  gen_pnet:start_link( ?MODULE, [] ).

%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

handle_call( _, _, _ ) -> {reply, {error, bad_msg}}.

handle_cast( _Request, _NetState ) -> noreply.

handle_info( _Request, _NetState ) -> noreply.

terminate( _Reason, _NetState ) -> ok.

trigger( _, _ ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

place_lst() ->
  [idle1, waiting1, replied1, finished1, confirmed1, sent1,
   idle2, waiting2, replied2, finished2, confirmed2, sent2].

trsn_lst() ->
  [send1, finish1, reply1, return1, crosstalk1,
   send2, finish2, reply2, return2, crosstalk2].

init_marking( idle1 ) -> [tk];
init_marking( idle2 ) -> [tk];
init_marking( _ )     -> [].

preset( send1 )      -> [idle1];
preset( send2 )      -> [idle2];
preset( finish1 )    -> [waiting1, confirmed2];
preset( finish2 )    -> [waiting2, confirmed1];
preset( reply1 )     -> [idle1, sent2];
preset( reply2 )     -> [idle2, sent1];
preset( return1 )    -> [replied1, finished2];
preset( return2 )    -> [replied2, finished1];
preset( crosstalk1 ) -> [waiting1, sent2];
preset( crosstalk2 ) -> [waiting2, sent1].

is_enabled( _, _ ) -> true.

fire( send1, _ )      -> {produce, #{ waiting1 => [tk], sent1 => [tk] }};
fire( send2, _ )      -> {produce, #{ waiting2 => [tk], sent2 => [tk] }};
fire( finish1, _ )    -> {produce, #{ idle1 => [tk], finished1 => [tk] }};
fire( finish2, _ )    -> {produce, #{ idle2 => [tk], finished2 => [tk] }};
fire( reply1, _ )     -> {produce, #{ replied1 => [tk], confirmed1 => [tk] }};
fire( reply2, _ )     -> {produce, #{ replied2 => [tk], confirmed2 => [tk] }};
fire( return1, _ )    -> {produce, #{ idle1 => [tk] }};
fire( return2, _ )    -> {produce, #{ idle2 => [tk] }};
fire( crosstalk1, _ ) -> {produce, #{ finished1 => [tk], replied1 => [tk] }};
fire( crosstalk2, _ ) -> {produce, #{ finished2 => [tk], replied2 => [tk] }}.



