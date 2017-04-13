-module( cvm ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, trigger/2] ).

-export( [place_lst/0, trsn_lst/0, init_marking/0, preset/1, is_enabled/2,
	      fire/2] ).

-export( [start_link/0, insert_coin/1] ).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_pnet:start_link( ?MODULE, [] ).

insert_coin( Pid ) ->
  gen_pnet:cast( Pid, insert_coin ).

%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

handle_call( _Request, _From, _NetState ) -> {reply, ok}.


handle_cast( insert_coin, _ ) -> {noreply, #{}, #{ coin_slot => [coin] }};
handle_cast( _, _ )           -> noreply.

handle_info( _Request, _NetState ) -> noreply.

terminate( _Reason, _NetState ) -> ok.

trigger( _, _ ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

place_lst() ->
  [coin_slot, cash_box, signal, storage, compartment].

trsn_lst() ->
   [a, b].

init_marking() ->
  #{ storage => [cookie_box, cookie_box, cookie_box] }.

preset( a ) -> [coin_slot];
preset( b ) -> [signal, storage].

is_enabled( a, #{ coin_slot := [coin] } )                      -> true;
is_enabled( b, #{ signal := [sig], storage := [cookie_box] } ) -> true;
is_enabled( _, _ )                                             -> false.

fire( a, _ ) -> {produce, #{ cash_box => [coin], signal => [sig] }};
fire( b, _ ) -> {produce, #{ compartment => [cookie_box] }}.




