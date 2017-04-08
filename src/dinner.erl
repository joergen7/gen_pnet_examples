-module( dinner ).

-export( [demo/0] ).
-export( [handle_call/3, handle_cast/2, trigger_map/0] ).
-export( [place_lst/0, trsn_lst/0, preset/1, is_enabled/2, fire/2] ).

%%====================================================================
%% API functions
%%====================================================================

demo() ->

  F = fun
        F( P ) ->
          timer:sleep( 10000 ),
          Stats = gen_pnet:stats( P ),
          io:format( "~p~n", [Stats] ),
          F( P )
      end,

  {ok, Pid} = gen_pnet:start_link( ?MODULE ),
  F( Pid ).

%%====================================================================
%% Interface callback functions
%%====================================================================


handle_call( _Request, _From, _NetState ) -> {reply, ok}.

handle_cast( _Request, _NetState ) -> noreply.

trigger_map() -> #{}.

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

init_marking() ->
  #{
    fork1 => [fork],
    fork2 => [fork],
    fork3 => [fork],
    fork4 => [fork],
    fork5 => [fork]
   }.

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

is_enabled( take1, #{ fork1 := [fork], fork2 := [fork] } ) -> true;
is_enabled( take2, #{ fork2 := [fork], fork3 := [fork] } ) -> true;
is_enabled( take3, #{ fork3 := [fork], fork4 := [fork] } ) -> true;
is_enabled( take4, #{ fork4 := [fork], fork5 := [fork] } ) -> true;
is_enabled( take5, #{ fork5 := [fork], fork1 := [fork] } ) -> true;
is_enabled( release1, #{ eat1 := [eating] } )              -> true;
is_enabled( release2, #{ eat2 := [eating] } )              -> true;
is_enabled( release3, #{ eat3 := [eating] } )              -> true;
is_enabled( release4, #{ eat4 := [eating] } )              -> true;
is_enabled( release5, #{ eat5 := [eating] } )              -> true;
is_enabled( _, _ )                                         -> false.

fire( take1, _ )    -> {produce, #{ eat1 => [eating] }};
fire( take2, _ )    -> {produce, #{ eat2 => [eating] }};
fire( take3, _ )    -> {produce, #{ eat3 => [eating] }};
fire( take4, _ )    -> {produce, #{ eat4 => [eating] }};
fire( take5, _ )    -> {produce, #{ eat5 => [eating] }};
fire( release1, _ ) -> {produce, #{ fork1 => [fork], fork2 => [fork] }};
fire( release2, _ ) -> {produce, #{ fork2 => [fork], fork3 => [fork] }};
fire( release3, _ ) -> {produce, #{ fork3 => [fork], fork4 => [fork] }};
fire( release4, _ ) -> {produce, #{ fork4 => [fork], fork5 => [fork] }};
fire( release5, _ ) -> {produce, #{ fork5 => [fork], fork1 => [fork] }}.




