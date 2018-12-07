-module(aestratum_app).

-behavior(application).

-export([start/2,
         stop/1
        ]).

-define(LISTENER, aestratum_listener).

start(_Type, _Args) ->
    %% TODO: read config with aeu_env:user_config
    Transport = ranch_tcp,
    %% TODO: ip - interface to listen on (all by default)
    TransportOpts = [{port, 9999}],
    Protocol = aestratum_handler,
    ProtocolOpts = [],
    %% The supervisor of the listener is ranch_sup.
    {ok, _} = ranch:start_listener(
                ?LISTENER,
                Transport, TransportOpts,
                Protocol, ProtocolOpts),
    aestratum_sup:start_link().

stop(_State) ->
	ok.

