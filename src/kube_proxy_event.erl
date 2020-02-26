-module(kube_proxy_event).

-behaviour(gen_event).

-export([add_handler/2]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

add_handler(Pid, Service) ->
    gen_event:add_sup_handler(kube_endpoints, ?MODULE, [Pid, Service]).

init([Pid, Service]) ->
    {ok, {Pid, Service}}.

handle_event({Service, NodeName, Names}, {Pid, Service} = State) ->
    gen_server:cast(Pid, {endpoints, NodeName, Names}),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.
