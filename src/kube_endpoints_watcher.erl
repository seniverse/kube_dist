%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(kube_endpoints_watcher).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(TAB, kube_endpoints).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TAB, [named_table, set, {read_concurrency,true}]),
    {ok, Namespace} = file:read_file("/run/secrets/kubernetes.io/serviceaccount/namespace"),
    {ok, Token} = file:read_file("/run/secrets/kubernetes.io/serviceaccount/token"),
    State = watch(#{namespace => Namespace, token => binary_to_list(Token)}),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({http, {Ref, stream_start, _}}, State = #{ref := Ref}) ->
    {noreply, State#{buf => <<>>}};
handle_info({http, {Ref, stream, Data}}, State = #{ref := Ref, buf := Buf}) ->
    {noreply, State#{buf := handle_changes(binary:split(<<Buf/binary, Data/binary>>, <<"\n">>, [global]))}};
handle_info({http, {Ref, stream_end, _}}, State = #{ref := Ref}) ->
    {noreply, watch(State)};
handle_info({http, {error, _}}, State) ->
    {noreply, watch(State)};
handle_info(Info, State) ->
    ?LOG_DEBUG("Unknown message ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

watch(#{namespace := Namespace, token := Token}) ->
    {ok, Ref} =
        httpc:request(
          get,
          {["https://kubernetes.default/api/v1/namespaces/", Namespace, "/endpoints?watch=true"],
           [{"Authorization", "Bearer " ++ Token}]},
          [{ssl, [{cacertfile, "/run/secrets/kubernetes.io/serviceaccount/ca.crt"}]}],
          [{body_format, binary}, {sync, false}, {stream, self}, {socket_opts,[{keepalive, true}]}]),
    #{namespace => Namespace, token => Token, ref => Ref}.

handle_changes([Buf]) ->
    Buf;
handle_changes([Buf|Rest]) ->
    case jsone:decode(Buf) of
        #{<<"type">> := <<"DELETED">>,
          <<"object">> := #{<<"metadata">> := #{<<"name">> := Name}}} ->
            ets:delete(?TAB, Name),
            gen_event:notify(?TAB, {Name, []});
        #{<<"object">> := #{<<"metadata">> := #{<<"name">> := Name}} = Object} ->
            Subsets = maps:get(<<"subsets">>, Object, []),
            Names =
                [ N
                  || #{<<"addresses">> := Addresses} <- Subsets,
                     #{<<"targetRef">> := #{<<"name">> := N} } <- Addresses ],
            ets:insert(?TAB, {Name, Names}),
            gen_event:notify(?TAB, {Name, Names});
        Data ->
            ?LOG_DEBUG("Unknown endpoint ~p~n", [Data])
    end,
    handle_changes(Rest).
