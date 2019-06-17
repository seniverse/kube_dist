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

-module(kube_endpoints).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([send/2, whereis_name/1]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

send(Name, Msg) ->
    case whereis_name(Name) of
        undefined ->
            exit({badarg, {Name, Msg}});
        Proc ->
            Proc ! Msg,
            Proc
    end.

whereis_name(Name) when is_atom(Name) ->
    [Node, _] = binary:split(atom_to_binary(node(), unicode), <<"@">>),
    whereis_name({Name, Node});
whereis_name({Name, Node}) ->
    case ets:lookup(?SERVER, atom_to_binary(Name, unicode)) of
        [] ->
            undefined;
        [{_, Names}] ->
            {Name, node_name(Node, lists:nth(rand:uniform(length(Names)), Names))}
    end.

node_name(Name, Host) when is_atom(Name) ->
    node_name(atom_to_binary(Name, unicode), Host);
node_name(Name, Host) ->
    binary_to_atom(<<Name/binary, "@", Host/binary>>, unicode).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?SERVER, [named_table, set, {read_concurrency,true}]),
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
          [{body_format, binary}, {sync, false}, {stream, self}]),
    #{namespace => Namespace, token => Token, ref => Ref}.

handle_changes([Buf]) ->
    Buf;
handle_changes([Data|Rest]) ->
    case jsone:decode(Data) of
        #{<<"type">> := <<"DELETED">>,
          <<"object">> := #{<<"metadata">> := #{<<"name">> := Name}}} ->
            ets:delete(?SERVER, Name);
        #{<<"object">> :=
              #{<<"metadata">> := #{<<"name">> := Name},
                <<"subsets">> := [#{<<"addresses">> := Addresses}]
               }} ->
            ets:insert(?SERVER, {Name, [N || #{<<"targetRef">> := #{<<"name">> := N} } <- Addresses]});
        Data ->
            ?LOG_DEBUG("Unknown endpoint ~p~n", [Data])
    end,
    handle_changes(Rest).
