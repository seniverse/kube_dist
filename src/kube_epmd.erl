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

-module(kube_epmd).

-behaviour(gen_server).

-export([start_link/0, names/1, address/2, address_please/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([start_resolver/4]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

names(Host) ->
    gen_server:call(?SERVER, [Host]).

address(Name, Host) ->
    gen_server:call(?SERVER, [Host, Name]).

address_please(Name, Host, inet) ->
    case gen_server:call(?SERVER, [Host, Name]) of
        {ok, Address, Port, _} ->
            {ok, Address, Port, 5};
        Error ->
            Error
    end.

init([]) ->
    {ok, []}.

handle_call(Req, From, State) ->
    supervisor:start_child(kube_dist_resolver_sup, [From, Req]),
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.


start_resolver(Namespace, Token, From, Req) ->
    {ok, spawn_link(fun () -> gen_server:reply(From, resolve(Namespace, Token, Req)) end)}.

resolve(Namespace, Token, [Pod|Rest]) ->
    case httpc:request(
           get,
           {["https://kubernetes.default/api/v1/namespaces/", Namespace, "/pods/", Pod, "/status"],
            [{"Authorization", "Bearer " ++ binary_to_list(Token)}]},
           [{ssl, [{cacertfile, "/run/secrets/kubernetes.io/serviceaccount/ca.crt"}]}],
           [{body_format, binary}])
    of
        {ok, {{_, 200, _}, _, Body}} ->
            #{<<"status">> :=
                  #{<<"podIP">> := PodIP,
                    <<"containerStatuses">> := Statuses},
              <<"spec">> :=
                  #{<<"containers">> := Containers}
             } = jsone:decode(Body),
            Names =
                [ {binary_to_list(ContainerName), P}
                  || #{<<"ports">> := Ports, <<"name">> := ContainerName} <- Containers,
                     #{<<"containerPort">> := P, <<"name">> := PortName, <<"protocol">> := <<"TCP">>} <- Ports,
                     PortName =:= <<ContainerName/binary, "-dist">>],
            case Rest of
                [] ->
                    {ok, Names};
                [Name] ->
                    case [P || {N, P} <- Names, N =:= Name] of
                        [] ->
                            {error, noport};
                        [Port] ->
                            {ok, Address} = inet:parse_strict_address(binary_to_list(PodIP)),
                            ContainerName = list_to_binary(Name),
                            [RestartCount] =
                                [ C
                                  || #{<<"name">> := N, <<"restartCount">> := C} <- Statuses,
                                     N =:= ContainerName ],
                            Creation = ((RestartCount + 1) rem 3) + 1,
                            {ok, Address, Port, Creation}
                    end
            end;
        _ ->
            {error, nxdomain}
    end.
