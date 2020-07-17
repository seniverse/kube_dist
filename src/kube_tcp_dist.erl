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

-module(kube_tcp_dist).

-include_lib("kernel/include/net_address.hrl").

-export([listen/1, accept/1, accept_connection/5, setup/5, close/1, select/1]).

select(_) ->
    true.

listen(Name) ->
    {ok, Host} = inet:gethostname(),
    case kube_epmd:address(atom_to_list(Name), Host) of
        {ok, IP, Port, Creation} ->
            Address =
                #net_address {
                   address = IP,
                   host = Host,
                   protocol = tcp,
                   family = inet},

            Opts =
                case application:get_env(kernel, inet_dist_listen_options) of
                    {ok, ListenOpts} ->
                        ListenOpts;
                    _ ->
                        []
                end ++
                [{active, false},
                 {packet,2},
                 {reuseaddr, true},
                 {backlog,128}
                ],

            case gen_tcp:listen(Port, Opts) of
                {ok, Socket} ->
                    {ok, {Socket, Address, Creation}};
                Error ->
                    Error
            end;
        {error, not_ready} ->
            receive
            after 1000 ->
                    listen(Name)
            end
    end.

accept(Listen) ->
    inet_tcp_dist:accept(Listen).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    inet_tcp_dist:accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime).

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    inet_tcp_dist:setup(Node, Type, MyNode, LongOrShortNames, SetupTime).

close(Socket) ->
    inet_tcp_dist:close(Socket).
