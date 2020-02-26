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

-export([send/2, whereis_name/1]).

send(Name, Msg) ->
    case whereis_name(Name) of
        undefined ->
            exit({badarg, {Name, Msg}});
        Proc ->
            Proc ! Msg,
            Proc
    end.

whereis_name(Name) when is_atom(Name) ->
    case ets:lookup(?MODULE, atom_to_binary(Name, unicode)) of
        [] ->
            undefined;
        [{_, Node, Names}] ->
            {Name, node_name(Node, lists:nth(rand:uniform(length(Names)), Names))}
    end.

node_name(Name, Host) when is_atom(Name) ->
    node_name(atom_to_binary(Name, unicode), Host);
node_name(Name, Host) ->
    binary_to_atom(<<Name/binary, "@", Host/binary>>, unicode).
