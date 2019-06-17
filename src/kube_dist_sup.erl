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

-module(kube_dist_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Name = list_to_atom(os:getenv("CONTAINER_NAME")),
    {ok,
     {#{strategy => one_for_one,
        intensity => 1,
        period => 5},
      [#{id => resolver_sup,
         start => {kube_dist_resolver_sup, start_link, []},
         restart => permanent,
         shutdown => infinity,
         type => supervisor,
         modules => [kube_dist_resolver_sup]
        },
       #{id => net_sup,
         start => {erl_distribution, start_link, [[Name, shortnames], true]},
         restart => permanent,
         shutdown => infinity,
         type => supervisor,
         modules => [erl_distribution]},
       #{id => endpoints,
         start => {kube_endpoints, start_link, []},
         restart => permanent,
         shutdown => infinity,
         type => worker,
         modules => [kube_endpoints]
        }]
     }
    }.
