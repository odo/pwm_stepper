%%
%% Copyright (c) 2025 <odo@mac.com>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(pwm_stepper).

-export([start/0]).

start() ->
    stepper:start(high_torque, 1.8),
    stepper:forward(360, 5, locked),
    stepper:backward(360, 5, locked),
    stepper:forward(360, 5, locked),
    stepper:backward(360, 5, locked),
    stepper:forward(360, 5, locked),
    stepper:backward(360, 5, locked),
    receive
        never -> ok
    end.

