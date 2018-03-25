%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2018 Pentland Edge Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations
%% under the License.
%%

-module(ais_json).

-export([tagged_ais_to_json/1]).

%% Function to convert a list of decoded AIS records to JSON. Consumes the 
%% tagged form e.g. {ok, #ais{}} and steps over any sentences that 
%% failed to decode.
tagged_ais_to_json(AisRecs) ->
    CnbList = aisle:extract_cnb_records(AisRecs),
    CnbList.
