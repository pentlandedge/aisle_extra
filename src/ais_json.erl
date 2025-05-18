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

-export([tagged_ais_to_json/1, tagged_ais_to_json2/1, mmsi_cnb_map_to_json/1]).

%% Function to convert a list of decoded AIS records to JSON. Consumes the 
%% tagged form e.g. {ok, #ais{}} and steps over any sentences that 
%% failed to decode.
tagged_ais_to_json(AisRecs) ->
    AisCnbList = aisle:extract_cnb_records(AisRecs),
    ais_cnb_to_geojson(AisCnbList).

tagged_ais_to_json2(AisRecs) ->
    GoodRecs = extract_valid_recs(AisRecs),
    GoodRecs.

extract_valid_recs(AisRecs) ->
    F = fun(Rec, Acc) ->
            case tagged_ais_valid(Rec) of
                true ->
                    {ok, Ais} = Rec,
                    [Ais|Acc];
                false ->
                    Acc
            end
        end,
    Rev = lists:foldl(F, [], AisRecs),
    lists:reverse(Rev).

tagged_ais_valid({ok, A}) ->
    aisle:is_ais_rec(A);
tagged_ais_valid(_)            -> false.

%% Convert a map containing MMSI => CNB pairs to GeoJSON.
mmsi_cnb_map_to_json(CnbMap) when is_map(CnbMap) ->
    CnbList = maps:values(CnbMap),
    cnb_to_geojson(CnbList).

%% Function to convert a list of AIS records containing CNB records to GeoJSON. 
ais_cnb_to_geojson(AisCnbList) when is_list(AisCnbList)  ->
    PrepList = lists:map(fun ais_cnb_prep/1, AisCnbList),
    jsx:encode([{<<"data">>, PrepList}]).

%% Function to convert a list of AIS records containing CNB records to GeoJSON. 
cnb_to_geojson(CnbList) when is_list(CnbList)  ->
    FeatureList = lists:map(fun cnb_prep/1, CnbList),
    jsx:encode([{<<"type">>,<<"FeatureCollection">>},
                {<<"features">>, FeatureList}]).

%% Collect the relevant data into a structure suitable for encoding using
%% the jsx library.
ais_cnb_prep(AISrec) -> 
    CNB = aisle:get_data(AISrec),
    cnb_prep(CNB).

%% Convert the CNB record to a form suitable for JSON conversion.
cnb_prep(CNB) ->
    Timestamp = aisle:get_timestamp(CNB),
    Lat = aisle:get_latitude(CNB),
    Lon = aisle:get_longitude(CNB),
    MMSI = aisle:get_mmsi(CNB),
    gen_cnb_geojson(Timestamp, Lat, Lon, MMSI).

%% Generate the GeoJSON for a CNB record.
gen_cnb_geojson(TimeUtc, Lat, Lon, MMSI) ->
    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"timestamp">>, TimeUtc},
                         {<<"type">>, <<"CNB">>},
                         {<<"mmsi">>, MMSI}]},
     {<<"geometry">>, [{<<"type">>, <<"Point">>},
                       {<<"coordinates">>, [Lon, Lat]}]}].
