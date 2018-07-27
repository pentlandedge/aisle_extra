-module(ais_time).

-export([extract_datetime/1, extract_time_info/1]).

%% Function to extract a datetime from an AIS record if one is available.
extract_datetime({ok, A} = AisRec) ->
    MessageType = aisle:get_payload_type(AisRec),
    Data = aisle:get_data(A), 
    case MessageType of
        cnb     -> 
            {seconds, aisle:get_timestamp(Data)};  
        base_sr ->
            io:format("Got a BSR~n"),
            DateTime = aisle:get_bsr_datetime(Data),
            io:format("DateTime ~p~n", [DateTime]),
            DateTime; 
        _       -> 
            undefined
    end.

extract_time_info(AisRecs) when is_list(AisRecs) ->
    F = fun(AIS, Acc) ->
            case extract_datetime(AIS) of
                {{_,_,_},{_,_,_}} = DateTime -> 
                    [DateTime|Acc];
                _ -> 
                    Acc
            end
        end,
    lists:foldl(F, [], AisRecs).
