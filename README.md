# aisle_extra
Extra utilities for manipulating and converting AIS data.

## Dependencies
The library is intended to be used in conjunction with the core AIS decoder library aisle. It also uses the jsx library for JSON conversion.

## Pre-requisites

It is necessary to have a recent version of Erlang, the rebar3 build tool and (optionally) a make utiility installed in order to build the software.

## Building and runnning the unit tests
The software can be built (on a Linux platform) as follows:
```
# make 
```
This will fetch the dependencies, run the unit tests, generate module documentation and run the dialyzer static analysis tool.

## Decoding AIS sentences and converting to JSON.
This example shows an interactive session in the Erlang shell, decoding some recorded AIS data supplied by the aisle library.

From the project root directory, start the Erlang shell:
```
rebar3 shell
```

Parse the supplied log file containing AIS sentences (this assumes that the AIS library is located in deps/aisle):
```
1> A = aisle:parse_file("deps/aisle/logs/ais_squeensferry_advansea_rx_100_20161120.log").
```

This will return a list of tagged, decoded AIS records of the form 
```
[{ok, #ais{}}|....].
```
where the first element in the tuple is the decode status of the sentence and the second element is the decoded sentence record. The aisle library provides an API for accessing fields of the decoded structure.

This tagged list can be passed directly to the JSON conversion utility:

```
2> GeoJSON = ais_json:tagged_ais_to_json(A).
```
This will return a binary blob containing fields from the AIS CNB (common navigation block) converted into a list of  GeoJSON features.  The aim is to extend both the aisle decoder and the GeoJSON conversion to provide more comprehensive information.

