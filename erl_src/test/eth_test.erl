-module(eth_test).

-include_lib("eunit/include/eunit.hrl").

%% convert Erlang terms to an intermediate Erlang structure which can be encoded
%% as JSON by https://github.com/talentdeficit/jsx
convert_atom_test() ->
    <<"key">> = eth:convert(key),
    <<"__atom___binary_key">> = eth:convert('__binary_key'),
    <<"__atom___string_key">> = eth:convert('__string_key').

convert_binary_test() ->
    <<"__binary_key">> = eth:convert(<<"key">>).

convert_string_test() ->
    <<"__string_key">> = eth:convert("key").

convert_list_test() ->
    ?assertEqual([1, 2, 3], eth:convert([1, 2, 3])),
    ?assertEqual([<<"__atom___list">>, 1, 2, 3], eth:convert(['__list', 1, 2, 3])),
    ?assertEqual([<<"__atom___tuple">>, 1, 2, 3], eth:convert(['__tuple', 1, 2, 3])).

convert_tuple_test() ->
    ?assertEqual([<<"__tuple">>, 1, 2, 3], eth:convert({1, 2, 3})),
    ?assertEqual([<<"__tuple">>, <<"__atom___list">>, 1, 2, 3], eth:convert({'__list', 1, 2, 3})),
    ?assertEqual([<<"__tuple">>, <<"__atom___tuple">>, 1, 2, 3], eth:convert({'__tuple', 1, 2, 3})).

convert_proplist_test() ->
    [{<<"storage_backend">>, <<"bitcask">>}] = eth:convert([{storage_backend, bitcask}]).

convert_boolean_test() ->
    true = eth:convert(true),
    false = eth:convert(false).

convert_null_test() ->
    null = eth:convert(null).

comparison_test() ->
    {ok, Binary} = file:read_file("../../test/examples/multi_backend.json"),
    {ok, [Terms]} = file:consult("../../test/examples/multi_backend.config"),
    Json = jsx:decode(Binary),
    Config = eth:convert(Terms),
    ?assertEqual(Json, Config).
