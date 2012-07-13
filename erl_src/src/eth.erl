-module(eth).

-export([convert/1]).

-export([is_convertible_proplist/1, is_string/1]).

convert(Term) ->
    if
        is_atom(Term) -> convert_atom(Term);
        is_binary(Term) -> convert_binary(Term);
        is_list(Term) -> convert_list(Term);
        is_tuple(Term) -> convert_tuple(Term);
        is_number(Term) -> Term;
        true -> throw({unrecognized_term, Term})
    end.

convert_atom(Atom) when Atom =:= true; Atom =:= false; Atom =:= null ->
    Atom;

convert_atom(Atom) ->
    case is_reserved_atom(Atom) of
        true -> <<"__atom_", (atom_to_binary(Atom, utf8))/binary>>;
        false -> atom_to_binary(Atom, utf8)
    end.

convert_binary(Binary) ->
    <<"__binary_", Binary/binary>>.

convert_string(String) ->
    <<"__string_", (list_to_binary(String))/binary>>.

convert_list(List) ->
    case is_string(List) of
        true -> convert_string(List);
        false ->
            case is_convertible_proplist(List) of
                true -> convert_proplist(List);
                false -> [convert(Term) || Term <- List]
            end
    end.

convert_tuple(Tuple) ->
    [<<"__tuple">>|[convert(Term) || Term <- tuple_to_list(Tuple)]].

convert_proplist(Proplist) ->
    [{convert(Key), convert(Value)} || {Key, Value} <- Proplist].

%% key must be atom, string, or binary
is_convertible_proplist(List) ->
    lists:all(fun is_2tuple/1, List).

is_2tuple({Key,_}) ->
    if
        is_atom(Key) -> true;
        is_binary(Key) -> true;
        true -> is_string(Key)
    end;

is_2tuple(_) ->
    false.

is_string(List) ->
    io_lib:printable_list(List).

%% reserved atoms are used to encode lists, tuples, atomss, binaries, and strings
%% a reserved atom is one that is prefixed with '__'
is_reserved_atom(Atom) when is_atom(Atom) ->
    case atom_to_list(Atom) of
        "__" ++ _ -> true;
        _ -> false
    end;

is_reserved_atom(_) ->
    false.
