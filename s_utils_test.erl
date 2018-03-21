-module(s_utils_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

replaceListItem_test() ->
    [
        ?assert(s_utils:replaceListItem(-1, z, [a, b, c, d]) =:= {error, index_less_than_one}),
        ?assert(s_utils:replaceListItem( 0, z, [a, b, c, d]) =:= {error, index_less_than_one}),
        ?assert(s_utils:replaceListItem( 1, z, [a, b, c, d]) =:= [z, b, c, d]),
        ?assert(s_utils:replaceListItem( 2, z, [a, b, c, d]) =:= [a, z, c, d]),
        ?assert(s_utils:replaceListItem( 3, z, [a, b, c, d]) =:= [a, b, z, d]),
        ?assert(s_utils:replaceListItem( 4, z, [a, b, c, d]) =:= [a, b, c, z]),
        ?assert(s_utils:replaceListItem( 5, z, [a, b, c, d]) =:= {error, index_greater_than_list_size}),
        ?assert(s_utils:replaceListItem( 6, z, [a, b, c, d]) =:= {error, index_greater_than_list_size})
    ].

getListItem_test() ->
    [
        ?assert(s_utils:getListItem(-1, [a, b, c, d]) =:= {error, index_less_than_one}),
        ?assert(s_utils:getListItem( 0, [a, b, c, d]) =:= {error, index_less_than_one}),
        ?assert(s_utils:getListItem( 1, [a, b, c, d]) =:= a),
        ?assert(s_utils:getListItem( 2, [a, b, c, d]) =:= b),
        ?assert(s_utils:getListItem( 3, [a, b, c, d]) =:= c),
        ?assert(s_utils:getListItem( 4, [a, b, c, d]) =:= d),
        ?assert(s_utils:getListItem( 5, [a, b, c, d]) =:= {error, index_greater_than_list_size}),
        ?assert(s_utils:getListItem( 6, [a, b, c, d]) =:= {error, index_greater_than_list_size})
    ].

listLength_test() ->
    [
        ?assert(s_utils:listLength([]) =:= 0),
        ?assert(s_utils:listLength([a]) =:= 1),
        ?assert(s_utils:listLength([a, b]) =:= 2),
        ?assert(s_utils:listLength([a, b, c]) =:= 3),
        ?assert(s_utils:listLength([a, b, c, d]) =:= 4),
        ?assert(s_utils:listLength([a, b, c, d, e]) =:= 5),
        ?assert(s_utils:listLength([a, b, c, d, e, f]) =:= 6),
        ?assert(s_utils:listLength(a) =:= {error, not_list}),
        ?assert(s_utils:listLength({a}) =:= {error, not_list}),
        ?assert(s_utils:listLength({a, b}) =:= {error, not_list}),
        ?assert(s_utils:listLength(1) =:= {error, not_list}),
        ?assert(s_utils:listLength(2) =:= {error, not_list}),
        ?assert(s_utils:listLength(-1) =:= {error, not_list}),
        ?assert(s_utils:listLength(0) =:= {error, not_list})
    ].

sum_test() ->
    [
        ?assert(s_utils:sum([4, 5], [-3]) =:= {error, lists_have_different_lengths}),
        ?assert(s_utils:sum([4], [-3, 12]) =:= {error, lists_have_different_lengths}),
        ?assert(s_utils:sum([], [-3, 12]) =:= {error, lists_have_different_lengths}),
        ?assert(s_utils:sum([4, 5], []) =:= {error, lists_have_different_lengths}),
        ?assert(s_utils:sum([1, 2, 3], [1, 1, 1]) =:= [2, 3, 4]),
        ?assert(s_utils:sum([4, 2, 1], [1, 2, 3]) =:= [5, 4, 4]),
        ?assert(s_utils:sum([1], [2]) =:= [3]),
        ?assert(s_utils:sum([4, 5], [-3, 12]) =:= [1, 17]),
        ?assert(s_utils:sum([4, 5], a) =:= {error, arguments_contain_not_list}),
        ?assert(s_utils:sum([4, 5], {a}) =:= {error, arguments_contain_not_list}),
        ?assert(s_utils:sum([4, 5], {a, b}) =:= {error, arguments_contain_not_list}),
        ?assert(s_utils:sum([4, 5], 12) =:= {error, arguments_contain_not_list}),
        ?assert(s_utils:sum(a, [4, 5]) =:= {error, arguments_contain_not_list}),
        ?assert(s_utils:sum({a}, [4, 5]) =:= {error, arguments_contain_not_list}),
        ?assert(s_utils:sum({a, b}, [4, 5]) =:= {error, arguments_contain_not_list}),
        ?assert(s_utils:sum(12, [4, 5]) =:= {error, arguments_contain_not_list})
    ].

fillList_test() ->
    [
        ?assert(s_utils:fillList(-2, a) =:= {error, length_less_than_zero}),
        ?assert(s_utils:fillList(-1, a) =:= {error, length_less_than_zero}),
        ?assert(s_utils:fillList( 0, a) =:= []),
        ?assert(s_utils:fillList( 1, a) =:= [a]),
        ?assert(s_utils:fillList( 2, a) =:= [a, a]),
        ?assert(s_utils:fillList( 3, a) =:= [a, a, a]),
        ?assert(s_utils:fillList( 4, a) =:= [a, a, a, a])
    ].