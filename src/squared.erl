%%%-------------------------------------------------------------------
%%% @author ngq
%%% @copyright (C) 2023, <ngq_scut@126.com>
%%% @doc
%%%     九宫格地图接口
%%% @end
%%% Created : 31. 10月 2022 10:06
%%%-------------------------------------------------------------------
-module(squared).
-author("ngq").

-include("squared.hrl").

%% API
-export([search/3]).

-type pos() :: {X::integer(), Y::integer()}.
-export_type([pos/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 搜索路径
search(_Mod, P, P) ->
    [P];
search(Mod, {X0, Y0} = P0, {X1, Y1} = P1) ->
    Layer = Mod:get_layer(),
    Min = max(abs(X0 - X1), abs(Y0 - Y1)) + 1,
    L0 = do_search(Mod, Layer, 0, P0, P1),
    case check_best(L0, Min) of
        {ok, List} -> List;
        false ->
            L1 = do_search(Mod, Layer, 1, P0, P1),
            case check_best(L1, Min) of
                {ok, List} -> List;
                false ->
                    L2 = do_search(Mod, Layer, 2, P0, P1),
                    case check_best(L2, Min) of
                        {ok, List} -> List;
                        false ->
                            case sort_path(L0 ++ L1 ++ L2) of
                                [] -> [];
                                [#path{list = List}|_] -> List
                            end
                    end
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

search_path([#path{list = List}|T], Mod, Layer, Diff, {X0, Y0} = P, {X1, Y1} = Target,
    NextLayer, LayerP, LayerTarget) ->
    case sub_search_path(List, Mod, Layer, Diff, NextLayer, [LayerP], [LayerTarget], []) of
        false ->
            search_path(T, Mod, Layer, Diff, P, Target, NextLayer, LayerP, LayerTarget);
        {ok, [H|_] = _Path} when NextLayer == 1 ->
            [H];
        {ok, Path} ->
            NextLayer1 = NextLayer div 3,
            {LayerP1, LayerTarget1} =
                case NextLayer1 == 1 of
                    true ->
                        {
                            {X0, Y0},
                            {X1, Y1}
                        };
                    false ->
                        {
                            {(X0 + Diff) div NextLayer1, Y0 div NextLayer1},
                            {(X1 + Diff) div NextLayer1, Y1 div NextLayer1}
                        }
                end,
            case search_path(Path, Mod, NextLayer, Diff, P, Target, NextLayer1, LayerP1, LayerTarget1) of
                [] ->
                    search_path(T, Mod, Layer, Diff, P, Target, NextLayer, LayerP, LayerTarget);
                [H1|_] = _Path ->
                    [H1]
            end
    end;
search_path([], _Mod, _Layer, _Diff, _P, _Target, _NextLayer, _LayerP, _LayerTarget) ->
    [].

sub_search_path([{X, Y}], Mod, Layer, Diff, NextLayer, PList, TargetList, Res) ->
    #point{in_pass = InPass} = squared_code:get_point(Mod, Layer, Diff, X, Y),
    PDiff = Diff div NextLayer,
    LayerH = {erlang:trunc(X * ?SQUARED_LAYER_N) - PDiff, erlang:trunc(Y * ?SQUARED_LAYER_N)},
    Add = select_same(PList, Mod, NextLayer, Diff, LayerH, InPass, TargetList, []),
    case Add == [] of
        true -> false;
        false ->
            Path = add_path1(lists:reverse([Add | Res]), []),
            {ok, Path}
    end;
sub_search_path([{X0, Y0}, {X1, Y1} = H2|T], Mod, Layer, Diff, NextLayer, PList, TargetList, Res) ->
    #point{
        in_pass = InPass, out_pass = OutPass0
    } = squared_code:get_point(Mod, Layer, Diff, X0, Y0),
    #point{out_pass = OutPass1} = squared_code:get_point(Mod, Layer, Diff, X1, Y1),
    Direct = squared_util:turn_to({X0, Y0}, {X1, Y1}, ?SQUARED_CENTER),
    case lists:keyfind(Direct, 1, OutPass0) of
        {_, OutList} ->
            PDiff = Diff div NextLayer,
            LayerH1 = {
                erlang:trunc(X0 * ?SQUARED_LAYER_N) - PDiff,
                erlang:trunc(Y0 * ?SQUARED_LAYER_N)
            },
            Add = select_same(PList, Mod, NextLayer, Diff, LayerH1, InPass, OutList, []),
            case Add == [] of
                true -> false;
                false ->
                    case lists:keyfind(opposite(Direct), 1, OutPass1) of
                        {_, PList1} ->
                            sub_search_path([H2|T], Mod, Layer, Diff, NextLayer, PList1, TargetList, [Add | Res]);
                        false ->
                            false
                    end

            end;
        false -> false
    end.

add_path1([H|T], Res) ->
    L = add_path(Res, H),
    add_path2(L, T);
add_path1([], Res) -> sort_path(Res).

add_path2([H|T], Tail) ->
    case add_path1(Tail, [H]) of
        [] ->
            add_path2(T, Tail);
        [R|_] -> [R]
    end;
add_path2([], _Tail) -> [].

add_path([], Add) -> Add;
add_path(L, Add) -> add_path(L, Add, []).

add_path([#path{
    to = ToList, sum = Sum0, x_sum = XSum0, y_sum = YSum0, list = List0
}|T], Add, Res) ->
    Valid = lists:foldl(
        fun(V = #path{
            from = From, sum = Sum1, x_sum = XSum1, y_sum = YSum1, list = List1
        }, Acc) ->
            case lists:keyfind(From, 1, ToList) of
                {_, DX, DY} ->
                    [V#path{
                        sum = Sum0 + Sum1 + 1, x_sum = XSum0 + XSum1 + DX,
                        y_sum = YSum0 + YSum1 + DY, list = List0 ++ List1
                    }|Acc];
                false -> Acc
            end
        end, [], Add
    ),
    case Valid == [] of
        true ->
            add_path(T, Add, Res);
        false ->
            add_path(T, Add, Valid ++ Res)
    end;
add_path([], _Add, Res) -> sort_path(Res).

select_same([{X, Y} = H|T], Mod, Layer, Diff, LayerP, InPass, List, Res) ->
    case lists:member(H, List) of
        true ->
            select_same(T, Mod, Layer, Diff, LayerP, InPass, List, [#path{
                list = [H], from = H, to = squared_code:get_neighbours(Mod, Layer, Diff, X, Y)
            } | Res]);
        false ->
            Add = lists:foldl(
                fun(Target, Acc) ->
                    D0 = layer_nine_point(LayerP, H),
                    D1 = layer_nine_point(LayerP, Target),
                    case check_in_pass(Mod, Layer, Diff, InPass, H, Target, D0, D1, true) of
                        {ok, L} -> L ++ Acc;
                        false -> Acc
                    end
                end, [], List
            ),
            select_same(T, Mod, Layer, Diff, LayerP, InPass, List, Add ++ Res)
    end;
select_same([], _Mod, _Layer, _Diff, _LayerP, _InPass, _List, Res) ->
    sort_path(Res).

check_in_pass(Mod, Layer, Diff, List, From, To, D0, D1, _Bool) when D0 > D1 ->
    check_in_pass(Mod, Layer, Diff, List, From, To, D1, D0, false);
check_in_pass(Mod, Layer, Diff, List, From, {X, Y}, D0, D1, Bool) ->
    case lists:keyfind({D0, D1}, 1, List) of
        {_, Path} when Bool ->
            {ok, [P#path{
                from = From, to = squared_code:get_neighbours(Mod, Layer, Diff, X, Y)
            } || P <- Path]};
        {_, Path} ->
            {ok, [P#path{
                from = From, list = lists:reverse(PList),
                to = squared_code:get_neighbours(Mod, Layer, Diff, X, Y)
            }|| P = #path{list = PList} <- Path]};
        false -> false
    end.

layer_nine_point({X, Y}, {X, Y}) ->
    ?SQUARED_DOWN_LEFT;
layer_nine_point({X, Y0}, {X, Y1}) when Y0 + 1 == Y1 ->
    ?SQUARED_LEFT;
layer_nine_point({X, Y0}, {X, Y1}) when Y0 + 2 == Y1 ->
    ?SQUARED_UP_LEFT;
layer_nine_point({X0, Z}, {X1, Z}) when X0 + 1 == X1 ->
    ?SQUARED_DOWN;
layer_nine_point({X0, Y0}, {X1, Y1}) when X0 + 1 == X1 andalso Y0 + 1 == Y1 ->
    ?SQUARED_CENTER;
layer_nine_point({X0, Y0}, {X1, Y1}) when X0 + 1 == X1 andalso Y0 + 2 == Y1 ->
    ?SQUARED_UP;
layer_nine_point({X0, Z}, {X1, Z}) when X0 + 2 == X1 ->
    ?SQUARED_DOWN_RIGHT;
layer_nine_point({X0, Y0}, {X1, Y1}) when X0 + 2 == X1 andalso Y0 + 1 == Y1 ->
    ?SQUARED_RIGHT;
layer_nine_point({X0, Y0}, {X1, Y1}) when X0 + 2 == X1 andalso Y0 + 2 == Y1 ->
    ?SQUARED_UP_RIGHT.

opposite(?SQUARED_CENTER) -> ?SQUARED_CENTER;
opposite(?SQUARED_DOWN) -> ?SQUARED_UP;
opposite(?SQUARED_DOWN_LEFT) -> ?SQUARED_UP_RIGHT;
opposite(?SQUARED_DOWN_RIGHT) -> ?SQUARED_UP_LEFT;
opposite(?SQUARED_UP) -> ?SQUARED_DOWN;
opposite(?SQUARED_UP_LEFT) -> ?SQUARED_DOWN_RIGHT;
opposite(?SQUARED_UP_RIGHT) -> ?SQUARED_DOWN_LEFT;
opposite(?SQUARED_LEFT) -> ?SQUARED_RIGHT;
opposite(?SQUARED_RIGHT) -> ?SQUARED_LEFT.

check_best([#path{list = List}], Min) when length(List) == Min ->
    {ok, List};
check_best(_, _Min) -> false.

do_search(Mod, Layer, Diff, {X0, Y0}, {X1, Y1}) ->
    {LayerP0, LayerP1} =
        case Layer == 1 of
            true ->
                {
                    {X0, Y0},
                    {X1, Y1}
                };
            false ->
                {
                    {(X0 + Diff) div Layer, Y0 div Layer},
                    {(X1 + Diff) div Layer, Y1 div Layer}
                }
        end,
    L0 = search_nine(Mod, Layer, Diff, LayerP0, LayerP1, 0, 0, 0, [], []),
    L1 = get_element(L0, []),
    case L1 == [] of
        true -> [];
        false when Layer == 1 ->
            [H|_] = sort_path(L1),
            [H];
        false ->
            [Best|_] = _L2 = sort_path(L1),
            NextLayer = Layer div 3,
            {NextLayerP0, NextLayerP1} =
                case NextLayer == 1 of
                    true ->
                        {
                            {X0, Y0},
                            {X1, Y1}
                        };
                    false ->
                        {
                            {(X0 + Diff) div NextLayer, Y0 div NextLayer},
                            {(X1 + Diff) div NextLayer, Y1 div NextLayer}
                        }
                end,
            search_path([Best], Mod, Layer, Diff, {X0, Y0}, {X1, Y1}, NextLayer, NextLayerP0, NextLayerP1)
    end.

search_nine(_Mod, _Layer, _Diff, P, P, Sum, XSum, YSum, Path, _Done) ->
    [#path{
        sum = Sum, x_sum = XSum, y_sum = YSum,
        list = lists:reverse([P|Path])
    }];
search_nine(Mod, Layer, Diff, {X, Y} = P, Target, Sum, XSum, YSum, Path, Done) ->
    case squared_code:get_point(Mod, Layer, Diff, X, Y) of
        #point{neighbours = Neighbours0} ->
            {Neighbours1, Done1} = lists:foldl(
                fun({P0, _, _} = E, {Acc1, Acc2}) ->
                    case lists:member(P0, Path) orelse lists:member(P0, Done) of
                        true -> {Acc1, Acc2};
                        false ->
                            {[E|Acc1], [P0|Acc2]}
                    end
                end, {[], Done}, Neighbours0
            ),
            [search_nine(Mod, Layer, Diff, P1, Target, Sum + 1, XSum + DX, YSum + DY, [P|Path], Done1)
                || {P1, DX, DY} <- Neighbours1];
        undefined -> []
    end.

get_element([H|T], Res) when is_list(H) ->
    Add = get_element(H, []),
    get_element(T, Add ++ Res);
get_element([#path{list = []}|T], Res) ->
    get_element(T, Res);
get_element([#path{} = E|T], Res) ->
    get_element(T, [E|Res]);
get_element([], Res) ->
    Res.

sort_path(L) ->
    lists:sort(
        fun(#path{sum = Sum0, x_sum = XSum0, y_sum = YSum0},
            #path{sum = Sum1, x_sum = XSum1, y_sum = YSum1}) ->
            {Sum0, XSum0, YSum0} =< {Sum1, XSum1, YSum1}
        end, L).
