%%%-------------------------------------------------------------------
%%% @author ngq
%%% @copyright (C) 2023, <ngq_scut@126.com>
%%% @doc
%%%     九宫格函数
%%% @end
%%% Created : 21. 5月 2023 下午1:47
%%%-------------------------------------------------------------------
-module(squared_util).
-author("ngq").

-include("squared.hrl").

%% API
-export([adjust_path/1, turn_to/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 绝对路径，移除'..'
adjust_path(Path0) ->
    List0 = filename:split(filename:absname(Path0)),
    List1 = adjust_path(List0, []),
    filename:join(List1).

%% @doc 转向
-spec(turn_to(FromPos::squared:pos(), ToPos::squared:pos(), DefFaceTo::integer()) ->
    FaceTo::integer()).
turn_to({X0, Y0}, {X1, Y1}, DefFace) ->
    Vx =
        case X1 >= X0 of
            true -> min(1, X1 - X0);
            false -> max(-1, X1 - X0)
        end,
    Vy =
        case Y1 >= Y0 of
            true -> min(1, Y1 - Y0);
            false -> max(-1, Y1 - Y0)
        end,
    face({Vx, Vy}, DefFace).

%%%===================================================================
%%% Internal functions
%%%===================================================================

adjust_path([".."|T], []) ->
    adjust_path(T, []);
adjust_path([".."|T], [_|Res]) ->
    adjust_path(T, Res);
adjust_path([H|T], Res) ->
    adjust_path(T, [H|Res]);
adjust_path([], Res) ->
    lists:reverse(Res).

-compile({inline, [face/2]}).

face({0, 1}, _Def) -> ?SQUARED_UP;
face({1, 1}, _Def) -> ?SQUARED_UP_RIGHT;
face({-1, 1}, _Def) -> ?SQUARED_LEFT;
face({0, 0}, Def) -> Def;
face({1, 0}, _Def) -> ?SQUARED_RIGHT;
face({-1, 0}, _Def) -> ?SQUARED_LEFT;
face({0, -1}, _Def) -> ?SQUARED_DOWN;
face({1, -1}, _Def) -> ?SQUARED_DOWN_RIGHT;
face({-1, -1}, _Def) -> ?SQUARED_DOWN_LEFT.
