%%%-------------------------------------------------------------------
%%% @author ngq
%%% @copyright (C) 2023, <ngq_scut@126.com>
%%% @doc
%%%     九宫格地图
%%% @end
%%% Created : 21. 5月 2023 下午12:42
%%%-------------------------------------------------------------------
-author("ngq").

-ifndef(SQUARED_HRL).
-define(SQUARED_HRL, true).

-define(SQUARED_LAYER_N, 3).

%% 朝向
-define(SQUARED_CENTER,      0). %% 中点（原点）
-define(SQUARED_DOWN,        1). %% 下
-define(SQUARED_DOWN_LEFT,   2). %% 左下
-define(SQUARED_DOWN_RIGHT,  3). %% 右下
-define(SQUARED_UP,          4). %% 上
-define(SQUARED_UP_LEFT,     5). %% 左上
-define(SQUARED_UP_RIGHT,    6). %% 右上
-define(SQUARED_LEFT,        7). %% 左
-define(SQUARED_RIGHT,       8). %% 右

%% 全方向
-define(SCENE_ALL_DIRECTION, [
    ?SQUARED_CENTER, ?SQUARED_DOWN,
    ?SQUARED_DOWN_LEFT, ?SQUARED_DOWN_RIGHT,
    ?SQUARED_UP, ?SQUARED_UP_LEFT, ?SQUARED_UP_RIGHT,
    ?SQUARED_LEFT, ?SQUARED_RIGHT
]).
%% 八方向
-define(SCENE_OUT_DIRECTION, [
    ?SQUARED_DOWN, ?SQUARED_DOWN_LEFT, ?SQUARED_DOWN_RIGHT,
    ?SQUARED_UP, ?SQUARED_UP_LEFT, ?SQUARED_UP_RIGHT,
    ?SQUARED_LEFT, ?SQUARED_RIGHT
]).

-record(point, {
    id,
    out_pass = [],
    in_pass = [],
    neighbours = []
}).

-record(path, {
    sum = 0,
    x_sum = 0,
    y_sum = 0,
    list = [],
    from = {0, 0},
    to = []
}).

-endif.