%
%
%
-module(utils).
-export([coin/1]).



coin(P) -> P > rand:uniform().