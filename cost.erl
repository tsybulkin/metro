%
% This module contains functions that estimate diferent financial
% properties of the model like cost of construction and potential revenue
% 

-module(cost).
-export([estimate_profit/1
		]).


%% sale prices
-define(NO_LIFT_PENALTY, 0.95). % per each floor
-define(STUDIO_PRICE, 950). % USD per sq ft
-define(ONE_BEDROOM_PRICE, 810).
-define(TWO_BEDROOM_PRICE, 750).
-define(THREE_BEDROOM_PRICE, 700).



%% costs
-define(FLOOR_CONSTRUCTION_COST, 10). % per sq ft 
-define(STAIR_COST, 250). % per floor
-define(LIFT_FIXED_COST, 10000).
-define(LIFT_FLOOR_COST, 1000).

-define(BATHROOM_FIXED_COST, 2000).
-define(BATHROOM_FLOOR_COST, 8). %USD per sq ft
-define(BATHROOM_WALL_COST, 30). %USD per ft

-define(KITCHEN_FIXED_COST, 1000).
-define(KITCHEN_FLOOR_COST, 6).
-define(KITCHEN_WALL_COST, 20).

-define(BEDROOM_FLOOR_COST, 4).
-define(BEDROOM_WALL_COST, 15).

-define(LIVINGROOM_FLOOR_COST, 5).
-define(LIVING_WALL_COST, 16).




estimate_profit(Plan) -> 0.


