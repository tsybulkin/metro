%
% This module contains functions that estimate diferent financial
% properties of the model like cost of construction and potential revenue
% 

-module(cost).
-export([estimate_profit/1,
		appts_proportion_fits/1
		]).


%% sale prices
-define(NO_LIFT_PENALTY, 0.95). % per each floor starting from 2-nd
-define(STUDIO_PRICE, 950). % USD per sq ft
-define(ONE_BEDROOM_PRICE, 810).
-define(TWO_BEDROOM_PRICE, 750).
-define(THREE_BEDROOM_PRICE, 700).



%% construction costs
-define(FLOOR_CONSTRUCTION_COST, 10). % per sq ft 
-define(STAIR_COST, 600). % per floor
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

%%%  Plan structure  %%%%%%%%%%%%%%%%%%%%%%%%%
%
% Plan = {Lift, Floor_area, Floors}, where
% Lift:  true/false
% Floor_area: the area of one floor in sq ft
% Floors = [Floor|_]; 
% Floor = [{Kitchen_size,Bathroom_size,Livingroom_size,Bedroom_sizes}|Appartments]
%

estimate_profit(Plan) -> revenue(Plan) - cost(Plan).


revenue({Lift,_,Floors}) -> 
	lists:foldl(fun(Fl,Acc) -> 
		case Lift of
			true -> Acc + lists:sum([ get_price(Appt) || Appt <- Fl]);
			false-> Acc + lists:sum([ get_price(Appt) || Appt <- Fl]) * ?NO_LIFT_PENALTY
		end
				end,0,Floors).


get_price({Kit,Bath,LR,Bds}) when length(Bds) == 0 -> 
	?STUDIO_PRICE*(Kit+Bath+LR+lists:sum(Bds));
get_price({Kit,Bath,LR,Bds}) when length(Bds) == 1 -> 
	?ONE_BEDROOM_PRICE*(Kit+Bath+LR+lists:sum(Bds));
get_price({Kit,Bath,LR,Bds}) when length(Bds) == 2 -> 
	?TWO_BEDROOM_PRICE*(Kit+Bath+LR+lists:sum(Bds));
get_price({Kit,Bath,LR,Bds}) when length(Bds) == 3 -> 
	?THREE_BEDROOM_PRICE*(Kit+Bath+LR+lists:sum(Bds)).


	

cost(Plan) -> 0.


appts_proportion_fits(Floors) -> true.




