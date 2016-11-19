%
% This module contains generative model
%

-module(generate).
-export([init_plan/2,
		modify_plan/3
		]).

%% LOT CONSTRAINS
-define(MAX_FLOOR_NBR, 7).

%% bulding constrains
-define(LIFT_AFTER_N_FLOOR, 4).
-define(MIN_FLOORS_WITH_ONE_STAIR, 4).
-define(ONE_STAIR_AREA, 100). %sq ft
-define(LIFT_AREA, 20). %sq ft

%% unit area constrains
-define(MIN_BEDROOM, 80).
-define(MIN_LIVINGROOM, 150).
-define(MIN_KITCHEN, 60).
-define(MIN_BATHROOM, 48).



%%%%%%%%%%%%%%%%% Plan structure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plan = {Lift, Floor_area, Floors}, where
% Lift:  true/false
% Floor_area: the area of one floor in sq ft
% Floors = [Floor|_]; 
% Floor = [{Kitchen_size,Livingroom_size,Bathroom_size,Bedroom_sizes}|Appartments]
%

init_plan(X,Y) -> 
	Floor_nbr = rand:uniform(?MAX_FLOOR_NBR),
	Lift = sample_lift(Floor_nbr),
	Floor_area = X*Y,
	{Lift, Floor_area, sample_floors(Floor_area)}.





modify_plan(X,Y,Plan) -> Plan.




sample_lift(Floor_nbr) ->
	case Floor_nbr > ?LIFT_AFTER_N_FLOOR of
		true -> true;
		false-> utils:coin((Floor_nbr-1)/?LIFT_AFTER_N_FLOOR)
	end.



sample_floors(Area) -> [].



