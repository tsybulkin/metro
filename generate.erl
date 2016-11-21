%
% This module contains generative model
%

-module(generate).
-export([init_plan/2,
		modify_plan/3, modify_simply/3
		]).

%% LOT CONSTRAINS
-define(MAX_FLOOR_NBR, 7).

%% bulding constrains
-define(LIFT_AFTER_N_FLOOR, 4).
-define(MIN_FLOORS_WITH_ONE_STAIR, 4).
-define(ONE_STAIR_AREA, 100). %sq ft
-define(LIFT_AREA, 20). %sq ft
-define(ATTEMPTS, 5).

%% unit area constrains
-define(MAX_APPT_SIZE, 1500).
-define(MIN_BEDROOM, 80).
-define(MIN_LIVINGROOM, 150).
-define(MIN_KITCHEN, 60).
-define(MIN_BATHROOM, 48).
-define(MIN_APPT_SIZE, ?MIN_BATHROOM + ?MIN_KITCHEN + ?MIN_LIVINGROOM).


%%%%%%%%%%%%%%%%% Plan structure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plan = {Lift, Floor_area, Floors}, where
% Lift:  true/false
% Floor_area: the area of one floor in sq ft
% Floors = [Floor|_]; 
% Floor = [{Kitchen_size,Bathroom_size,Livingroom_size,Bedroom_sizes}|Appartments]
%

init_plan(X,Y) when X*Y - ?ONE_STAIR_AREA < ?MIN_APPT_SIZE -> 
	error("The lot size is too small. Unable to generate the floor plan~n");
init_plan(X,Y) -> 
	Floors_nbr = rand:uniform(?MAX_FLOOR_NBR),
	Lift = sample_lift(Floors_nbr),
	Floor_area = case Lift of
		true -> X*Y - ?LIFT_AREA - ?ONE_STAIR_AREA;
		false-> X*Y - ?ONE_STAIR_AREA
	end,
	case Floor_area < ?MIN_APPT_SIZE of
		true -> init_plan(X,Y);
		false->{Lift, Floor_area, sample_floors(Floor_area, Floors_nbr)}
	end.



modify_simply(X,Y,_) -> init_plan(X,Y).



modify_plan(X,Y,{Lift,Area,Floors}) -> 
	N = length(Floors),
	N1 = max(round(N + rand:normal()/2),1),
	case N < ?LIFT_AFTER_N_FLOOR andalso utils:coin(0.1) of
		true -> {not Lift, Area + factor(Lift)*?LIFT_AREA, sample_floors(Area + factor(Lift)*?LIFT_AREA,length(Floors))};
		false->
			if
				N == 1 andalso N1 < N -> {Lift,Area,change_floor(Floors,?ATTEMPTS)};
				N1 < N -> {Lift, Area, remove_floor(Floors,?ATTEMPTS)};				
				N1 > N andalso N < ?LIFT_AFTER_N_FLOOR -> 
					{Lift, Area, add_floor(Floors,?ATTEMPTS)};
				N1 > N andalso N < ?MAX_FLOOR_NBR andalso Lift -> 
					{Lift, Area, add_floor(Floors,?ATTEMPTS)};
				N1 > N andalso N < ?MAX_FLOOR_NBR -> 
					{not Lift, Area - ?LIFT_AREA, sample_floors(Area - ?LIFT_AREA,N+1)};
				N1 >= N -> {Lift,Area,change_floor(Floors,?ATTEMPTS)}
			end
	end.


factor(true) -> 1;
factor(false) -> -1.



remove_floor(Floors,0) -> sample_floors(area(Floors),length(Floors)-1);
remove_floor(Floors,N) ->
	F = rand:uniform(length(Floors)-1),
	{Ls1,[_|Ls2]} = lists:split(F,Floors),
	Floors1 = Ls1 ++ Ls2,
	case cost:appts_proportion_fits(Floors1) of
		true -> Floors1;
		false-> remove_floor(Floors,N-1)
	end.


add_floor(Floors,0) -> sample_floors(area(Floors),length(Floors)+1);
add_floor(Floors,N) ->
	F = generate_floor(area(Floors)),
	case cost:appts_proportion_fits([F|Floors]) of
		true -> [F|Floors];
		false-> add_floor(Floors,N-1)
	end.


change_floor(Floors,0) -> sample_floors(area(Floors),length(Floors));
change_floor(Floors,N) ->
	J = rand:uniform(length(Floors))-1,
	F = generate_floor(area(Floors)),
	{Ls1,[_|Ls2]} = lists:split(J,Floors),
	Floors1 = Ls1 ++ [F|Ls2],
	
	case cost:appts_proportion_fits(Floors1) of
		true -> Floors1;
		false-> change_floor(Floors,N-1)
	end.



sample_lift(Floors_nbr) ->
	case Floors_nbr > ?LIFT_AFTER_N_FLOOR of
		true -> true;
		false-> utils:coin((Floors_nbr-1)/?LIFT_AFTER_N_FLOOR)
	end.



sample_floors(Area,Floors_nbr) ->
	Floors = [ generate_floor(Area) || _ <- lists:seq(1,Floors_nbr)],
	case cost:appts_proportion_fits(Floors) of
		true -> Floors;
		false-> 
			%io:format("appartments proportion in the building violates constrains~n"),
			sample_floors(Area,Floors_nbr)
	end.



generate_floor(Area) -> generate_floor(Area,[]).

generate_floor(Area, Appts) ->
	Appt_area = round(?MIN_APPT_SIZE * ( 2*abs(rand:normal()) + 1 )),			
	if 
		Area - Appt_area =< ?MIN_APPT_SIZE -> [generate_appt(Area) | Appts];
		true -> generate_floor(Area-Appt_area, [generate_appt(Appt_area) | Appts])
	end.



generate_appt(Area) when Area =< ?MIN_APPT_SIZE + ?MIN_BEDROOM -> generate_studio(Area,?ATTEMPTS);
generate_appt(Area) when Area =< ?MIN_APPT_SIZE + 2*?MIN_BEDROOM -> 
	case utils:coin(0.5) of
		true -> generate_studio(Area,?ATTEMPTS);
		false-> generate_one_bd(Area,?ATTEMPTS)
	end;
generate_appt(Area) when Area =< ?MIN_APPT_SIZE + 3*?MIN_BEDROOM -> 
	case utils:coin(0.2) of
		true -> generate_studio(Area,?ATTEMPTS);
		false-> 
			case utils:coin(0.4) of
				true -> generate_one_bd(Area,?ATTEMPTS);
				false-> generate_two_bd(Area,?ATTEMPTS)
			end
	end;
generate_appt(Area) -> 
	case utils:coin(0.1) of
		true -> generate_studio(Area,?ATTEMPTS);
		false-> 
			case utils:coin(0.2) of
				true -> generate_one_bd(Area,?ATTEMPTS);
				false-> 
					case utils:coin(0.5) of
						true -> generate_two_bd(Area,?ATTEMPTS);
						false-> generate_three_bd(Area,?ATTEMPTS)
					end
			end
	end.


generate_studio(Area,0) -> 
	LR = Area - ?MIN_KITCHEN - ?MIN_BATHROOM,
	{?MIN_KITCHEN,?MIN_BATHROOM,LR,[]};
generate_studio(Area,N) ->
	Kitchen_size = round(?MIN_KITCHEN + abs(rand:normal())*?MIN_KITCHEN/5),
	Bathroom_size = round(?MIN_BATHROOM + abs(rand:normal())*?MIN_BATHROOM/5),
	Livingroom_size = Area - Kitchen_size - Bathroom_size,
	case Livingroom_size >= ?MIN_LIVINGROOM of
		true -> {Kitchen_size, Bathroom_size, Livingroom_size, []};
		false-> 
			%io:format("failed to generate studio. Area: ~p~n",[Area]), 
			generate_studio(Area,N-1)
	end.

generate_one_bd(Area,0) -> 
	LR = Area - ?MIN_KITCHEN - ?MIN_BATHROOM - ?MIN_BEDROOM,
	{?MIN_KITCHEN,?MIN_BATHROOM,LR,[?MIN_BEDROOM]};
generate_one_bd(Area,N) ->
	Kitchen_size = round(?MIN_KITCHEN + abs(rand:normal())*?MIN_KITCHEN/5),
	Bathroom_size = round(?MIN_BATHROOM + abs(rand:normal())*?MIN_BATHROOM/5),
	Bedroom_size = round(?MIN_BEDROOM +abs(rand:normal())*?MIN_BEDROOM/5),
	Livingroom_size = Area - Kitchen_size - Bathroom_size - Bedroom_size,
	case Livingroom_size >= ?MIN_LIVINGROOM of
		true -> {Kitchen_size, Bathroom_size, Livingroom_size, [Bedroom_size]};
		false-> 
			%io:format("failed to generate 1-bd appt. Area: ~p~n",[Area]), 
			generate_one_bd(Area,N-1)
	end.


generate_two_bd(Area,0) -> 
	LR = Area - ?MIN_KITCHEN - ?MIN_BATHROOM - 2*?MIN_BEDROOM,
	{?MIN_KITCHEN,?MIN_BATHROOM,LR,[?MIN_BEDROOM,?MIN_BEDROOM]};
generate_two_bd(Area,N) ->
	Kitchen_size = round(?MIN_KITCHEN + abs(rand:normal())*?MIN_KITCHEN/5),
	Bathroom_size = round(?MIN_BATHROOM + abs(rand:normal())*?MIN_BATHROOM/5),
	Bedroom_size1 = round(?MIN_BEDROOM + abs(rand:normal())*?MIN_BEDROOM/5),
	Bedroom_size2 = round(?MIN_BEDROOM + abs(rand:normal())*?MIN_BEDROOM/5),
	Livingroom_size = Area - Kitchen_size - Bathroom_size - Bedroom_size1 - Bedroom_size2,
	case Livingroom_size >= ?MIN_LIVINGROOM of
		true -> {Kitchen_size, Bathroom_size, Livingroom_size, [Bedroom_size1, Bedroom_size2]};
		false-> 
			%io:format("failed to generate 2-bd appt. Area: ~p~n",[Area]), 
			generate_two_bd(Area,N-1)
	end.



generate_three_bd(Area,0) -> 
	LR = Area - ?MIN_KITCHEN - ?MIN_BATHROOM - 3*?MIN_BEDROOM,
	{?MIN_KITCHEN,?MIN_BATHROOM,LR,[?MIN_BEDROOM,?MIN_BEDROOM,?MIN_BEDROOM]};
generate_three_bd(Area,N) ->
	Kitchen_size = round(?MIN_KITCHEN + abs(rand:normal())*?MIN_KITCHEN/5),
	Bathroom_size = round(?MIN_BATHROOM + abs(rand:normal())*?MIN_BATHROOM/5),
	Bedroom_size1 = round(?MIN_BEDROOM +abs(rand:normal())*?MIN_BEDROOM/5),
	Bedroom_size2 = round(?MIN_BEDROOM +abs(rand:normal())*?MIN_BEDROOM/5),
	Bedroom_size3 = round(?MIN_BEDROOM +abs(rand:normal())*?MIN_BEDROOM/5),
	Livingroom_size = Area - Kitchen_size - Bathroom_size - Bedroom_size1 - Bedroom_size2 - Bedroom_size3,
	case Livingroom_size >= ?MIN_LIVINGROOM of
		true -> {Kitchen_size, Bathroom_size, Livingroom_size, [Bedroom_size1, Bedroom_size2, Bedroom_size3]};
		false-> 
			%io:format("failed to generate 3-bd appt. Area: ~p~n",[Area]), 
			generate_three_bd(Area,N-1)
	end.



area([F|_]) -> lists:sum([ K+B+Lr+lists:sum(Bds) || {K,B,Lr,Bds} <- F]).





