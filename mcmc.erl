%
% this module contains the MC Metropolis algorithm
%

-module(mcmc).
-export([metropolis/3, run/3]).


run(X,Y,N) ->
	Plans = metropolis(X,Y,N),
	{Profit,{Lift,Area,Floors}} = lists:max(Plans),
	io:format("Best plan found:~n"),
	io:format("  ~p floors. Total area: ~p~n",[length(Floors),Area*length(Floors)]),
	io:format("  Lift: ~p~n",[pp(Lift)]),
	show_appt_stat(Floors),
	io:format("-----~nProfit: ~p~n",[Profit]).

metropolis(X,Y,N) -> 
	InitPlan = generate:init_plan(X,Y),
	Profit = cost:estimate_profit(InitPlan),
	metropolis(X,Y,N-1,[{Profit,InitPlan}]).

metropolis(_X,_Y,0,Accepted) -> Accepted;
metropolis(X,Y,N,[{LastProfit,LastPlan}|_]=Accepted) -> 
	if N rem 10 == 0 -> io:format("~p iterations remain~n",[N]); true -> ok end,
	NewPlan = generate:modify_plan(X,Y,LastPlan),
	NewProfit = cost:estimate_profit(NewPlan),
	
	case (Delta = NewProfit - LastProfit) >= 0.0 of
		true -> metropolis(X,Y,N-1,[{NewProfit,NewPlan}|Accepted]);
		false->
			case utils:coin(math:exp(Delta/2000)) of
				true -> metropolis(X,Y,N-1,[{NewProfit,NewPlan}|Accepted]);
				false-> metropolis(X,Y,N-1,Accepted)
			end
	end.


show_appt_stat(Floors) -> 
	D = dict:from_list([{studio,[]},{one,[]},{two,[]},{three,[]}]),
	show_appt_stat(Floors,D).

show_appt_stat([F|Floors],Stat) ->
	Stat1 = lists:foldl(fun({_,_,_,Bds}=Appt,Acc)-> 
		Q = length(Bds),
		if 
			Q == 0 -> dict:append(studio,Appt,Acc);
			Q == 1 -> dict:append(one,Appt,Acc);
			Q == 2 -> dict:append(two,Appt,Acc);
			Q == 3 -> dict:append(three,Appt,Acc)
		end
						end,Stat,F),
	show_appt_stat(Floors,Stat1);
show_appt_stat([],Stat) ->
	io:format("  Studios:~n"),
	lists:foreach(fun(A)-> show_appt(A) end, dict:fetch(studio,Stat)),
	io:format("  1-bed:~n"),
	lists:foreach(fun(A)-> show_appt(A) end, dict:fetch(one,Stat)),
	io:format("  2-bed:~n"),
	lists:foreach(fun(A)-> show_appt(A) end, dict:fetch(two,Stat)),
	io:format("  3-bed:~n"),
	lists:foreach(fun(A)-> show_appt(A) end, dict:fetch(three,Stat)).
	
	

show_appt({K,B,L,Bds}) ->
	io:format("Kitchen:~p, Bath:~p, Livingroom:~p, ",[K,B,L]),
	io:format("Bedrooms: "),
	lists:foreach(fun(Bd)-> io:format("~p ",[Bd]) end, Bds),
	io:format("~n").






pp(true) -> "Yes";
pp(false) -> "No".