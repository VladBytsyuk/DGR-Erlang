-module(oset).
%-compile(export_all).
-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([max/1, min/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([subtract/2]).
-export([is_disjoint/2,is_subset/2]).
-export([fold/3,filter/2]).
-include_lib("eunit/include/eunit.hrl").



% Создание нового упорядоченного множества
% new() -> OSet.

new() ->
	{nil, black, nil, nil}.



% Является ли аргумент множеством
% is_set(Term) -> boolean().

is_set({nil, black, nil, nil}) ->
	true;

is_set({_, _, Left, Right}) ->
	is_set(Left) and is_set(Right);
	
is_set(_) ->
	false.
		

	
% Количество элементов в упорядоченном множестве
% size(OSet) -> int().	
	
size({nil, black, nil, nil}) ->
	0;	
	
size({_, _, Left, Right}) ->
	oset:size(Left) + 1 + oset:size(Right).



% Перевод упорядоченного множества в список
% to_list(OSet) -> [Elem].

to_list({nil, black, nil, nil}) ->
	[];

to_list({Key, _, Left, Right}) ->
	to_list(Left) ++ [Key] ++ to_list(Right).
	

	
% Перевод списка в упорядоченное множество
% from_list([Elem]) -> OSet.

from_list(List) ->
	lists:foldl(fun(Elem, OSet) -> add_element(Elem, OSet) end, new(), List).



% Максимальный элемент упорядоченного множества
% max(OSet) -> Elem.

max({Key, _, _, {nil, black, nil, nil}}) ->
	Key;
	
max({_, _, _, Right}) ->
	max(Right).


 
% Минимальный элемент упорядоченного множества
% min(OSet) -> Elem. 

min({Key, _, {nil, black, nil, nil}, _}) ->
	Key;
	
min({_, _, Left, _}) ->
	min(Left).



% Объединение нескольких упорядоченных множеств в одно
% union(OSetA, OSetB) -> OSet.

union(OSetA, {nil, black, nil, nil}) ->
	OSetA;
	
union(OSetA, {Key, _, Left, Right}) ->
	union(union(add_element(Key, OSetA), Left), Right).

% union([OSet]) -> OSet.

union([OSet1, OSet2 | []]) ->
	union(OSet1, OSet2);

union([OSet1, OSet2 | OSetsListTail]) ->
	OSetUnion = union(OSet1, OSet2),
	union([OSetUnion | OSetsListTail]).



% Пересечение нескольких упорядоченных множеств
% intersection(OSetA, OSetB) -> OSet.

intersection(OSetA, OSetB) ->
	intersection(OSetA, OSetB, {nil, black, nil, nil}).
	
intersection({nil, black, nil, nil}, _, Acc) ->
	Acc;	
	
intersection(OSetA, OSetB, Acc) ->
	OSetALeft = min(OSetA),
	OSetANew = del_element(OSetALeft, OSetA),
	IsElem = is_element(OSetALeft, OSetB),
	if 
		IsElem =:= true  ->	AccNew = add_element(OSetALeft, Acc),
						   	intersection(OSetANew, OSetB, AccNew);
		
		IsElem =:= false -> intersection(OSetANew, OSetB, Acc)
	end.


% intersection([OSet]) -> OSet.	
	
intersection([OSet1, OSet2 | []]) ->
	intersection(OSet1, OSet2);

intersection([OSet1, OSet2 | OSetsListTail]) ->
	OSetIntersection = intersection(OSet1, OSet2),
	intersection([OSetIntersection | OSetsListTail]).



% Вычитание одного упорядоченного множества из другого
% subtract(OSetA, OSetB) -> OSet.

subtract(OSetA, {nil, black, nil, nil}) ->
	OSetA;

subtract(OSetA, OSetB) ->
	OSetBLeft = min(OSetB),
	OSetBNew = del_element(OSetBLeft, OSetB),
	IsElem = is_element(OSetBLeft, OSetA),
	if 
		IsElem =:= true  ->  OSetANew = del_element(OSetBLeft, OSetA),
						     subtract(OSetANew, OSetBNew);

		IsElem =:= false ->  subtract(OSetA, OSetBNew)
	end.	 	



% Является ли одно упорядоченное множество подмножеством другого
% is_subset(OSetA, OSetB) -> boolean().
	
is_subset({nil, black, nil, nil}, _) ->
	true;	
	
is_subset({Key, _, Left, Right}, OSetB) ->
	IsElem = is_element(Key, OSetB),
	if
		IsElem  -> is_subset(Left, OSetB) and is_subset(Right, OSetB);
		true 	-> false
	end.
	
% Являются ли упорядоченные множества непересекающимися
% is_disjoint(OSetA, OSetB) -> boolean().

is_disjoint(_, {nil, black, nil, nil}) ->
	true;	

is_disjoint(OSetA, {Key, _, Left, Right}) ->
	IsElem = is_element(Key, OSetA),
	if
		IsElem	-> false;
		true	-> is_disjoint(OSetA, Left) and is_disjoint(OSetA, Right)
	end.
	
% Применение функции к каждому элементу упорядоченного множества
% fold(Fun, Acc, OSet) -> Acc. 

fold(_, Acc, {nil, black, nil, nil}) ->
	Acc;	
	
fold(Fun, Acc, {Key, _, Left, Right}) ->
	Fun(Fun(Key, fold(Fun, Acc, Left)), fold(Fun, Acc, Right)).
	
	
% Фильтрует упорядоченное множество с помощью предиката
% filter(Fun, OSet) -> OSet.

filter(Pred, OSet) ->
	OSetList = to_list(OSet),
	FilteredList = lists:filter(Pred, OSetList),
	from_list(FilteredList).					

% Принадлежит ли элемент упорядоченному множеству
% is_element(Elem, OSet) -> boolean().

is_element(_,  {nil, black, nil, nil}) ->
	false;				
	
is_element(Elem, {Elem, _, _, _}) ->
	true;
	
is_element(Elem, {CurrElem, _, Left, Right}) ->		
	if
		Elem < CurrElem -> is_element(Elem, Left);
		Elem > CurrElem -> is_element(Elem, Right)
	end.	



% Добавление элемента в упорядоченное множество
% add_element(Elem, OSet) -> OSet.

add_element(Key, Tree) ->
	make_black(ins(Key, Tree)).


ins(Key, {nil, black, nil, nil}) ->
    {Key, red, {nil, black, nil, nil}, {nil, black, nil, nil}};
 
ins(Key, {Key, Color, Left, Right})	->
    {Key, Color, Left, Right};
  
ins(Key, {Key1, Color, Left, Right}) when Key < Key1 ->
	balance({Key1, Color, ins(Key, Left), Right});
  
ins(Key, {Key1, Color, Left, Right}) when Key > Key1 ->
	balance({Key1, Color, Left, ins(Key, Right)}).

% Окраска узла в черный цвет

make_black({Key, _, Left, Right }) ->
  {Key, black, Left, Right}.

% Исправление дерева после вставки, для сохранения RB-свойств

% Случай 1
%======================================================================================
%			   Key1(b)									   Key2(r)					 ||
%			/			\								/			\				 ||
%		 Left1		  Key2(r)						Key1(b)		  Key3(b)			 ||
%					/		  \			====>	   /       \      /     \			 ||
%  				Left2		Key3(r)				Left1    Left2  Left3  Right3		 ||	
%						   /       \												 ||
%						Left3    Right3											 	 ||
balance({Key1, black, Left1, {Key2, red, Left2, {Key3, red, Left3, Right3}}}) ->    %||
	{Key2, red, {Key1, black, Left1, Left2}, {Key3, black, Left3, Right3}};		    %||
%====================================================================================== 			

% Случай 2	
%======================================================================================
%			   Key1(b)									   Key3(r)					 ||
%			/			\								/			\				 ||
%		 Left1		  Key2(r)						Key1(b)		  Key3(b)			 ||
%					/		  \			====>	   /       \      /     \			 ||
%  				Key3(r)		Right2				Left1    Left3  Right3  Right2		 ||	
%				/	  \	   														 	 ||
%			Left3	Right3														 	 ||
balance({Key1, black, Left1, {Key2, red, {Key3, red, Left3, Right3}, Right2}}) ->   %||
	{Key3, red, {Key1, black, Left1, Left3}, {Key2, black, Right3, Right2}};	    %||
%======================================================================================

% Случай 3
%======================================================================================
%			   Key1(b)									   Key2(r)					 ||
%			/			\								/			\				 ||
%		 Key2(r)		Right1						Key3(b)		  Key1(b)			 ||
%		/		\			 			====>	   /       \      /     \			 ||
%  	 Key3(r)	Right2							Left3    Right3 Right2  Right1		 ||	
%	 /	    \			   															 ||
%  Left3   Right3																	 ||  
balance({Key1, black, {Key2, red, {Key3, red, Left3, Right3}, Right2}, Right1}) ->  %||
	{Key2, red, {Key3, black, Left3, Right3}, {Key1, black, Right2, Right1}};	    %||
%======================================================================================  

% Случай 4
%======================================================================================
%			   Key1(b)									   Key3(r)					 ||
%			/			\								/			\				 ||
%		 Key2(r)		Right1						Key2(b)		  Key1(b)			 ||
%		/		\			 			====>	   /       \      /     \			 ||
%  	 Left2	   Key3(r)							Left2    Left3 Right3  Right1		 ||	
%	 	 	   /     \																 ||
%  		 	Left3		Right3														 || 
balance({Key1, black, {Key2, red, Left2, {Key3, red, Left3, Right3}}, Right1}) ->   %||
	{Key3, red, {Key2, black, Left2, Left3}, {Key1, black, Right3, Right1}};	    %||
%======================================================================================

% Случай 5 (RB-свойства не нарушены)  
balance(Tree) -> 
	Tree.


	
% Удаление элемента из упорядоченного множества
% del_element(Elem, OSet) -> OSet.	

del_element(Key, Tree) -> 
	nilFix(make_black(del(Key, Tree))).
	
% Дерево пусто	
del(_, {nil, black, nil, nil}) ->
	{nil, black, nil, nil};
	
% Удаление красного узла с одним потомком
del(Key, {Key, red, Left, {nil, black, nil, nil}}) ->
	Left;
	
del(Key, {Key, red, {nil, black, nil, nil}, Right})	->
	Right;
	
% Удаление черного узла с одним потомком
del(Key, {Key, black, Left, {nil, black, nil, nil}}) ->
	addBlack(Left);
	
del(Key, {Key, black, {nil, black, nil, nil}, Right}) ->
	addBlack(Right);

% Удаление узла с двумя потомками
del(Key, {Key, Color, Left, Right})	->
	delFix({max(Left), Color, del(max(Left), Left), Right});
	
del(Key, {KeyTree, ColorTree, LeftTree, RightTree}) when Key < KeyTree 	->
	delFix({KeyTree, ColorTree, del(Key, LeftTree), RightTree});
	
del(Key, {KeyTree, ColorTree, LeftTree, RightTree}) when Key > KeyTree 	->	
	delFix({KeyTree, ColorTree, LeftTree, del(Key, RightTree)}).

% Добавление черного цвета
addBlack({Key, red, Left, Right}) ->
	{Key, black, Left, Right};
	
addBlack({Key, black, Left, Right})	->
	{Key, doubleBlack, Left, Right}.
			
			
% Восстановление черного цвета в листьях дерева
nilFix({nil, doubleBlack, nil, nil}) ->
	{nil, black, nil, nil};
	
nilFix(Tree) ->
	Tree.					
	
% Удаление с исправлением RB-свойств и установлением цветов
% Случай 1
%======================================================================================	
%							   	       Key2(c)										 ||
%   	      				/    			       \								 ||
%  						 Key1(bb)  			      Key3(b)							 ||
%        					/     \              /            \						 ||
%     				Left1    Right1  Left3Key(b)        Right3Key(b)				 ||
%               				       /      \            /        \				 ||
%              					Left3Left  Left3Right  Right3Left  Right3Right		 ||
%																			 		 ||
%										||									 		 ||
%										||									 		 ||
%									   \||/									 		 ||
%										\/									 		 ||
%																			 		 ||
%									   Key2(c+1)							 		 ||
%							/					       \					 		 ||
%						Key1(b)					     Key3(r)				 		 ||
%						/     \					/                \			 		 ||
%    				Left1   Right1       Left3Key(b)	      Right3Key(b)	 		 ||
%                  				         /        \          /          \	 		 ||
% 									Left3Left  Left3Right Right3Left  Right3Right	 ||
%------------------------------------------------------------------------------------||
delFix({Key2, 														   				%||
	    Color2, 													   				%||
	    {Key1, doubleBlack, Left1, Right1}, 						   				%||
	    {Key3, black, {Left3Key,  black, Left3Left,  Left3Right}, 	   				%||
	                  {Right3Key, black, Right3Left, Right3Right}	   				%||
	    }															   				%||
	   })															   				%||
->																	   				%||
	addBlack({Key2, 												   				%||
	          Color2, 												   				%||
	          nilFix({Key1, black, Left1, Right1}), 				   				%||
	          {Key3, red, {Left3Key,  black, Left3Left,  Left3Right},  				%||
	                      {Right3Key, black, Right3Left, Right3Right}  				%||
	          }     												   				%||
	         });													   	 			%||
%======================================================================================	         

% Случай 2
%======================================================================================	
%			                 	     Key2(c)										 ||
%   	             	/    			      			    \						 ||
%  			    	 Key1(b)  		            		  Key3(bb)					 ||
%            	/                 \                 	  /      \					 ||
%     	  Left1Key(b)              Right1Key(b)       Left3    Right3				 ||
%   	/             \          /		        \									 ||
% 	Left1Left   Left1Right  Right1Left  Right1Right      							 ||
%																					 ||
%										||											 ||
%										||											 ||
%									   \||/											 ||
%										\/											 ||
%																					 ||
%					              Key2(c+1)											 ||
%			             /   					       \							 ||
%		             Key1(r)					     Key3(b)						 ||
%		      /     			\					/       \						 ||
%      Left1Key(b) 			    Right1Key(b)     Left3	  Right3					 ||
%   /              \         /       	    \            							 ||
% Left1Left   Left1Right  Right1Left  Right1Right									 ||
%------------------------------------------------------------------------------------||          
delFix({Key2, 														   				%||
	    Color2, 													   				%||
	    {Key1, black, {Left1Key,  black, Left1Left,  Left1Right}, 	   				%||
	                  {Right1Key, black, Right1Left, Right1Right}},    				%||
	    {Key3, doubleBlack, Left3, Right3}							   				%||
	   }) 															   				%||
->																	   				%||
	addBlack({Key2, 												   				%||
	          Color2, 												   				%||
	          {Key1, red, {Left1Key,  black, Left1Left,  Left1Right},  				%||
	                      {Right1Key, black, Right1Left, Right1Right}},				%||
	          nilFix({Key3, black, Left3, Right3})					   				%||
	         });													   				%||
%======================================================================================

% Случай 3   
%======================================================================================	
%			   	     				  Key2(b)										 ||
%   	    				     /    		        \						 		 ||
%  		 				    Key1(bb)  			     Key3(r)						 ||
%        					/     \              /            \						 ||
%    				 Left1    Right1  Left3Key(b)        Right3Key(b)				 ||
%                  					    /      \            /        \				 ||
%              					Left3Left  Left3Right  Right3Left  Right3Right		 ||
%																					 ||
%										||											 ||
%										||											 ||
%									   \||/											 ||
%										\/											 ||
%																			 		 ||
%					 		 		 Key3(b)										 ||
%							/						       \						 ||
%delFix:				Key2(r)					    	 Right3Key(b)				 ||
%				/     				\					/            \				 ||
%   		Key1(bb)   			Left3Key(b)	      Right3Left	 Right3Right	 	 ||
%   		/      \            /         \											 ||
% 		Left1	  Right1	Left3Left  Left3Right									 ||
%------------------------------------------------------------------------------------||	   
delFix({Key2, 														   				%||							
	    black, 														   				%||
	    {Key1, doubleBlack, Left1, Right1}, 						   				%||
	    {Key3, red, {Left3Key,  black, Left3Left,  Left3Right}, 	   				%||
	                {Right3Key, black, Right3Left, Right3Right}		   				%||
	    }															   				%||
	   })															   				%||
->																	   				%||
	{Key3, 															   				%||
	 black, 														   				%||
	 delFix({Key2, red, {Key1,     doubleBlack, Left1, 	   Right1},    				%||
	 					{Left3Key, black, 		Left3Left, Left3Right}}),			%|| 
	 {Right3Key, black, Right3Left, Right3Right} 									%||
	};																				%||
%======================================================================================

% Случай 4
%======================================================================================	
%			                    	  Key2(b)									 	 ||
%   	             	/    			      			    \						 ||
%  			     	Key1(r)  		            		  Key3(bb)					 ||
%            	/                 \                 	  /      \				 	 ||
%     	  Left1Key(b)              Right1Key(b)       Left3    Right3				 ||
%   	/             \          /		        \									 ||
% 	Left1Left   Left1Right  Right1Left  Right1Right      							 ||
%																					 ||
%										||											 ||
%										||											 ||
%									   \||/											 ||
%										\/											 ||
%																					 ||
%							         Key1(b)									 	 ||
%	     		 	/   					      		 \					 		 ||
%      		  Left1Key(b)				delFix:	    	Key2(r)						 ||
%  			 /	  		\						/                   \				 ||
% 		Left1Left 	 Left1Right 		Right1Key(b)               Key3(bb)			 ||
%   		          					/          \			   /      \          ||
% 							     Right1Left  Right1Right 		Left3    Right3		 ||
%------------------------------------------------------------------------------------||          
delFix({Key2,																		%||
   	    black,																		%||
   	    {Key1, red, {Left1Key,  black, Left1Left,  Left1Right},						%||
   	                {Right1Key, black, Right1Left, Right1Right}},					%||
   	    {Key3, doubleBlack, Left3, Right3}											%||
   	   })																			%||
->																					%||
	{Key1,																			%||
	 black,																			%||							
	 {Left1Key,  black, Left1Left,  Left1Right},									%||
	 delFix({Key2, red, {Right1Key, black, 		 Right1Left, Right1Right},			%||
	 					{Key3, 		doubleBlack, Left3, 	 Right3}})				%||
	};																				%||
%======================================================================================

% Случай 5
%======================================================================================	
%			   	    				   Key2(c)										 ||
%   	  				        /    			       \							 ||
%  		 				    Key1(bb)  			      Key4(b)						 ||
%        					/     \                /            \					 ||
%     					Left1    Right1        Key3(r)        Right4Key(b)			 ||
%                   				    	   /      \ 	     /        \			 ||
%                							Left3    Right3  Right4Left  Right4Right ||
%																					 ||
%										||											 ||
%										||											 ||
%									   \||/											 ||
%										\/											 ||
%																					 ||
%		delFix:			Key2(c)												 		 ||
%			/					     	  \									 		 ||
%		Key1(bb)					     Key3(b)								 	 ||
%		/     \						/                \							 	 ||
%	Left1   Right1 				Left3	      Right3Key(b)					 		 ||
%         				          	          /          \					 		 ||
%										Right3Left 		Key4(r)						 ||
%														/     \						 ||
%  				                                     Right3  Right4Key(b)			 ||
%  				                                             /          \			 ||
%        		                                       Right4Left     Right4Right	 ||
%------------------------------------------------------------------------------------||	
delFix({Key2,																		%||
		Color2,																		%||
		{Key1, doubleBlack, Left1, Right1},											%||
		{Key4, black, {Key3, red, Left3, Right3}, 									%||
					  {Right4Key, black, Right4Left, Right4Right}}					%||
	   })																			%||
->																					%||
	delFix({Key2,																	%||
		    Color2,																	%||
		    {Key1, doubleBlack, Left1, Right1},										%||
		    {Key3, black, Left3, 													%||
		    			  {Key4, red, Right3,										%|| 
		    						 {Right4Key, black, Right4Left, 				%||
		    						 					Right4Right}}}				%||
	});																				%||
%======================================================================================

% Случай 6
%======================================================================================	
%			                     	 Key3(c)										 ||
%   	           	 	/    			      			    \						 ||
%  			     	Key1(b)  		            		  Key4(bb)					 ||
%            	/                 \                 	  /      \					 ||
%     		Left1Key(b)              Key2(r)  	       Left4    Right4				 ||
%   	   /           \           /       \ 										 ||
% 		Left1Left   Left1Right  Left2  	Right2			   							 ||
%																					 ||
%										||											 ||
%										||											 ||
%									   \||/											 ||
%										\/											 ||
%																					 ||
%		delFix:			            Key3(c)											 ||
%			             /   					       \							 ||
%		             Key2(b)					     Key4(bb)						 ||
%		      /     			\					/       \						 ||
%      	   Key1(r) 			    Right2     		  Left4	  Right4					 ||
%         /       \      					              							 ||
%     Left1key    Left2  															 ||
%	 /        \																		 ||
% Left1Left Left1Right																 ||
%------------------------------------------------------------------------------------||
delFix({Key3,																		%||
		Color3,																		%||
		{Key1, black, {Left1Key, black, Left1Left, Left1Right}, 					%||
					  {Key2, 	 red,   Left2, 	   Right2    }},					%||
		{Key4, doubleBlack, Left4, Right4}											%||
	   })																			%||
->																					%||
	delFix({Key3, 																	%||
			Color3,																	%||
			{Key2, black, {Key1, 													%||
						   red, 													%||
						   {Left1Key, black, Left1Left, Left1Right},				%||
						   Left2},													%||
						  Right2},													%||
			{Key4, doubleBlack, Left4, Right4}										%||
	});																				%||
%======================================================================================

% Случай 7
%======================================================================================	
%			   	      				 Key2(c)										 ||
%   					      /    			       \								 ||
%  						 Key1(bb)  			      Key3(b)							 ||
%       				 /     \              /            \						 ||
% 				      Left1    Right1      Left3	      Key4(r)					 ||
%                       		 	     				/        \					 ||
%                									  Left4  Right4Key(b)			 ||
%															 /          \			 ||
%                                   				 Right4Left      Right4Right	 ||
%                                                                                    ||
%										||											 ||
%										||											 ||
%									   \||/											 ||
%										\/											 ||
%																					 ||
%					          		 Key3(c)										 ||
%			   			     /   					       \						 ||
% nilFix:				Key2(b)					     	Key4(b)						 ||
%    				/     	\						   /       \					 ||
%    		  Key1(b) 		Left3     Left3	  		Left4	   Right4Key(b)			 ||
%   		/       \                   					   /       \   			 ||
% 		Left1   Right1  								Right4Left	 Right4Right	 ||
%------------------------------------------------------------------------------------||
delFix({Key2,																		%||
		Color2,																		%||
		{Key1, doubleBlack, Left1, Right1},											%||
		{Key3, black, Left3,														%||
					  {Key4, red, Left4, 											%||
					 			  {Right4Key, black, Right4Left, Right4Right}}}		%||
	   })																			%||
->																					%||
	{Key3,																			%||
	 Color2,																		%||
	 {Key2, black, nilFix({Key1, black, Left1, Right1}),							%||
	               Left3},															%||
	 {Key4, black, Left4,															%||
				   {Right4Key, black, Right4Left, Right4Right}}						%||
	};																				%||
%======================================================================================

% Случай 8
%======================================================================================
%		       			            Key3(c)											 ||
%			             /   					       \							 ||
%		             Key2(b)					     Key4(bb)						 ||
%		      /     			\					/       \						 ||
%      	   Key1(r) 			    Right2     		  Left4	  Right4					 ||
%         /       \      					              							 ||
%     Left1key    Right1  															 ||
%	 /        \																		 ||
% Left1Left Left1Right																 ||
%                                                                                    ||
%										||											 ||
%										||											 ||
%									   \||/											 ||
%										\/											 ||
%																					 ||
%			                      	Key2(c)											 ||
%   	             /    			      			    \							 ||
%  			     Key1(b)  		            		  Key3(b)						 ||
%            /                 \                 	  /      \						 ||
%     Left1Key(b)             Right1  	         Right2    Key4(b)	:nilFix			 ||
%   /             \            							   /     \					 ||
% Left1Left   Left1Right  			   					Left4   Right4				 ||
%------------------------------------------------------------------------------------||
delFix({Key3,																		%||
		Color3,																		%||
		{Key2, black, {Key1, 														%||
					   red, 														%||
					   {Left1Key, black, Left1Left, Left1Right},					%||
					   Right1}, 													%||
					  Right2},														%||
		{Key4, doubleBlack, Left4, Right4}											%||
	   })																			%||
->																					%||
	{Key2,																			%||
	 Color3,																		%||
	 {Key1, black, {Left1Key, black, Left1Left, Left1Right},						%||
	 			   Right1},															%||
	 {Key3, black, Right2,															%||
	 			   nilFix({Key4, black, Left4, Right4})}							%||
	};																				%||
%======================================================================================

% Случай 9 (RB-свойства не нарушены и лишних цветов нет)
delFix(Tree) ->
	Tree.

