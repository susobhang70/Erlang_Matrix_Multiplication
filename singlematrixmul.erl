-module(singlematrixmul).
-export([multiply/2]).

%% base case of transpose
transpose([[]|_]) ->
    [];
%% transpose main, concatenates each row recursively
transpose(B) ->
  [lists:map(fun(A) -> hd(A) end, B) | transpose(lists:map(fun(A) -> tl(A) end, B))].
 
 
multiply_elements(Pair, Sum) ->
    X = element(1, Pair),   % gets first element
    Y = element(2, Pair),   % gets second element
    X * Y + Sum.            % Sum is accumulator. So product is added to Sum, 
                            % and result is stored back in Sum (accumulator)

%% A and B are two single rows (one dimensional vectors).
%% foldl takes a function as first argument, with two parameters.
%% second argument is called accumulator, initialized to zero in this case
%% this accumulator is the second parameter to the first function.
%% the third argument is the list, whose elements become the first parameter 
%% of the first function. The first function is executed repeatedly over all 
%% elements of the list. The final value of the accumulator is returned as the
%% result of foldl
dot_product(A, B) ->
    lists:foldl(fun(X,Y)->multiply_elements(X,Y) end, 0, lists:zip(A, B)).
 
 
%% Exposed function. Expected result is C = A x B.
multiply(A, B) ->
    %% First transposes B. Indexing by row is easier, as we can zip rows and do cross product
    io:format("~w~n", [multiply_matrix_rows(A, transpose(B))]).


%% Performs multiplication on cross product of rows (i.e. tuples of rows from 2 matrices).
%% Expects second matrix to be transposed.
multiply_matrix_rows([Head | Rest], B) ->
    % multiply each row by B
    Element = multiply_row_by_col(Head, B),
 
    % concatenate the result of this multiplication with the next ones
    [Element | multiply_matrix_rows(Rest, B)];
 
multiply_matrix_rows([], _) ->
    % base case - concatenating an empty list to the end of the list
    [].
 

%% Takes out single rows from second matrix, and
%% Calls dot product to multiply two single rows.
multiply_row_by_col(Row, [Col_Head | Col_Rest]) ->
    Scalar = dot_product(Row, Col_Head),
 
    [Scalar | multiply_row_by_col(Row, Col_Rest)];
 
multiply_row_by_col(_, []) ->
    % base case - concatenating an empty list to the end of the list
    [].