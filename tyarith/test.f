/* Examples for testing */

succ 0;
pred (succ (pred (pred 0)));
iszero (succ (pred (succ (succ 0))));
if true then (if true then false else (succ 0)) else (if true then 0 else false);
succ 2;
succ -3;
pred -1;
pred 2;
-2;
pair true false;
fst (pair true false);
snd (pair 0 (succ 0));
pair true false;
and zero one;
and one one;
