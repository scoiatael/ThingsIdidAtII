change(var(_), _).
change(op(X,Y), W):- change(X,W1), change(Y,W2), W = op(W1,W2).