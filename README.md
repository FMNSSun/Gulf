# Gulf

Gulf is yet another esoteric programming language research project of mine that makes use of
postfix, infix and prefix operators using a rather strange system. Infix expression have to be annotated
by a ```,``` and postfix expressions with a ```.``` and prefix is default. Therefore, instead of writing
```(a+b)+c``` we have to write ```,,a+b+c```. Instead of writing ```(5+3*3)*(-1)``` we have to write
```.,5+,3*3-``` (where postfix ```-``` is ```-1*abs(x)```). We could also use regular multiplication of course:
```,,5+,3*3*_1``` (```_``` is the minus sign).
