# lambdaCalc

Code to evaluate lambda expressions.

# How to use

Load lambda.hs into GHCi

ghci lambda.hs

To define an expression use the lambda function

> lambda "\\x.x"

If you want to store the expression

> let e = lambda "(\\x.xx)y"

In the interpreter you must use two '\' since it is an escape character.

\ is used in the place of λ 

Variables are one character that aren't '(', ')', '\', '.'

To reduce a lambda expression use reduce.

> reduce $ lambda "(\\x.xx)y"

If you want to reduce the expression one reduction at a time (mostly works...) use reduceOne

> reduceOne $ lambda "(\\x.\\y.xy)z"

The string parser supports currying. It will allow multiiple parameters and will turn it into nested lambdas.

Reduce does not stop when passed a diverging expression.

> reduce $ lambda "(\\x.xx)(\\x.xx)"

will try to reduce forever and will need to be stopped.
