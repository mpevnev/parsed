# parsed

'parsed' is a text parsing library for D programming language. It allows to
combine basic parsers in various ways, creating thus more complex and elaborate
ones.

Parsers carry two pieces of information: a built-up value that is common to the 
whole parser chain and that will be ultimately used as the chain's output, and
intermediate parsed string. Both can be manipulated.

At 'parsed's core lie three operators: 
1. `*` is applied to two parsers. It runs both of them and concatenates
together the strings they parsed (if they've succeeded, of course).
2. `/` is also applied to two parsers. It runs both of them and discards first
one's parsed string, resulting in only second parser's parsed string being
passed down the parser chain.
3. `%` is the most importnant one. It applied to a parser on the left side and 
a function that takes chain's built-up value and this parser's parsed string.
The function should return new build-up value, which will replace that of the
whole parser chain.

For examples, see 'examples' folder and unittests in the source files.
