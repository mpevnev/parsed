# parsed

'parsed' is a text parsing library for D programming language. It allows to
combine basic parsers in various ways, creating thus more complex and elaborate
ones.

'parsed' consists of two modules: `parsed.core` and `parsed.extras`. First one
contains interfaces, structures and basic parsers used throughout 'parsed', 
while the second one contains parsers that are more likely to crop up in real
applications. `extras` imports `core` publicly, so you don't have to import 
`core` if you use `extras`. `extras` is probably the module you want to use, 
unless you enjoy writing most parsers from scratch.

# Basics

All parsers implement Parser interface, which is parametric in two things:
the type of a value the parser builds up as it works, and type of the strings
the parser can work with. String type defaults to `string`, value type has to
be declared explicitly. Most parser-producing functions are parametric in the
same way, except for those that take character type instead of string type.

To perform a parse, you need a parser and an initial state to feed to that
parser.  Parser state is represented by a ParserState struct, which is
parametric in the same way as parsers themselves. Constructing a ParserState
looks like this: `auto state = ParserState!SomeType(someString)`. To perform a
parse now, you need to `auto res = someParser.run(state)`. Then you'll want to
test if the parser succeeded (which is done via `.success` property of
ParserState) and to get the value (of type `SomeType`) built by the parser,
which is done via `.value` property of ParserState. To build a value, you'll
have to use `%` operator described below. Parsed string can be accesses via
`.parsed` property of ParserState.

Four operators provide a way to build complex parsers from simpler ones.
1. `*` is applied to two parsers. It runs both of them and concatenates
together the strings they parsed (if they've succeeded, of course). So if you
do 
`auto p = literal!SomeType("foo") * literal!SomeType("bar");`, then `p` will
succeed on any string starting with "foobar", and `.parsed` will also be 
"foobar".
2. `/` is also applied to two parsers. It runs both of them and discards first
one's parsed string, resulting in only second parser's parsed string being
passed down the parser chain. So if you do
`auto p = literal!SomeType("foo") / literal!SomeType("bar");`, then `p` will
succeed on any string starting with "foobar", but `.parsed` will be just "bar".
3. `%` is the most importnant operator. It is applied to a parser on the left
side and a function that takes chain's built-up value and this parser's parsed
string. The function should return new build-up value, which will replace that
of the whole parser chain. So if you do
`auto p = literal!int("12") % (res, i) => to!int(i);` then on success `.value`
of resulting ParserState will be 12.
4. `|` is applied to two parsers. It runs the first one, and if it fails, it 
runs the other one. So
`auto p = literal!int("foo") | literal!int("bar");` will match both strings
starting with "foo" and strings starting with "bar".

For more complex examples, see 'examples' folder. Unittests in the source files
also provide several, but they are not very elaborate.

# Core parsers

These are the parsers that are defined in `parsed.core`.

## literal

`auto literal(B, S = string)(S str, bool consumeInput = true, bool caseSensitive = true)`.

Matches a literal string. Optionally may not consume input and disregard case.
`.parsed` is the matched string.

## fail

`auto fail(B, S = string)()`.

Always fails.

## succeed

`auto succeed(B, S = string)()`.

Always succeeds.

## test

`auto test(B, S = string)(bool delegate (B, S) test)`.

Runs built-up value and previous parser's `.parsed` through the given 
function. Succeeds with `.parsed` inherited from the previous parsed if the
function returns `true`, fails otherwise.

## build

`auto build(B, S = string)(B delegate (B, S) dg)`.

Builds a value just like `%` operator does. However, `build` can be the first
element in a parser chain, unlike `%`, which requires a parser to be in the
chain before it.

## many

`auto many(B, S = string)(int min, int max, Parser!(B, S) p)`.

Uses the given parser between `min` and `max` times (if `min` is negative,
there's no lower limit, if `max` is negative, there's no upper limit). Succeeds
if required minimum of successful applications of `p` is achieved. `.parsed` is
concatenated from each run's `.parsed`s.

## absorb

`auto absorb(B, B2, S = string)(B delegate (B, B2, S) dg, Parser!(B2, S) subparser)`.

This parser's job is to make parsing nested structures easier. It allows you to
run a subparser on the current input and then appropriate its resulting
`.value` into the main parser chain's `.value`. The delegate this function
expects as the first argument should perform said appropriation, returning new
`.value` for the main chain. "struct" example makes use of this function.

## morph

`auto morph(B, S = string)(S delegate (S) dg)`.

Takes previous parser's `.parsed` and transforms it using the given delegate.
`.parsed` is the delegate's return value.

## singleChar

`auto singleChar(B, C = char)(bool delegate (C) test)`.

Parses a single character. Succeeds if the character passes the given test,
fail on empty string or if the test is not passed.

## charWhile

`auto charWhile(B, C = char)(bool delegate (C) test, bool keepTerminator = true)`.

Parses input one character at a time while a given condition is met. `.return.`
is concatenation of all parsed characters, optionally without the character
that failed the test. Note that `keepTerminator` also affects whether or not
the terminating character remains in the input after the parser is done.
Always succeeds.

## charUntil

`auto charUntil(B, C = char)(bool delegate (C) test, bool keepTerminator = true)`.

Same as `charWhile`, but parses until a condition is met. Always succeeds.

## repeatWhile

`auto repeatWhile(B, S = string)(bool delegate (B, S, int) test, Parser!(B, S) p)`.

Runs the same parser for as long as a condition is met. The condition takes 
three arguments: built-up value, `.parsed` of the current run, and iteration's
index (0-based). `.parsed` is concatenation of individual runs' `.parsed`s.
Always succeeds.

## repeatUntil

`auto repeatUntil(B, S = string)(bool delegate(B, S, int) test, Parser!(B, S) p)`.

Same as `repeatWhile`, but parses until a condition is met. Always succeeds.

# Extra parsers

These parsers are defined in `parsed.extras`.

## whitespace

`auto whitespace(B, C = char)(bool acceptNewline = false)`.

Parses a single character of whitespace (as defined by `uni.isWhite`). By
default fails on newline characters.

## nonwhite 

`auto nonwhite(B, C = char)()`.

Parses a single character of anything but whitespace.

## alnum

`auto alnum(B, C = char)()`.

Parses a single alphanumeric character (as defined by `uni.isAlphaNum`).

## alpha

`auto alpha(B, C = char)()`.

Parses a single alphabetic character (as defined by `uni.isAlpha`).

## digit

`auto digit(B, C = char)()`.

Parses a single decimal digit.

## hexdigit

`auto hexdigit(B, C = char)()`.

Parses a single hexadecimal digit (both lower- and upper-case).

## newline

`auto newline(B, C = char)()`.

Parses a single '\n' or '\r'.

## line

`auto line(B, C = char)(bool keepTerminator)`.

Parses a whole line. Note that 'keepTerminator' parameter only affects whether
or not newline is included in the `.parsed`, it's always removed from the
input.  Always succeeds.

## someWhite

`auto someWhite(B, C = char)(bool acceptNewlines)`.

Parses as many whitespace characters as it can, but no less than one.
Optionally parses newlines as well.

## maybeWhite

`auto maybeWhite(B, C = char)(bool acceptNewlines)`.

Parses as many whitespace characters as it can, maybe even 0. Optionally parses
newlines as well.

## someNewlines

`auto someNewlines(B, C = char)()`.

Parses as many newline characters as it can, but no less than one.

## word

`auto word(B, C = char)(Word type, int minLength = 1, int maxLength = -1)`.

Parses a word. The definition of a word is determined by the `type` parameter.
If `type` is `Word.any` then a word is a sequence of non-whitespace characters.
If `type` is `Word.alnum` then a word is a sequence of alphanumeric characters.
If `type` is `Word.alpha` then a word is a sequence of alphabetic characters.
The sense of `minLength` and `maxLength` is the same as for `many`. Note that
if `maxLength` cuts a word in two, this parser will still succeed with the
first bit as `.parsed`.

## number

`auto number(B, C = char)()`.

Parses a decimal integral number (no floats).

## hexnum

`auto hexnum(B, C = char)()`.

Parser a hexadecimal number (both upper- and lower-case). A number may begin
with "0x", but it's not included in the `.parsed`.

## maybe

`auto maybe(B, S = string)(Parser!(B, S) p)`.

Uses the given parser zero or one time. This is convenience wrapper over
`many(0, 1, parser)`.

## balanced (1st overload)

`auto balanced(B, C = char)(C left, C right, bool keepPair = false)`.

Parses text between a balanced pair of symbols. May either include or not the
starting and terminating `left` and `right` respectively. Note that `keepPair`
has effect only on `.parsed`, not on the state of the input, the pair is 
always removed from it.

## balanced (2nd overload)

`auto balanced(B, S = string)(Parser!(B, S) left, Parser!(B, S) right, bool keepPair = false)`.

Acts just like the first overload, except it uses parsers to determine which
bits of text will serve as left and right components of a pair. Note that it
will call both parsers on _every_ character in between left and right, so it's
going to be extremely slow for longer strings.
