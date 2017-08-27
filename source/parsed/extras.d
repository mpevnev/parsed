/* Extras module

   Provides some nice-to-have, often useful things.

   It publicly imports 'core', so there's no need to import it if you use
   extras.

   */

module parsed.extras;

import std.range;
import std.traits;

public import parsed.core;

/* ---------- single-character parsers ---------- */

/* A char of whitespace. May optionally also match newlines.

  Note: on Windows machines EOL is two chars, so there'll be leftover chars
  after this parses away the first char in the pair. 
 */
auto
whitespace(B, C = char)(bool acceptNewline = false)
    if (isSomeChar!C)
{
    bool acceptable(C ch)
    {
        import std.uni;
        if (!ch.isWhite) return false;
        if (!acceptNewline && (ch == '\n' || ch == '\r')) return false;
        return true;
    }
    return singleChar!(B, C)(&acceptable);
}
unittest
{
    string str1 = "foo bar";
    string str2 = "foo\nbar";
    string str3 = "foobar";

    auto p1 = literal!string("foo")
        / whitespace!string;
    assert(p1.match(str1));
    assert(!p1.match(str2));
    assert(!p1.match(str3));

    auto p2 = literal!string("foo")
        / whitespace!string(true);
    assert(p2.match(str1));
    assert(p2.match(str2));
    assert(!p2.match(str3));
}

/* A char of anything but whitespace. */
auto
nonwhite(B, C = char)()
    if (isSomeChar!C)
{
    import std.uni;
    return singleChar!(B, C)(c => !c.isWhite);
}
unittest
{
    string str = "foo bar";
    auto state = ParserState!string(str);
    auto word = many(1, -1, nonwhite!string);

    auto res1 = word.run(state);
    assert(res1.success);
    assert(res1.parsed == "foo");

    auto res2 = word.run(res1);
    assert(!res2.success);
}

/* An alphanumeric char. */
auto
alnum(B, C = char)()
    if (isSomeChar!C)
{
    import std.uni;
    return singleChar!(B, C)(c => c.isAlphaNum);
}
unittest
{
    string str = "foo12";
    auto state = ParserState!string(str);

    auto p = many(1, -1, alnum!string);
    auto res = p.run(state);
    assert(res.success);
    assert(res.parsed == "foo12");
}

/* An alphabetic char. */
auto
alpha(B, C = char)()
    if (isSomeChar!C)
{
    import std.uni;
    return singleChar!(B, C)(c => c.isAlpha);
}
unittest
{
    string str = "foo12";
    auto state = ParserState!string(str);

    auto p = many(1, -1, alpha!string);
    auto res = p.run(state);
    assert(res.success);
    assert(res.parsed == "foo");
}

/* A decimal digit. */
auto
digit(B, C = char)()
    if (isSomeChar!C)
{
    import std.uni;
    return singleChar!(B, C)(c => c.isNumber);
}
unittest
{
    string str = "123f";
    auto state = ParserState!string(str);

    auto p = many(1, -1, digit!string);
    auto res = p.run(state);
    assert(res.success);
    assert(res.parsed == "123");
}

/* A hexadecimal digit. */
auto
hexdigit(B, C = char)()
    if (isSomeChar!C)
{
    import std.uni;
    return singleChar!(B, C)(c => c.isNumber 
            || ('a' <= c && c <= 'f')
            || ('A' <= c && c <= 'F'));

}

/* A newline. */
auto
newline(B, C = char)()
    if (isSomeChar!C)
{
    return singleChar!(B, C)(ch => ch == '\n' || ch == '\r');
}
unittest
{
    string str = "foo\nbar";

    auto p = literal!string("foo") 
        * newline!string 
        * literal!string("bar");
    assert(p.match(str));
}

/* ---------- multi-character combinations of the above ---------- */

/* Parses a whole line (with or without terminating newline). Note that
   'keepTerminator' option only affects '.parsed', the terminating newline is
   always removed from input. Always succeeds. */
auto
line(B, C = char)(bool keepTerminator)
    if (isSomeChar!C)
{
    import std.string;
    alias S = immutable(C)[];

    auto res = charUntil!(B, C)(ch => ch == '\n' || ch == '\r', true);
    if (keepTerminator)
        return res;
    else
        return res / morph!(B, S)(s => s.chomp);
}
unittest
{
    string str = "foo\nbar";
    auto state = ParserState!string(str);
    auto p = line!string(false);

    auto res1 = p.run(state);
    assert(res1.success);
    assert(res1.parsed == "foo");

    auto res2 = p.run(res1);
    assert(res2.success);
    assert(res2.parsed == "bar");
}

/* Parses several whitespace characters, but no less than one. */
auto
someWhite(B, C = char)(bool acceptNewlines = false)
    if (isSomeChar!C)
{
    return many(1, -1, whitespace!(B, C)(acceptNewlines));
}
unittest
{
    string str1 = "foo   bar";
    string str2 = "foo\tbar";
    string str3 = "foobar";
    string str4 = "foo \n\nbar";

    auto p1 = literal!int("foo") / someWhite!int(false) / literal!int("bar");
    auto p2 = literal!int("foo") / someWhite!int(true) / literal!int("bar");

    assert(p1.match(str1));
    assert(p1.match(str2));
    assert(!p1.match(str3));
    assert(!p1.match(str4));

    assert(p2.match(str1));
    assert(p2.match(str2));
    assert(!p2.match(str3));
    assert(p2.match(str4));
}

/* Parses zero or more whitespace characters. */
auto
maybeWhite(B, C = char)(bool acceptNewlines = false)
    if (isSomeChar!C)
{
    return many(0, -1, whitespace!(B, C)(acceptNewlines));
}
unittest
{
    string str1 = "foo   bar";
    string str2 = "foo\tbar";
    string str3 = "foobar";
    string str4 = "foo \n\nbar";

    auto p1 = literal!int("foo") / maybeWhite!int(false) / literal!int("bar");
    auto p2 = literal!int("foo") / maybeWhite!int(true) / literal!int("bar");

    assert(p1.match(str1));
    assert(p1.match(str2));
    assert(p1.match(str3));
    assert(!p1.match(str4));

    assert(p2.match(str1));
    assert(p2.match(str2));
    assert(p2.match(str3));
    assert(p2.match(str4));
}

/* Parses several newline characters, but no less than one. */
auto 
someNewlines(B, C = char)()
    if (isSomeChar!C)
{
    return many(1, -1, newline!(B, C));
}
unittest
{
    string str1 = "foo\n\rbar";
    auto p1 = literal!int("foo")
        / someNewlines!int
        / literal!int("bar");
    assert (p1.match(str1));
}

/* Parses zero or more newline characters. */
auto 
maybeNewlines(B, C = char)()
    if (isSomeChar!C)
{
    return many(0, -1, newline!(B, C));
}
unittest
{
    string str1 = "foo\n\rbar";
    auto p1 = literal!int("foo")
        / someNewlines!int
        / literal!int("bar");
    assert (p1.match(str1));
}

enum Word
{
    any,
    alnum,
    alpha
}
/* Parses either a sequence on non-whitespace characters (if given Word.any) or
   a sequence of alphanumeric characters (if given Word.alnum) or a sequence of
   alphabetic characters (if given Word.alpha) of length withing given bounds.
   Any of the bounds can be negative, which means no limit on the corresponding
   end.
   */
auto
word(B, C = char)(Word type, int minLength = 1, int maxLength = -1)
    if (isSomeChar!C)
{
    final switch (type) {
        case Word.any: 
            return many(minLength, maxLength, nonwhite!(B, C));
        case Word.alnum:
            return many(minLength, maxLength, alnum!(B, C));
        case Word.alpha:
            return many(minLength, maxLength, alpha!(B, C));
    }
}
unittest
{
    string str = "foo12( bar1 ";
    auto state = ParserState!string(str);

    auto p1 = word!string(Word.any);
    auto res1 = p1.run(state);
    assert(res1.success);
    assert(res1.parsed == "foo12(");

    auto p2 = word!string(Word.alnum);
    auto res2 = p2.run(state);
    assert(res2.success);
    assert(res2.parsed == "foo12");

    auto p3 = word!string(Word.alpha);
    auto res3 = p3.run(state);
    assert(res3.success);
    assert(res3.parsed == "foo");
}

/* Parses a number. */
auto
number(B, C = char)()
    if (isSomeChar!C)
{
    return many(1, -1, digit!(B, C));
}
unittest
{
    string str1 = "12 12";
    string str2 = "12f";
    string str3 = "foo";
    auto state1 = ParserState!string(str1);
    auto state2 = ParserState!string(str2);

    auto p = number!string;
    auto res1 = p.run(state1);
    assert(res1.success);
    assert(res1.parsed == "12");

    auto res2 = p.run(state2);
    assert(res2.success);
    assert(res2.parsed == "12");

    assert(!p.match(str3));
}

/* Parses a hexadecimal number. The number may or may not be prefixed by '0x'.
   The prefix will *not* appear in '.parsed'. 
 */
auto
hexnum(B, C = char)()
    if (isSomeChar!C)
{
    alias S = immutable(C)[];
    return maybe(literal!S("0x")) / many(1, -1, hexdigit!C);
}

/* ---------- misc ---------- */

/* Parses something one or zero times. */
auto
maybe(B, S = string)(Parser!(B, S) p)
    if (isSomeString!S)
{
    return many(0, 1, p);
}
unittest
{
    string str1 = "foo bar";
    string str2 = "foobar";
    string str3 = "foo!bar";

    auto p = literal!string("foo")
        / maybe(whitespace!string)
        / literal!string("bar");

    assert(p.match(str1));
    assert(p.match(str2));
    assert(!p.match(str3));
}

/* Parses text between balanced pair of given symbols. */
auto
balanced(B, C = char)(C left, C right, bool keepPair = false)
    if (isSomeChar!C)
{
    alias S = immutable(C)[];
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            /* There must be space for a pair. */
            if (toParse.left.length < 2) return toParse.fail;
            if (toParse.left[0] != left) return toParse.fail;

            int level = 1;
            size_t parsed = 1;
            size_t len = toParse.left.length;
            while (level != 0 && parsed < len) {
                C ch = toParse.left[parsed];
                /* Note that the order here is very important. It allows to
                   use the same character for left and right.
                   */
                if (ch == right) {
                    level--;
                    parsed++;
                    continue;
                }
                if (ch == left) {
                    level++;
                    parsed++;
                    continue;
                }
                parsed++;
            }

            if (level == 0) {
                auto res = toParse;
                if (keepPair)
                    res.parsed = toParse.left[0 .. parsed];
                else
                    res.parsed = toParse.left[1 .. parsed - 1]; 
                if (parsed < len)
                    res.left = res.left[parsed .. $];
                else
                    res.left = [];
                return res.succeed;
            } else {
                return toParse.fail;
            }
        } /* run */
    } /* Res */
    return new Res();
}
unittest
{
    string str1 = "(abcdef)";
    string str2 = "(ab(df))";
    string str3 = "((asdf)d)";
    string str4 = "(asdf)f";
    string str5 = "(asdff";
    string str6 = "/asdf/";
    auto state1 = ParserState!int(str1);
    auto state2 = ParserState!int(str2);
    auto state3 = ParserState!int(str3);
    auto state4 = ParserState!int(str4);
    auto state5 = ParserState!int(str5);
    auto state6 = ParserState!int(str6);
    auto p = balanced!int('(', ')');
    auto p2 = balanced!int('/', '/');

    auto res1 = p.run(state1);
    assert(res1.success);
    assert(res1.parsed == "abcdef");

    auto res2 = p.run(state2);
    assert(res2.success);
    assert(res2.parsed == "ab(df)");

    auto res3 = p.run(state3);
    assert(res3.success);
    assert(res3.parsed == "(asdf)d");

    auto res4 = p.run(state4);
    assert(res4.success);
    assert(res4.parsed == "asdf");

    auto res5 = p.run(state5);
    assert(!res5.success);

    auto res6 = p2.run(state6);
    assert(res6.success);
    assert(res6.parsed == "asdf");
}

/* Parses text between balanced pair of bits that match given parsers. 'left'
   and 'right' parsers are going to be run many times, so be careful with
   building inside them. */
auto
balanced(B, S = string)(Parser!(B, S) left, Parser!(B, S) right, bool keepPair = false)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;

            auto cur = toParse;
            cur = left.run(cur);
            if (!cur.success) return toParse.fail;

            import std.stdio;

            int level = 1;
            size_t start = cur.parsed.length;
            size_t parsed = start;
            size_t lastRightLen;
            size_t len = toParse.left.length;
            while (level != 0 && parsed < len) {
                /* Note the order. It allows using same parsers for left and
                   right.
                   */
                auto maybeRight = right.run(cur);
                if (maybeRight.success) {
                    level--;
                    size_t rightLen = maybeRight.parsed.length;
                    parsed += rightLen;
                    lastRightLen = rightLen;
                    cur = maybeRight;
                    continue;
                }
                auto maybeLeft = left.run(cur);
                if (maybeLeft.success) {
                    level++;
                    parsed += maybeLeft.parsed.length;
                    cur = maybeLeft;
                    continue;
                }
                parsed++;
                cur.left = cur.left[1 .. $];
            } /* while level != 0 */
            if (level != 0) return toParse.fail;
            auto res = toParse;
            res.left = cur.left;
            if (keepPair)
                res.parsed = toParse.left[0 .. parsed];
            else
                res.parsed = toParse.left[start .. parsed - lastRightLen];
            return res.succeed;
        } /* run */
    } /* Res */
    return new Res();
}
unittest
{
    string str1 = "foo 1 2 3 bar";
    string str2 = "foo 1 2 3";
    string str3 = "foo 1 2 3 foo";
    auto s1 = ParserState!int(str1);
    auto s3 = ParserState!int(str3);
    auto p = balanced!int(literal!int("foo"), literal!int("bar"), false);
    auto p2 = balanced!int(literal!int("foo"), literal!int("foo"), false);

    auto res1 = p.run(s1);
    assert(res1.success);
    assert(res1.parsed == " 1 2 3 ");

    assert(!p.match(str2));

    auto res3 = p2.run(s3);
    assert(res3.success);
    assert(res3.parsed == " 1 2 3 ");
}

/* Parses text until a given parser succeeds. The part that matches the given
   parser is removed from the input (but can optionally be left there). Fails
   if nothing matches the parser.
 */
auto
upTo(B, S = string)(
        Parser!(B, S) parser, 
        bool keepTerminator = false,
        bool consumeTerminator = true)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            auto cur = toParse;
            size_t parsed = 0;
            while (cur.left.length > 0) {
                auto maybeDone = parser.run(cur);
                if (maybeDone.success) {
                    size_t finish = parsed;
                    if (keepTerminator) parsed += maybeDone.parsed.length;
                    if (consumeTerminator) finish = parsed;
                    return maybeDone.succeed(toParse.left[0 .. finish]);
                }
                parsed++;
                cur.left = cur.left[1 .. $];
            }
            return toParse.fail;
        }
    }
    return new Res();
}
unittest
{
    string str1 = "foo bar! baz";
    string str2 = "foo bar";
    auto s1 = ParserState!int(str1);
    auto s2 = ParserState!int(str2);
    auto p = upTo(literal!int("!"));

    auto res1 = p.run(s1);
    assert(res1.success);
    assert(res1.parsed == "foo bar");
    assert(res1.left == " baz");

    auto res2 = p.run(s2);
    assert(!res2.success);
}

/* Behaves just like the 'literal' parser from core, but takes a range of 
   strings as patterns. */
auto
multiliteral(B, S = string, R)(R range, bool consumeInput = true,  bool caseSensitive = true)
    if (isInputRange!R && is(ElementType!R: S))
{
    import std.algorithm;
    import std.string;

    auto literals = range.map!(x => caseSensitive ? x : x.toLower);
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            foreach (literal; literals) {
                S checkAgainst = caseSensitive ? toParse.left : toParse.left.toLower;
                if (checkAgainst.startsWith(literal)) {
                    if (consumeInput)
                        toParse.left = toParse.left[literal.length .. $];
                    return toParse.succeed(literal);
                }
            }
            return toParse.fail;
        }
    }
    return new Res();
}
unittest
{
    import std.array; /* This is required to turn arrays into ranges. */

    string str1 = "foo";
    string str2 = "bar";
    string str3 = "baz";

    auto s1 = ParserState!int(str1);
    auto s2 = ParserState!int(str2);
    auto s3 = ParserState!int(str3);
    
    auto p1 = multiliteral!int(["foo", "bar"]);

    auto res1_1 = p1.run(s1);
    auto res1_2 = p1.run(s2);
    auto res1_3 = p1.run(s3);

    assert(res1_1.success);
    assert(res1_1.parsed == "foo");

    assert(res1_2.success);
    assert(res1_2.parsed == "bar");

    assert(!res1_3.success);
}
