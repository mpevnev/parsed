/* Core module

   Provides base Parser interface and some basic parser generators.

   If you need to write a parser yourself, be sure to make it short-circuiting
   - include 'if (!toParse.success) return toParse.fail;' as the first line of
   its 'run' method.

   I also recommend making an alias with type of your built-up value and using
   that instead of having the type everywhere. This way changing it later to
   something more appropriate will be a lot easier. Also, less typing if you
   choose a short alias.

   */

module parsed.core;

import std.traits;

/* ---------- base ---------- */

struct ParserState(B, S = string) /* B(uild) and S(tring). */
    if (isSomeString!S)
{
    private alias ThisState = ParserState!(B, S);

    S left;
    S parsed;
    B value; 
    bool success = true;
    bool empty = false;

    this(B init, S toParse)
    {
        value = init;
        left = toParse;
    }

    this(S toParse)
    {
        value = B.init;
        left = toParse;
    }

    ThisState fail()
    {
        auto res = this;
        res.success = false;
        res.value = B.init;
        return res;
    }

    ThisState succeed()
    {
        auto res = this;
        res.success = true;
        res.empty = false;
        return res;
    }

    ThisState succeed(S withParsed)
    {
        auto res = this;
        res.success = true;
        res.empty = false;
        res.parsed = withParsed;
        return res;
    }

    ThisState build(B delegate (B, S) dg)
    {
        auto res = this;
        if (success) {
            res.value = dg(value, parsed);
            return res.succeed;
        } else {
            return res.fail;
        }
    }

    ThisState absorb(B2)(
            ParserState!(B2, S) other,
            B delegate (B, B2, S) dg)
    {
        auto res = this;
        if (success) {
            res.value = dg(value, other.value, other.parsed);
            res.left = other.left;
            res.parsed = other.parsed;
            return res.succeed;
        } else {
            return res.fail;
        }
    }

    ThisState pass()
    {
        auto res = this;
        res.success = true;
        res.empty = true;
        return res;
    }
}

/* B(uild) and S(tring). */
interface Parser(B, S = string) 
    if (isSomeString!S)
{
    private:
        alias State = ParserState!(B, S);
        alias ThisParser = Parser!(B, S);
    
    public:

    /* ---------- high-level operations ---------- */

    /* Main method. */
    State run(State toParse);

    /* Runs the parser on the given text. */
    final State run(S text)
    {
        return run(State(B.init, text));
    }

    /* Returns true if the parser succeeds on the text. */
    final bool match(S text)
    {
        auto res = run(text);
        return res.success;
    }

    /* ---------- parser combinations ---------- */

    /* Builds up a value. */
    final ThisParser build(B delegate (B, S) dg)
    {
        class Res: ThisParser
        {
            State run(State toParse)
            {
                auto res = this.outer.run(toParse);
                if (res.success) {
                    res.value = dg(res.value, res.parsed);
                    return res.succeed;
                } else {
                    return toParse.fail; 
                }
            }
        }
        return new Res();
    }

    /* Feeds this parser's state to another parser. Succeeds if both parsers
       do.
       */
    final ThisParser chain(ThisParser other, bool concat)
    {
        class Res: ThisParser
        {
            State run(State toParse)
            {
                if (!toParse.success) return toParse.fail;
                auto res1 = this.outer.run(toParse);
                if (res1.success) {
                    auto res2 = other.run(res1);
                    if (res2.success) {
                        if (concat) res2.parsed = res1.parsed ~ res2.parsed;
                        return res2.succeed;
                    }
                }
                return toParse.fail;
            }
        }
        return new Res();
    }

    /* Returns state of the first parser of the two to succeed. */
    final ThisParser any(ThisParser other)
    {
        class Res: ThisParser
        {
            State run(State toParse)
            {
                if (!toParse.success) return toParse.fail;
                auto res = this.outer.run(toParse);
                if (res.success) return res;
                return other.run(toParse);
            }
        }
        return new Res();
    }

    /* Make a new parser that passes previous parser's parsed text
       transparently. */
    final ThisParser discard()
    {
        class Res: Parser
        {
            State run(State toParse)
            {
                if (!toParse.success) return toParse.fail;
                State res = this.outer.run(toParse);
                if (res.success)
                    return res.pass;
                else
                    return res;
            }
        }
        return new Res();
    }

    /* ---------- operator overloads ---------- */ 

    /* Infix analog of 'chain' without parsed string concatenation. Think of 
       '/' as a wall where flow stops. */
    final ThisParser opBinary(string op)(ThisParser other)
        if (op == "/")
    {
        return chain(other, false);
    }

    /* Infix analog of 'chain' with parsed string concatenation. Think of '*'
       as of a piece of chain. */
    final ThisParser opBinary(string op)(ThisParser other)
        if (op == "*")
    {
        return chain(other, true);
    }

    /* Infix analog of 'build'. I've got no clever analogy as to why it's '%'.
       The real reason is that '%' is in the same precedence group as '*' and
       '/'. */
    final ThisParser opBinary(string op)(B delegate (B, S) dg)
        if (op == "%")
    {
        return build(dg);
    }

    /* Infix analog of 'any' */
    final ThisParser opBinary(string op)(ThisParser other)
        if (op == "|")
    {
        return any(other);
    }
}
unittest
{
    import std.conv;

    string str1 = "foo bar baz";

    auto p1 = literal!int("foo");
    assert(p1.match(str1));

    auto p2 = p1 * literal!int(" ");
    assert(p2.match(str1));

    auto p3 = p1 * literal!int("BAR");
    assert(!p3.match(str1));

    string str2 = "1 2 3";
    auto state = ParserState!int(0, str2);

    auto p = (literal!int("1") | literal!int("2") | literal!int("3"))
        % (int i, string s) => i + to!int(s);
    auto space = literal!int(" ");
    auto sum = p * space * p * space * p;
    auto res = sum.run(state);
    assert(res.success);
    assert(res.value == 6);
}

/* ---------- fundamental parsers ---------- */

/* Parses a literal string (case-sensitive by default). */
auto
literal(B, S = string)(S str, bool consumeInput = true, bool caseSensitive = true) 
{
    import std.string;

    S use = str;
    if (!caseSensitive) use = use.toLower;
    class Res: Parser!(B, S)
    {
        ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            S checkAgainst = toParse.left;
            if (!caseSensitive) checkAgainst = checkAgainst.toLower;
            if (checkAgainst.startsWith(use)) {
                if (consumeInput)
                    toParse.left = toParse.left[use.length .. $];
                return toParse.succeed(str);
            }
            return toParse.fail;
        }
    }
    return new Res();
}
unittest 
{
    string str = "Hello world";

    auto p1 = literal!int("Hello");
    assert(p1.match(str));

    auto p2 = literal!int("hello", true, false);
    assert(p2.match(str));

    assert(!(p1 * p2).match(str));

    auto p3 = literal!int("Hello")
        * literal!int(" ")
        * literal!int("world");
    assert(p3.match(str));
}

/* Always fails. Useful to terminate 'many'. */
auto
fail(B, S = string)()
{
    class Res: Parser!(B, S)
    {
        ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            return toParse.fail;
        }
    }
    return new Res();
}
unittest
{
    string str = "foo";
    auto p = fail!string;

    assert(!p.match(str));
}

/* Fails if given condition returns false, succeeds consuming no input otherwise. */
auto
test(B, S = string)(bool delegate (B, S) tst)
{
    class Res: Parser!(B, S)
    {
        ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            if (tst(toParse.value, toParse.parsed))
                return toParse.succeed;
            else
                return toParse.fail;
        }
    }
    return new Res();
}
unittest
{
    import std.conv;

    string str = "12";
    auto state = ParserState!int(str);

    auto p = literal!int("12")
        % ((res, i) => to!int(i))
        / test!int((res, s) => res > 5);
    auto res = p.run(state);
    assert(res.success);
}

/* Uses the same parser between 'min' and 'max' times. If either of 'min' and
   'max' is negative, there's no limit on corresponding allowed amount of
   times. Value is passed from each run to the next one, with resulting 
   parser inheriting the value from the last run. 
   */
auto 
many(B, S = string)(int min, int max, Parser!(B, S) p)
{
    class Res: Parser!(B, S)
    {
        ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            import std.stdio;

            if (!toParse.success) return toParse.fail;
            S parsed;
            ParserState!(B, S) cur = toParse.succeed;

            /* Check required minimum of successful parses. */
            size_t n = 0;
            if (min > 0) {
                while (n < min) {
                    cur = p.run(cur);
                    if (!cur.success) return toParse.fail;
                    n++;
                    parsed ~= cur.parsed;
                }
            }

            /* Parse the rest. */
            B value = cur.value;
            while ((max < 0 || n < max) && cur.success) {
                cur = p.run(cur);
                if (!cur.success) 
                    break;
                else
                    value = cur.value;
                n++;
                parsed ~= cur.parsed;
            }
            cur.value = value;
            return cur.succeed(parsed);
        }
    }
    return new Res();
}
unittest
{
    import std.conv; 

    string str1 = "123 12 13";
    string str2 = "1 2 3";
    string str3 = "foo bar";

    auto digit = literal!int("1") | literal!int("2") | literal!int("3");
    auto a = many(1, -1, digit) % (i, s) => to!int(s);
    auto s1 = ParserState!int(str1);
    s1 = a.run(s1);
    assert(s1.success);
    assert(s1.value == 123);

    auto s2 = ParserState!int(str2);
    s2 = a.run(s2);
    assert(s2.success);
    assert(s2.value == 1);

    assert(!a.match(str3));
}

/* Uses a subparser and absorbs its built value into main chain's one by
   passing it through a given delegate. 
   */
auto
absorb(B, B2, S = string)(B delegate (B, B2, S) dg, Parser!(B2, S) subparser)
{
    class Res: Parser!(B, S)
    {
        ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            auto sendToSub = ParserState!(B2, S)(toParse.left);
            auto returned = subparser.run(sendToSub);
            if (returned.success) {
                return toParse.absorb(returned, dg);
            } else {
                return toParse.fail;
            }
        }
    }
    return new Res();
}
unittest
{
    import std.conv;

    string str = "12 25";

    auto digit = literal!int("1") | literal!int("2") | literal!int("5");
    auto someint = many(1, -1, digit) % (int i, string s) => to!int(s);

    auto list = 
        absorb!(int[], int)((l, i, s) => [i], someint)
        / literal!(int[])(" ")
        / absorb!(int[], int)((l, i, s) => l ~ i, someint);
    auto res = list.run(str);
    assert(res.success);
    assert(res.value == [12, 25]); 
}

/* Run previous parser's parsed string through a function and substitute the
   result for parsed string.
   */
auto
morph(B, S = string)(S delegate (S) dg)
{
    class Res: Parser!(B, S)
    {
        ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            return toParse.succeed(dg(toParse.parsed));
        }
    }
    return new Res();
}
unittest
{
    import std.string;

    string str = "FOO bar";
    auto state = ParserState!string(str);
    auto p = literal!string("FOO")
        / morph!string(s => s.toLower);
    auto res = p.run(state);
    assert(res.success);
    assert(res.parsed == "foo");
}

/* Parses a single character if it passes a given test. */
auto
singleChar(B, C = char)(bool delegate (C) test)
    if (isSomeChar!C)
{
    alias S = immutable(C)[];
    class Res: Parser!(B, S)
    {
        ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            if (toParse.left.length == 0) return toParse.fail;
            auto res = toParse;
            C ch = toParse.left[0];
            S s = [ch];
            if (test(ch)) {
                res.left = res.left[1 .. $];
                return res.succeed(s);
            } else {
                return toParse.fail;
            }
        }
    }
    return new Res();
}
unittest
{
    import std.conv;

    string str = "123 12";
    auto state = ParserState!int(str);

    auto digit = singleChar!int(c => '0' <= c && c <= '9');
    auto num = many(1, -1, digit) % (i, s) => to!int(s);
    auto res = num.run(state);
    assert(res.success);
    assert(res.value == 123);
}

/* ---------- conditional parsers ---------- */

/* Be extra careful with the following parsers: they always succeed and are not
   guaranteed to consume any input. This can lead to infinite loops. 
   */

/* Parses characters while a condition is met. */
auto
parseWhile(B, C = char)(bool delegate (C) test, bool keepTerminator = true)
    if (isSomeChar!C)
{
    alias S = immutable(C)[];
    class Res: Parser!(B, S)
    {
        ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            auto res = toParse;
            size_t i = 0;
            size_t len = toParse.left.length;
            while (i < len && test(res.left[i])) i++;
            if (keepTerminator && i < len) i++;
            res.parsed = res.left[0 .. i];
            res.left = res.left[i .. $];
            return res.succeed;
        }
    }
    return new Res();
}
unittest
{
    string str1 = "foo bar";
    auto state1 = ParserState!string(str1);

    auto word = parseWhile!string(c => c != ' ', false);
    auto res1 = word.run(state1);
    assert(res1.parsed == "foo");
    assert(res1.left == " bar");
    /* Notice that the following does succeed, but with empty string parsed. */
    auto res2 = word.run(res1);
    assert(res2.parsed == ""); 
}

/* Parses characters until a condition is met. */
auto
parseUntil(B, C = char)(bool delegate (C) test, bool keepTerminator = true)
    if (isSomeChar!C)
{
    return parseWhile!(B, C)(c => !test(c), keepTerminator);
}
