/* Core module

   Provides base Parser interface and some basic parser generators.

   If you need to write a parser yourself, be sure to make it short-circuiting
   - include 'if (!toParse.success) return toParse.fail;' as the first line of
   its 'run' method (unless it's oblivious, see below).

   Parsers can be made 'oblivious' by setting corresponding field to true. An
   oblivious parser doesn't care if the previous chain has failed, it'll try to
   parse anyway. Oblivious parsers can fail or succeed as usual. This provides
   an ability to: one, recover from parser errors (kind of like | operator
   does), and, two, perform some operations if the chain has failed. For an
   example of the latter see the 'throwOnFailure' parser.

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
        res.parsed = "";
        return res;
    }

    ThisState succeed()
    {
        auto res = this;
        res.success = true;
        return res;
    }

    ThisState succeed(S withParsed)
    {
        auto res = this;
        res.success = true;
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
        res.parsed = null;
        return res;
    }
}

/* B(uild) and S(tring). */
class Parser(B, S = string) 
    if (isSomeString!S)
{
    private:
        alias State = ParserState!(B, S);
        alias ThisParser = Parser!(B, S);

    /* This should be true if the parser is able to operate even if the chain
       is in the failed state. */
    protected bool oblivious = false;

    public:

    /* ---------- high-level operations ---------- */

    /* Main method. */
    abstract State run(State toParse);

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
            override State run(State toParse)
            {
                auto outer = this.outer;
                if (outer.oblivious || toParse.success) {
                    auto res = outer.run(toParse);
                    if (res.success) {
                        res.value = dg(res.value, res.parsed);
                        return res.succeed;
                    } else {
                        return res.fail;
                    }
                } else {
                    return toParse.fail;
                } /* if oblivious */
            } /* run */
        } /* Res */
        return new Res();
    } /* build */

    /* Feeds this parser's state to another parser. Succeeds if both parsers do. */
    final ThisParser chain(ThisParser other, bool concat)
    {
        class Res: ThisParser
        {
            this() 
            { 
                oblivious = this.outer.oblivious || other.oblivious; 
            }

            override State run(State toParse)
            {
                auto outer = this.outer;
                State res1, res2;
                import std.stdio;
                if (outer.oblivious && other.oblivious) {
                    /* Run both. They don't care if the chain or each other
                       have failed. */
                    res1 = outer.run(toParse);
                    res2 = other.run(res1);
                } else if (outer.oblivious && !other.oblivious) {
                    /* Run first even if the chain has failed. */
                    res1 = outer.run(toParse);
                    if (!res1.success) return toParse.fail;
                    res2 = other.run(res1);
                } else if (!outer.oblivious && other.oblivious) {
                    /* Run the second even if the first has failed. */
                    if (toParse.success) {
                        res1 = outer.run(toParse);
                        res2 = other.run(res1);
                    } else {
                        return toParse.fail;
                    }
                } else {
                    /* Normal case, both must succeed. */
                    if (toParse.success) {
                        res1 = outer.run(toParse);
                        if (!res1.success) return toParse.fail;
                        res2 = other.run(res1);
                        if (!res2.success) return toParse.fail;
                    } else {
                        return toParse.fail;
                    }
                } /* if combination of obliviousness */
                if (res2.success) {
                    if (concat) 
                        res2.parsed = res1.parsed ~ res2.parsed;
                    return res2.succeed;
                } else  {
                    return toParse.fail;
                }
            } /* run */
        } /* Res */
        return new Res();
    } /* chain */

    /* Returns state of the first parser of the two to succeed. */
    final ThisParser any(ThisParser other)
    {
        class Res: ThisParser
        {
            override State run(State toParse)
            {
                auto outer = this.outer;
                if (toParse.success) {
                    auto res1 = outer.run(toParse);
                    if (res1.success) return res1.succeed;
                    auto res2 = other.run(toParse);
                    if (res2.success) return res2.succeed;
                    return toParse.fail;
                } else {
                    if (outer.oblivious) {
                        auto res1 = outer.run(toParse);
                        if (res1.success) return res1.succeed;
                    }
                    if (other.oblivious) {
                        auto res2 = outer.run(toParse);
                        if (res2.success) return res2.succeed;
                    }
                    return toParse.fail;
                } /* if toParse.success */
            } /* run */
        } /* Res */
        return new Res();
    } /* any */

    /* Make a new parser that discards original parser's 'parsed' and sets it
       to an empty string */
    final ThisParser discard()
    {
        class Res: ThisParser
        {
            override State run(State toParse)
            {
                auto outer = this.outer;
                if (toParse.success || outer.oblivious) {
                    auto res = outer.run(toParse);
                    if (res.success) {
                        res.parsed = "";
                        return res.succeed;
                    } else {
                        return toParse.fail;
                    }
                } else {
                    return toParse.fail;
                }
            } /* run */
        } /* Res */
        return new Res();
    } /* discard */

    final ThisParser makeOblivious()
    {
        class Res: ThisParser
        {
            this() { oblivious = true; }
            override State run(State toParse)
            {
                return this.outer.run(toParse);
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

    /*
    string str1 = "foo bar baz";

    auto p1 = literal!int("foo");
    assert(p1.match(str1));

    auto p2 = p1 * literal!int(" ");
    assert(p2.match(str1));

    auto p3 = p1 * literal!int("BAR");
    assert(!p3.match(str1));
    */

    string str2 = "1 2 3";
    auto state = ParserState!int(0, str2);

    import std.stdio;
    auto p = (literal!int("1") | literal!int("2") | literal!int("3"))
        % (int i, string s) => i + to!int(s);
    auto space = literal!int(" ");
    auto sum = p * space * p * space * p;
    auto res = sum.run(state);
    assert(res.success);
    assert(res.parsed == "1 2 3");
    assert(res.value == 6);
}
unittest
{
    string str1 = "foo bar";
    auto s1 = ParserState!int(str1);

    auto p1 = literal!int("foo")
        * literal!int(" ").discard
        * literal!int("bar");

    auto res1 = p1.run(str1);
    assert(res1.success);
    assert(res1.parsed == "foobar");
}

/* ---------- fundamental parsers ---------- */

/* Parses a literal string (case-sensitive by default). */
auto
literal(B, S = string)(S str, bool consumeInput = true, bool caseSensitive = true) 
    if (isSomeString!S)
{
    import std.string;

    S use = str;
    if (!caseSensitive) use = use.toLower;
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
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
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
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

/* Always succeeds. Useful if the first thing in the chain you want to do is to
   build value.
   */
auto
succeed(B, S = string)()
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            return toParse.succeed;
        }
    }
    return new Res();
}
unittest
{
    string str = "foo";
    assert(succeed!string.match(str));
}

/* Fails if given condition returns false, succeeds consuming no input otherwise. */
auto
test(B, S = string)(bool delegate (B, S) tst)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
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

/* Builds a value from previous parser's output. Always succeeds. */
auto
build(B, S = string)(B delegate (B, S) dg)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            auto res = toParse;
            res.value = dg(toParse.value, toParse.parsed);
            return res;
        }
    }
    return new Res();
}
unittest
{
    string str = "foo";
    auto state = ParserState!int(str);

    auto p = build!int((res, s) => 10);
    auto res = p.run(state);
    assert(res.success);
    assert(res.value == 10);
}

/* Makes a copy of '.parsed' and '.left'. Can be used to drop a long string 
   from the memory if only a small portion of it is used. Always succeeds.
   */
auto
force(B, S = string)()
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            auto res = toParse;
            res.left = toParse.left.dup;
            res.parsed = toParse.parsed.dup;
            return res.succeed;
        }
    }
    return new Res();
}

/* Uses the same parser between 'min' and 'max' times. If either of 'min' and
   'max' is negative, there's no limit on corresponding allowed amount of
   times. Value is passed from each run to the next one, with resulting 
   parser inheriting the value from the last run. 
   */
auto 
many(B, S = string)(int min, int max, Parser!(B, S) p)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
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
    string str2 = "foo bar";
    auto s1 = ParserState!int(str1);
    auto s2 = ParserState!int(str2);
    auto digit = singleChar!int(ch => '0' <= ch && ch <= '9');

    auto p1 = many(1, -1, digit) % (res, i) => to!int(i);
    auto p2 = many(1, -1, digit % (res, i) => res * 10 + to!int(i));

    auto res1_1 = p1.run(s1);
    assert(res1_1.success);
    assert(res1_1.value == 123);
    auto res1_2 = p1.run(s2);
    assert(!res1_2.success);

    auto res2_1 = p2.run(s1);
    assert(res2_1.success);
    assert(res2_1.value == 123);
    auto res2_2 = p2.run(s2);
    assert(!res2_2.success);
}

/* Uses a subparser and absorbs its built value into main chain's one by
   passing it through a given delegate. 
   */
auto
absorb(B, B2, S = string)(B delegate (B, B2, S) dg, Parser!(B2, S) subparser)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
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
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
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
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
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

/* Throws an exception if the parser chain is in the success state. */
auto 
throwOnSuccess(B, S = string)(Exception exc)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;
            throw exc;
        }
    }
    return new Res();
}
unittest
{
    import std.exception;

    string str1 = "foobar";
    string str2 = "fooBAR";
    auto p = literal!int("foo") / 
        literal!int("bar") / 
        throwOnSuccess!int(new Exception("Parse succesful"));

    assertThrown(p.match(str1));
    assertNotThrown(p.match(str2));
}

/* Throws an exception if the parser chain is in the failed state. */
auto 
throwOnFailure(B, S = string)(Exception exc)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        this() { oblivious = true; }
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (toParse.success) 
                return toParse.succeed;
            else
                throw exc;
        }
    }
    return new Res();
}
unittest
{
    import std.exception;

    string str1 = "foobar";
    string str2 = "fooBAR";
    auto p = literal!int("foo") / 
        literal!int("bar") / 
        throwOnFailure!int(new Exception("Parse succesful"));

    assertNotThrown(p.match(str1));
    assertThrown(p.match(str2));
}

/* Throws an exception. */
auto 
throwAnyway(B, S = string)(Exception exc)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        this() { oblivious = true; }
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            throw exc;
        }
    }
    return new Res();
}
unittest
{
    import std.exception;

    string str1 = "foobar";
    string str2 = "fooBAR";
    auto p = literal!int("foo") / 
        literal!int("bar") / 
        throwAnyway!int(new Exception("Throwing in any case"));

    assertThrown(p.match(str1));
    assertThrown(p.match(str2));
}

/* ---------- conditional parsers ---------- */

/* Be extra careful with the following parsers: they always succeed and are not
   guaranteed to consume any input. This can lead to infinite loops. 
   */

/* Parses characters while a condition is met. */
auto
charWhile(B, C = char)(bool delegate (C) test, bool keepTerminator = true)
    if (isSomeChar!C)
{
    alias S = immutable(C)[];
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
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

    auto word = charWhile!string(c => c != ' ', false);
    auto res1 = word.run(state1);
    assert(res1.parsed == "foo");
    assert(res1.left == " bar");
    /* Notice that the following does succeed, but with empty string parsed. */
    auto res2 = word.run(res1);
    assert(res2.parsed == ""); 
}

/* Parses characters until a condition is met. */
auto
charUntil(B, C = char)(bool delegate (C) test, bool keepTerminator = true)
    if (isSomeChar!C)
{
    return charWhile!(B, C)(c => !test(c), keepTerminator);
}

/* Uses the same parser while a condition is met. The condition function takes
   (in order) currently built value, parsed string and iteration (0-based).
   Always succeeds.
   */
auto
repeatWhile(B, S = string)(bool delegate (B, S, int) test, Parser!(B, S) p)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) run(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;

            auto old = toParse;
            auto cur = toParse;
            int n = 0;
            while (true) {
                old = cur;
                cur = p.run(cur);
                if (!cur.success) break;
                if (!test(cur.value, cur.parsed, n)) break;
                n++;
            }

            return old.succeed;
        }
    }
    return new Res();
}
unittest
{
    import std.conv;

    auto str = "12345";
    auto state = ParserState!int(str);
    auto digit = singleChar!int(c => '0' <= c && c <= '9')
        % (res, s) => res * 10 + to!int(s);

    auto p1 = repeatWhile!int((res, s, i) => i < 3, digit);
    auto res1 = p1.run(state);
    assert(res1.success);
    assert(res1.value == 123);

    auto p2 = repeatWhile!int((res, s, i) => res < 100, digit);
    auto res2 = p2.run(state);
    assert(res2.success);
    assert(res2.value == 12);
}

/* Uses the same parser until a condition is met. The condition function is the
   same as for 'repeatWhile'.
   */
auto
repeatUntil(B, S = string)(bool delegate (B, S, int) test, Parser!(B, S) p)
    if (isSomeString!S)
{
    return repeatWhile((b, s, i) => !test(b, s, i), p);
}
