/* Core module

   Provides base Parser interface and some basic parser generators.

   Note that most parsers that do not care about previous return value's type
   default to using 'string' as the input type. This of course can be 
   overridden if you need different input type.
   */

module parsed.core;

import std.traits;

/* ---------- base ---------- */

struct ParserState(O, S = string) /* O(utput) and S(tring). */
    if (isSomeString!S)
{
    private alias ThisState = ParserState!(O, S);

    S left;
    O returnValue;
    bool success = true;
    bool empty = false;

    this(S toParse)
    {
        left = toParse;
    }

    ThisState fail()
    {
        auto res = this;
        res.success = false;
        res.returnValue = O.init;
        return res;
    }

    ParserState!(O2, S) fail(O2)()
    {
        ParserState!(O2, S) res;
        res.success = false;
        res.returnValue = O2.init;
        res.empty = false;
        res.left = left;
        return res;
    }

    ThisState succeed()
    {
        success = true;
        empty = false;
        return this;
    }

    ThisState succeed(O retVal)
    {
        auto res = this;
        res.success = true;
        res.empty = false;
        res.returnValue = retVal;
        return res;
    }

    ParserState!(O2, S) morph(O2)(O2 delegate (O) dg)
    {
        ParserState!(O2, S) res;
        if (success)
            res.returnValue = dg(returnValue);
        else
            res.returnValue = O2.init;
        res.left = left;
        res.success = success;
        res.empty = empty;
        return res;
    }

    ParserState!(O2, S) replace(O2)(O2 newValue)
    {
        ParserState!(O2, S) res;
        if (success)
            res.returnValue = newValue;
        else
            res.returnValue = O2.init;
        res.left = left;
        res.success = success;
        res.empty = empty;
        return res;
    }

    ThisState pass()
    {
        auto res = this;
        res.success = true;
        res.empty = true;
        return res;
    }
}

interface Parser(I, O, S = string) /* I(nput), O(utput) and S(tring). */
    if (isSomeString!S)
{
    private:
        alias OutState = ParserState!(O, S);
        alias InState = ParserState!(I, S);
        alias ThisParser = Parser!(I, O, S);
    
    public:

    /* ---------- high-level operations ---------- */

    /* Main method. */
    OutState run(InState toParse);

    /* Run the parser on the given text. */
    final OutState run(S text)
    {
        return run(InState(text));
    }

    /* Return true if the parser succeeds on the text. */
    final bool match(S text)
    {
        auto res = run(text);
        return res.success;
    }

    /* ---------- parser combinations ---------- */

    /* Feed this parser's return value to the second one, collecting their
       output together with '~'. As to why this must produce a parser with
       the same output type as first parser's input type, see the note near
       the beginning of the module. */
    final Parser!(I, O2, S) chain(O2)(Parser!(O, O2, S) other)
    {
        class Res: Parser!(I, O2, S)
        {
            ParserState!(O2, S) run(InState toParse)
            {
                InState old = toParse;
                auto res1 = this.outer.run(toParse);
                auto res2 = other.run(res1);
                if (res1.success && res2.success) 
                    return res2.succeed(res1.returnValue ~ res2.returnValue);
                else
                    return old.fail!O2;
            } 
        } 
        return new Res();
    } 

    /* Feed previous parser's output to this parser and replace it with this
       one's. As to why this must produce a parser with the same output type as
       first parser's input type, see the note near the beginning of the
       module.  */
    final Parser!(I, O2, S) sequence(O2)(Parser!(O, O2, S) other)
    {
        class Res: Parser!(I, O2, S)
        {
            ParserState!(O2, S) run(InState toParse)
            {
                InState old = toParse;
                auto res1 = this.outer.run(toParse);
                auto res2 = other.run(res1);
                if (res1.success && res2.success)
                    return res2.succeed;
                else
                    return old.fail!O2;
            }
        }
        return new Res();
    }

    /* Try running two parsers, collect the first one that succeeds. */
    final ThisParser any(ThisParser other)
    {
        class Res: Parser
        {
            OutState run(InState toParse)
            {
                InState old = toParse;
                auto res = this.outer.run(toParse);
                if (res.success) return res.succeed;
                return other.run(toParse);
            }
        }
        return new Res();
    }

    /* Perform an action on the parser's return value. Note that the delegate
       used should be able to handle .init of original parser's output type as
       an argument, as it'll be called on it in case of parsing failure. */
    final Parser!(I, O2, S) morph(O2)(O2 delegate(O) dg)
    {
        class Res: Parser
        {
            ParserState!(O2, S) run(InState toParse)
            {
                return this.outer.run(toParse).morph(dg);
            }
        }
        return new Res();
    }

    /* Make a new parser that passes return value transparently. */
    final ThisParser discard()
    {
        class Res: Parser
        {
            OutState run(InState toParse)
            {
                OutState res = this.outer.run(toParse);
                if (res.success)
                    return res.pass;
                else
                    return res.fail;
            }
        }
        return new Res();
    }

    /* ---------- operator overloads ---------- */ 

    /* Chain discarding first output. Think of '/' as a wall where flow stops. */
    final Parser!(I, O2, S) opBinary(string op, O2)(Parser!(O, O2, S) other)
        if (op == "/")
    {
        return sequence(other);
    }

    /* Chain collecting first output. Think of '*' as a piece of chain. */
    final Parser!(I, O2, S) opBinary(string op, O2)(Parser!(O, O2, S) other)
        if (op == "*")
    {
        return chain(other);
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
    string str = "foo bar baz";
    auto state = ParserState!string(str);

    auto p1 = literal("foo")
        * literal(" ")
        * literal("bar");
    auto s1 = p1.run(state);
    assert(s1.returnValue == "foo bar");

    auto p2 = literal("foo")
        / literal(" ")
        / literal("bar");
    auto s2 = p2.run(state);
    assert(s2.returnValue == "bar");
}

/* ---------- fundamental parsers ---------- */

/* Parses a literal string (case-sensitive by default). Returns parsed string. */
auto
literal(I = string, S = string)(S str, bool consumeInput = true, bool caseSensitive = true) 
{
    import std.string;

    S use = str;
    if (!caseSensitive) use = use.toLower;
    class Res: Parser!(I, S, S)
    {
        ParserState!(S, S) run(ParserState!(I, S) toParse)
        {
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

    auto p1 = literal("Hello");
    assert(p1.match(str));

    auto p2 = literal("hello", true, false);
    assert(p2.match(str));

    assert(!(p1 * p2).match(str));

    auto p3 = literal("Hello")
        * literal(" ")
        * literal("world");
    assert(p3.match(str));
}

/* Parses a literal string (case sensitive by default). Returns a given constant. */
auto
literal(I = string, O, S = string)(S str, 
        O returnValue, 
        bool consumeInput = true, 
        bool caseSensitive = true)
{
    import std.string;

    S use = str;
    if (!caseSensitive) use = use.toLower;
    class Res: Parser!(I, O, S)
    {
        ParserState!(O, S) run(ParserState!(I, S) toParse)
        {
            S checkAgainst = toParse.left;
            if (!caseSensitive) checkAgainst = checkAgainst.toLower;
            auto afterParse = toParse.replace(returnValue);
            if (checkAgainst.startsWith(use)) {
                if (consumeInput)
                    afterParse.left = toParse.left[use.length .. $];
                return afterParse.succeed;
            }
            return afterParse.fail;
        }
    }
    return new Res();
}
unittest
{
    string str = "Foo bar baz";
    auto state = ParserState!string(str);

    auto p1 = literal("Foo", 5);
    auto state2 = p1.run(state);
    assert(state2.returnValue == 5);
}

/* Always succeeds with given return value, consuming no input. */
auto
produce(I = string, O, S = string)(O returnValue)
{
    class Res: Parser!(I, O, S)
    {
        ParserState!(O, S) run(ParserState!(I, S) toParse)
        {
            return toParse.succeed.replace(returnValue);
        }
    }
    return new Res();
}
unittest
{
    string str = "foo";
    auto state = ParserState!string(str);

    auto p = produce(10);
    auto s = p.run(state);
    assert(s.returnValue == 10);
}

/* Always fails consuming to input. */
auto
fail(I = string, O, S = string)()
{
    class Res: Parser!(I, O, S)
    {
        ParserState!(O, S) run(ParserState!(I, S) toParse)
        {
            return toParse.fail!O;
        }
    }
    return new Res();
}
unittest
{
    string str = "foo";
    auto state = ParserState!string(str);

    auto p = fail!(string, string);
    assert(!p.match(str));
}

/* Transforms previous return value with a given delegate. Succeeds if the
   previous parser did. */
auto
morph(I, O, S = string)(O delegate(I) dg)
{
    class Res: Parser!(I, O, S)
    {
        ParserState!(O, S) run(ParserState!(I, S) toParse)
        {
            return toParse.morph(dg);
        }
    }
    return new Res();
}
unittest
{
    import std.string;

    string str = "foo";
    auto state = ParserState!string(str);

    auto p = literal("foo")
        / morph!string(x => x.toUpper);
    auto s = p.run(state);
    assert(s.returnValue == "FOO");
}

/* Uses the same parser between 'min' and 'max' times. If either of 'min' and
   'max' is negative, there's no limit on corresponding allowed amount of
   times. Concatenates each run's output into single object, so make sure that
   the parser's return value supports '~' operator.

   Note that each run's output is *not* going to be fed to the parser on the
   next run.
   */
auto 
many(I = string, O, S = string)(Parser!(I, O, S) p, int min, int max)
{
    class Res: Parser!(I, O, S)
    {
        ParserState!(O, S) run(ParserState!(I, S) toParse)
        {
            O res;
            ParserState!(O, S) cur = toParse.succeed.replace(O.init);

            /* Check required minimum of successful parses. */
            size_t n = 0;
            if (min > 0) {
                while (n < min) {
                    cur = p.run(cur.replace(I.init));
                    if (!cur.success) return toParse.fail!O;
                    n++;
                    res ~= cur.returnValue;
                }
            }

            /* Parse the rest. */
            while ((max < 0 || (max > 0 && n < max)) && cur.success) {
                cur = p.run(cur.replace(I.init));
                n++;
                res ~= cur.returnValue;
            }

            return cur.succeed(res);
        }
    }
    return new Res();
}
unittest
{
    string str1 = "111 11";
    string str2 = "112 aa";
    auto s2 = ParserState!string(str2);

    auto p1 = many(literal("1"), 3, 3);
    assert(p1.match(str1));
    assert(!p1.match(str2));

    auto p2 = many(literal("1"), 0, 4);
    assert(p2.match(str1));
    assert(p2.match(str2));

    auto p3 = many(literal("1"), 4, -1);
    assert(!p3.match(str1));
    assert(!p3.match(str2));

    auto p4 = many(literal("2"), 1, -1);
    assert(!p4.match(str1));
    assert(!p4.match(str2));

    auto p5 = p2 * p4;
    auto res5 = p5.run(s2);
    assert(res5.success);
    assert(res5.returnValue == "112");
}

/* Uses the parser zero or one times. Basically many(0, 1). */
auto 
maybe(I = string, O, S = string)(Parser!(I, O, S) p)
{
    return many!(I, O, S)(p, 0, 1);
}
unittest
{
    string str1 = "foo bar";
    string str2 = "foobar";
    string str3 = "foo-bar";

    auto p = literal("foo") 
        * maybe(literal(" "))
        * literal("bar");
    assert(p.match(str1));
    assert(p.match(str2));
    assert(!p.match(str3));
}

/* Returns one of two supplied return values based on a result of applying a
   given test to the previous return value. Always succeeds and consumes no
   input.
   */
auto
fork(I, O, S = string)(bool delegate(I) test, O onTrue, O onFalse)
{
    class Res: Parser!(I, O, S)
    {
        ParserState!(O, S) run(ParserState!(I, S) toParse)
        {
            if (test(toParse.returnValue))
                return toParse.succeed.replace(onTrue);
            else
                return toParse.succeed.replace(onFalse);
        }
    }
    return new Res();
}
unittest
{
    string str = "foo bar baz";
    auto initial = ParserState!string(str);

    auto p = literal("foo")
        / fork!string(x => x == "foo", 1, 2);
    auto s = p.run(initial);
    assert(s.returnValue == 1);
}

/* ---------- conditional parsers ---------- */

/* Parses a single character that passes a test. Fails on empty strings and if
   the test is failed on the first character in the input. Returns parsed
   character as a string with unit length.
   */
auto
singleChar(I = string, T = char)(bool delegate(T) test)
{
    alias S = immutable(T)[];
    class Res: Parser!(I, S, S)
    {
        ParserState!(S, S) run(ParserState!(I, S) toParse)
        {
            if (toParse.left.length == 0)
                return toParse.fail!S;
            T ch = toParse.left[0];
            S s = [ch];
            if (test(ch)) {
                auto res = toParse.replace(s);
                res.left = res.left[1 .. $];
                return res.succeed;
            } else {
                return toParse.fail!S;
            }
        }
    }
    return new Res();

}
unittest
{
    string str1 = "foo bar";
    string str2 = "baz bar";
    string str3 = "abcdefghijk";
    auto state3 = ParserState!string(str3);

    auto p1 = singleChar!()(ch => ch == 'f'); 

    assert(p1.match(str1));
    assert(!p1.match(str2));
    assert(!p1.match(str3));

    /* Same. */
    auto p2 = many(singleChar!()(ch => 'a' <= ch && ch <= 'e'), -1, -1);
    auto s2 = p2.run(state3);
    assert(s2.returnValue == "abcde");
}

/* Be very careful with these, as they always succeed and may not consume any
   input, which can result in infinite loops.
   */

/* Collects characters while a condition is met. Returns collected string. */
auto 
collectWhile(I = string, T = char)(
        bool delegate (T) test,
        bool keepTerminator = true)
    if (isSomeChar!T)
{
    alias S = immutable(T)[];
    class Res: Parser!(I, S, S)
    {
        ParserState!(S, S) run(ParserState!(I, S) toParse)
        {
            size_t i = 0;
            while (i < toParse.left.length && test(toParse.left[i]))
                i++;
            S res;
            if (keepTerminator && (i + 1) < toParse.left.length) {
                res = toParse.left[0 .. i + 1];
                toParse.left = toParse.left[i + 1 .. $];
            } else {
                res = toParse.left[0 .. i];
                toParse.left = toParse.left[i .. $];
            }
            return toParse.succeed.replace(res);
        }
    }
    return new Res();
}
unittest
{
    string str = "123 23 23";
    auto state = ParserState!string(str);

    auto p = collectWhile!()(x => '0' <= x && x <= '9', false);
    auto state2 = p.run(state);

    assert(state2.returnValue == "123");
}

/* Collects characters until a condition is met. Returns collected string. */
auto 
collectUntil(I = string, T = char)(
        bool delegate (T) test,
        bool keepTerminator = true)
    if (isSomeChar!T)
{
    return collectWhile!(I, T)(x => !test(x), keepTerminator);
}

/* Drops characters while a condition is met. Passes previous return value 
   transparently.
   */
auto
dropWhile(I = string, T = char)(
        bool delegate (T) test,
        bool dropTerminator = true)
    if (isSomeChar!T)
{
    return collectWhile!(I, T)(test, dropTerminator).discard;
}

/* Drops characters until a condition is met. Passes previous return value
   transparently.
   */
auto
dropUntil(I = string, T = char)(
        bool delegate (T) test,
        bool dropTerminator = true)
    if (isSomeChar!T)
{
    return collectUntil!(I, T)(test, dropTerminator).discard;
}
