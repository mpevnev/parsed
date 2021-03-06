/* Core module

   Provides base Parser interface and some basic parser generators.

   If you need to write a parser yourself, be sure to make it short-circuiting
   - include 'if (!toParse.success) return toParse.fail;' as the first line of
   its 'run' method (unless it's inherently oblivious, see below).

   Parsers can be made 'oblivious' by using makeOblivious method. An oblivious
   parser doesn't care if the previous chain has failed, it'll try to parse
   anyway. Oblivious parsers can fail or succeed as usual. This provides an
   ability to: one, recover from parser errors (kind of like | operator does),
   and, two, perform some operations if the chain has failed. For an example of
   the latter see the 'throwOnFailure' parser.

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
    bool recovered = false;

    private struct Slice
    {
        size_t start, end;
    }

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

    /* ---------- slicing overloads ---------- */

    Slice opSlice(size_t Dim)(size_t start, size_t end)
    {
        return Slice(start, end);
    }

    size_t opDollar(size_t Dim)()
    {
        return left.length;
    }

    ThisState opIndex(Slice slice)
    {
        auto res = this;
        res.left = left[slice.start .. slice.end];
        return res;
    }

    /* ---------- manipulation ---------- */

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
        res.recovered = false;
        return res;
    }

    ThisState succeed(S withParsed)
    {
        auto res = this;
        res.success = true;
        res.recovered = false;
        res.parsed = withParsed;
        return res;
    }

    ThisState build(B delegate (B, const S) dg)
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
            B delegate (B, B2, const S) dg)
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
        res.recovered = false;
        res.parsed = null;
        return res;
    }

    ThisState recover()
    {
        auto res = this;
        res.recovered = true;
        res.success = true;
        return res;
    }
}
unittest
{
    string str1 = "foo bar";

    auto s1 = ParserState!int(str1);

    auto slice1_1 = s1[0 .. 3];
    auto slice1_2 = s1[3 .. $];
    assert(slice1_1.left == "foo");
    assert(slice1_2.left == " bar");
}

/* B(uild) and S(tring). */
class Parser(B, S = string) 
    if (isSomeString!S)
{
    private:

    alias State = ParserState!(B, S);
    alias ThisParser = Parser!(B, S);
    alias Group = ParserGroup!(B, S);

    /* This should be true if the parser is able to operate even if the chain
       is in the failed state. */
    protected bool oblivious_ = false;
    protected LookaheadMode lookahead = LookaheadMode.none;

    public:

    bool oblivious() const @property { return oblivious_; }

    /* ---------- high-level operations ---------- */

    /* A wrapper over 'parse' that performs some operations. */
    State run(State toParse)
    {
        if (!toParse.success && oblivious)
            toParse = toParse.recover;
        if (!toParse.success) 
            return toParse.fail;
        return parse(toParse);
    }

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

    /* Main method. */
    protected abstract State parse(State toParse);

    /* ---------- parser combinations ---------- */

    /* Builds up a value. */
    ThisParser build(B delegate (B, const S) dg)
    {
        class Res: ThisParser
        {
            this()
            {
                oblivious_ = this.outer.oblivious;
                lookahead = this.outer.lookahead;
            }

            override State parse(State toParse)
            {
                auto outer = this.outer;
                auto res = outer.parse(toParse);
                if (res.success) {
                    res.value = dg(res.value, res.parsed);
                    return res.succeed;
                } else {
                    return res.fail;
                }
            } /* parse */
        } /* Res */
        return new Res();
    } /* build */

    Group chain(ThisParser other, bool concat, bool prepend)
    {
        auto res = new Group(GroupType.and, false);
        if (prepend)
            res.parsers = [other, this];
        else
            res.parsers = [this, other];
        res.concat = [false, concat];
        return res;
    }

    Group chain(Group other, bool concat, bool prepend)
    {
        return other.chain(this, concat, prepend);
    }

    /* Returns state of the first parser of the two to succeed. */
    Group any(ThisParser other, bool prepend)
    {
        auto res = new Group(GroupType.or, false);
        if (prepend)
            res.parsers = [other, this];
        else
            res.parsers = [this, other];
        return res;
    }

    /* An overload for Groups. */
    Group any(Group other, bool prepend)
    {
        return other.any(this, prepend);
    }

    /* Make a new parser that discards original parser's 'parsed' and sets it
       to an empty string */
    ThisParser discard()
    {
        class Res: ThisParser
        {
            this()
            {
                oblivious_ = this.outer.oblivious;
                lookahead = this.outer.lookahead;
            }

            override State parse(State toParse)
            {
                auto res = this.outer.parse(toParse);
                if (res.success) {
                    res.parsed = "";
                    return res.succeed;
                } else {
                    return toParse.fail;
                }
            } /* parse */
        } /* Res */
        return new Res();
    } /* discard */

    ThisParser makeOblivious()
    {
        class Res: ThisParser
        {
            this() 
            { 
                oblivious_ = true; 
                lookahead = this.outer.lookahead;
            }

            override State parse(State toParse)
            {
                return this.outer.parse(toParse);
            }
        }
        return new Res();
    }

    /* Need a group because that's where lookahead driver is. */
    Group makeReluctant()
    {
        class Res: ThisParser
        {
            this()
            { 
                lookahead = LookaheadMode.reluctant; 
                oblivious_ = this.outer.oblivious_;
            }

            override State parse(State toParse)
            {
                return this.outer.parse(toParse);
            }
        }
        return new Group(new Res, false);
    }

    /* Need a group because that's where lookahead driver is. */
    Group makeGreedy()
    {
        class Res: ThisParser
        {
            this()
            { 
                lookahead = LookaheadMode.greedy; 
                oblivious_ = this.outer.oblivious_;
            }

            override State parse(State toParse)
            {
                return this.outer.parse(toParse);
            }
        }
        return new Group(new Res, false);
    }

    /* Make a new parser that doesn't consume input. */
    ThisParser noConsume()
    {
        class Res: ThisParser
        {
            this()
            {
                oblivious_ = this.outer.oblivious_;
                lookahead = this.outer.lookahead;
            }

            override State parse(State toParse)
            {
                auto res = this.outer.parse(toParse);
                res.left = toParse.left;
                return res;
            }
        }
        return new Res();
    }

    /* Make a new parser that doesn't build the value. */
    ThisParser noBuild()
    {
        class Res: ThisParser
        {
            this()
            {
                oblivious_ = this.outer.oblivious_;
                lookahead = this.outer.lookahead;
            }

            override State parse(State toParse)
            {
                auto value = toParse.value;
                auto res = this.outer.parse(toParse);
                res.value = value;
                return res;
            }
        }
        return new Res();
    }

    /* ---------- operator overloads ---------- */ 

    /* Infix analog of 'chain' without parsed string concatenation. Think of 
       '/' as a wall where flow stops. */
    Group opBinary(string op)(ThisParser other)
        if (op == "/")
    {
        return chain(other, false, false);
    }

    /* Infix analog of 'chain' with parsed string concatenation. Think of '*'
       as of a piece of chain. */
    Group opBinary(string op)(ThisParser other)
        if (op == "*")
    {
        return chain(other, true, false);
    }

    /* Infix analog of 'build'. I've got no clever analogy as to why it's '%'.
       The real reason is that '%' is in the same precedence group as '*' and
       '/'. */
    ThisParser opBinary(string op)(B delegate (B, const S) dg)
        if (op == "%")
    {
        return build(dg);
    }

    /* Infix analog of 'any' */
    Group opBinary(string op)(ThisParser other)
        if (op == "|")
    {
        return any(other, false);
    }
}
unittest
{
    /* Building test. */

    import std.conv;

    string str1 = "1 2 3";
    auto s1 = ParserState!int(0, str1);

    auto p1 = (literal!int("1") | literal!int("2") | literal!int("3"))
        % (int i, string s) => i + to!int(s);
    auto space = literal!int(" ");

    auto sum1 = p1 * space * p1 * space * p1;
    auto res1_1 = sum1.run(s1);
    assert(res1_1.success);
    assert(res1_1.parsed == "1 2 3");
    assert(res1_1.value == 6);

    auto sum2 = p1 * space * p1.noBuild * space * p1;
    auto res2_1 = sum2.run(s1);
    assert(res2_1.success);
    assert(res2_1.parsed == "1 2 3");
    assert(res2_1.value == 4);
}
unittest
{
    /* Discard and chaining test. */

    string str1 = "foo bar";
    auto s1 = ParserState!int(str1);

    auto p1 = literal!int("foo")
        * literal!int(" ").discard
        * literal!int("bar");

    auto res1 = p1.run(str1);
    assert(res1.success);
    assert(res1.parsed == "foobar");
}
unittest
{
    /* noConsume test. */

    string str1 = "foofoo";
    auto s1 = ParserState!int(str1);

    auto p1 = literal!int("foo").noConsume;

    auto res1_1 = p1.run(s1);
    assert(res1_1.success);
    assert(res1_1.parsed == "foo");
    assert(res1_1.left == "foofoo");
}

/* B(uild) and S(tring). */
private class ParserGroup(B, S = string): Parser!(B, S)
    if (isSomeString!S)
{
    /* All members are private unless stated otherwise. */
    private:

    Parser!(B, S)[] parsers;
    bool[] concat;
    GroupType type;
    bool monolithic;

    public override bool oblivious() const @property
    {
        import std.algorithm;
        return parsers.any!(x => x.oblivious);
    }

    alias Group = ParserGroup!(B, S);
    alias ThisParser = Parser!(B, S);
    alias State = ParserState!(B, S);

    this(GroupType type, bool monolithic)
    {
        this.type = type;
        this.monolithic = monolithic;
    }

    this(Group original, bool monolithic)
    {
        parsers = original.parsers.dup;
        concat = original.concat.dup;
        type = original.type;
        this.monolithic = monolithic;
    }

    this(ThisParser parser, bool monolithic)
    {
        this(GroupType.and, monolithic);
        lookahead = parser.lookahead;
        parsers = [parser];
        concat = [false];
    }

    /* Obliviousness handling is a bit different in groups. */
    public override State run(State toParse)
    {
        if (toParse.success || oblivious) {
            return parse(toParse);
        } else {
            return toParse.fail;
        }
    }

    public alias run = ThisParser.run; /* Allows other overloads of run. */

    protected override State parse(State toParse)
    {
        if (type == GroupType.and) {
            /* Sequential application of parsers. */
            return tryRun(toParse, 0, parsers);
        } else {
            /* Alternative application of parsers. */
            State save = toParse;
            foreach (current; parsers) {
                auto maybeRes = current.run(save);
                if (maybeRes.success) return maybeRes.succeed;
            }
            return toParse.fail;
        } /* if type == GroupType.and */
    } /* parse */

    /* A helper function to deal with in-chain lookahead. */
    State tryRun(State tryParse, 
            size_t i, 
            ThisParser[] leftParsers)
    {
        if (leftParsers == []) {
            if (tryParse.success)
                return tryParse.succeed;
            else
                return tryParse.fail;
        }

        /* A helper function to avoid excessive typing. */
        void prepend(ref State to, State prep) {
            if (concat[i] && to.success)
                to.parsed = prep.parsed ~ to.parsed;
        }

        ThisParser current = leftParsers[0];
        final switch (current.lookahead) {
            case LookaheadMode.none:
                auto newState = current.run(tryParse);
                prepend(newState, tryParse);
                return tryRun(newState, i + 1, leftParsers[1 .. $]);
            case LookaheadMode.reluctant:
                size_t end = 0;
                size_t len = tryParse.left.length;
                while (end <= len) {
                    State substate = tryParse[0 .. end];
                    State newState = current.run(substate);
                    if (newState.success) {
                        newState.left ~= tryParse.left[end .. $];
                        prepend(newState, tryParse);
                        State newerState = tryRun(newState, i + 1, leftParsers[1 .. $]);
                        if (newerState.success)
                            return newerState.succeed;
                        else
                            end++;
                    } else {
                        end++;
                    }
                } /* while end < len */
                return tryParse.fail;
            case LookaheadMode.greedy:
                size_t len = tryParse.left.length;
                size_t end = len;
                while (end > 0) {
                    State substate = tryParse[0 .. end];
                    State newState = current.run(substate);
                    if (newState.success) {
                        newState.left ~= tryParse.left[end .. $];
                        prepend(newState, tryParse);
                        State newerState = tryRun(newState, i + 1, leftParsers[1 .. $]);
                        if (newerState.success)
                            return newerState.succeed;
                        else
                            end--;
                    } else {
                        end--;
                    }
                } /* while end > 0 */
                return tryParse.fail;
        } /* switch current.lookahead */
    } /* tryRun */

    public final Group makeMonolithic()
    {
        return new Group(this, true);
    }

    /* Either append or prepend a parser to the chain. */
    public override Group chain(ThisParser other, bool concat, bool prepend)
    {
        /* We simply wrap the group and the other parser in a new group in two
           cases: when the group is monolithic, or when we can't chain extra
           parsers to the group (because it's not an actual chain, it's a
           choice construct). */
        if (monolithic || type == GroupType.or) {
            auto res = new Group(this, false);
            if (prepend) {
                res.parsers = [other, this];
                res.concat = [false, concat];
            } else {
                res.parsers = [this, other];
                res.concat = [false, concat];
            }
            return res;
        } else {
            auto res = new Group(this, false);
            if (prepend) {
                res.parsers = other ~ parsers;
                res.concat = false ~ this.concat;
            } else {
                res.parsers = parsers ~ other;
                res.concat = this.concat ~ concat;
            }
            return res;
        } /* if monolithic */
    } /* chain */

    /* Same, but add a chain instead of an individual parser. */
    public override Group chain(Group other, bool concat, bool prepend)
    {
        auto res = new Group(GroupType.and, false);
        /* We treat OR groups as monolithic, because we can't add elements to
           them in this method. */
        if (monolithic && other.monolithic 
                || type == GroupType.or 
                || other.type == GroupType.or) {
            /* Produce a simple Group that uses both of these without
               unwrapping them. */
            if (prepend) 
                res.parsers = [other, this];
            else
                res.parsers = [this, other];
            res.concat = [false, concat];
        } else if (monolithic && !other.monolithic) {
            if (prepend) {
                res.parsers = other.parsers ~ this;
                res.concat = other.concat ~ concat;
            } else {
                res.parsers = this ~ other.parsers;
                res.concat = false ~ other.concat;
            }
        } else if (!monolithic && other.monolithic) {
            if (prepend) {
                res.parsers = other ~ parsers;
                res.concat = false ~ this.concat;
                res.concat[1] = concat;
            } else {
                res.parsers = parsers ~ other;
                res.concat = this.concat ~ concat;
            }
        } else {
            if (prepend) {
                size_t middle = other.parsers.length;
                res.parsers = other.parsers ~ parsers;
                res.concat = other.concat ~ this.concat;
                res.concat[middle] = concat;
            } else {
                size_t middle = parsers.length;
                res.parsers = parsers ~ other.parsers;
                res.concat = this.concat ~ other.concat;
                res.concat[middle] = concat;
            }
        } /* if monilithic combination */
        return res;
    } /* chain */

    public override Group any(ThisParser other, bool prepend)
    {
        /* We treat AND groups as monolithic because we can't add elements to
           them in this method. */
        auto res = new Group(GroupType.or, false);
        if (monolithic || type == GroupType.and) {
            if (prepend) 
                res.parsers = [other, this];
            else
                res.parsers = [this, other];
        } else {
            if (prepend) 
                res.parsers = other ~ parsers;
            else
                res.parsers = parsers ~ other;
        }
        return res;
    }

    public override Group any(Group other, bool prepend)
    {
        /* We treat AND groups as monolithic because we can't add elements to
           them in this method. */
        auto res = new Group(GroupType.or, false);
        if (monolithic && other.monolithic
                || type == GroupType.and 
                || type == GroupType.and) {
            if (prepend)
                res.parsers = [other, this];
            else
                res.parsers = [this, other];
        } else if (monolithic && !other.monolithic) {
            if (prepend)
                res.parsers = other.parsers ~ this;
            else
                res.parsers = this ~ other.parsers;
        } else if (!monolithic && other.monolithic) {
            if (prepend)
                res.parsers = other ~ parsers;
            else
                res.parsers = parsers ~ other;
        } else {
            if (prepend)
                res.parsers = other.parsers ~ parsers;
            else
                res.parsers = parsers ~ other.parsers;
        } /* if monolithic combination */
        return res;
    } /* any */
}
unittest
{
    /* Greed and reluctance test. */

    string str1 = "foo!bar!";
    auto s1 = ParserState!int(str1);

    auto p1 = everything!int.makeReluctant * literal!int("!");
    auto p2 = everything!int.makeGreedy * literal!int("!");

    auto res1_1 = p1.run(s1);
    assert(res1_1.success);
    assert(res1_1.parsed == "foo!");

    auto res2_1 = p2.run(s1);
    assert(res2_1.success);
    assert(res2_1.parsed == "foo!bar!");
}
unittest
{
    /* One more greed and reluctance test; this time the greedy/reluctant
       parser is the sole parser in the chain. */

    string str1 = "111";
    auto s1 = ParserState!int(str1);

    auto p1 = many(1, -1, literal!int("1")).makeReluctant;
    auto p2 = many(1, -1, literal!int("1")).makeGreedy;

    auto res1_1 = p1.run(s1);
    assert(res1_1.success);
    assert(res1_1.parsed == "1");

    auto res2_1 = p2.run(s1);
    assert(res2_1.success);
    assert(res2_1.parsed == "111");
}
unittest
{
    /* Obliviousness test. */

    string str1 = "foobar";
    auto s1 = ParserState!int(str1);

    auto p1 = literal!int("BAZ") 
        / literal!int("foo").makeOblivious
        * literal!int("bar");

    auto res1_1 = p1.run(s1);
    assert(res1_1.success);
    assert(res1_1.parsed == "foobar");
}
unittest
{
    /* Obliviousness and lookahead test. */

    string str1 = "12121";
    auto s1 = ParserState!int(str1);
    
    auto p1 = literal!int("0")
        / everything!int.makeOblivious.makeReluctant
        * literal!int("1");
    auto p2 = literal!int("0")
        / everything!int.makeOblivious.makeGreedy
        * literal!int("1");

    auto res1_1 = p1.run(s1);
    assert(res1_1.success);
    assert(res1_1.parsed == "1");

    auto res2_1 = p2.run(s1);
    assert(res2_1.success);
    assert(res2_1.parsed == "12121");
}

private enum LookaheadMode
{
    none,
    greedy,
    reluctant,
}

private enum GroupType
{
    and,
    or
}

/* ---------- fundamental parsers ---------- */

/* Parses a literal string (case-sensitive by default). */
auto
literal(B, S = string)(const S str, bool caseSensitive = true) 
    if (isSomeString!S)
{
    import std.string;

    S use = str;
    if (!caseSensitive) use = use.toLower;
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            S checkAgainst = toParse.left;
            if (!caseSensitive) checkAgainst = checkAgainst.toLower;
            if (checkAgainst.startsWith(use)) {
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

    auto p2 = literal!int("hello", false);
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
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
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
   build value and you dislike (relative) clunkiness of 'build' parser.
   */
auto
succeed(B, S = string)()
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            return toParse.succeed("");
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
test(B, S = string)(bool delegate (B, const S) tst)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            if (tst(toParse.value, toParse.parsed))
                return toParse.succeed("");
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
build(B, S = string)(B delegate (B, const S) dg)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            auto res = toParse;
            res.value = dg(toParse.value, toParse.parsed);
            res.parsed = "";
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
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            auto res = toParse;
            res.left = toParse.left.dup;
            res.parsed = toParse.parsed.dup;
            return res.succeed("");
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
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
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
                auto old = cur;
                cur = p.run(cur);
                if (cur.success) {
                    value = cur.value;
                    n++;
                    parsed ~= cur.parsed;
                } else {
                    return old.succeed(parsed);
                }
            }
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
absorb(B, B2, S = string)(B delegate (B, B2, const S) dg, Parser!(B2, S) subparser)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            auto sendToSub = ParserState!(B2, S)(toParse.left);
            auto returned = subparser.parse(sendToSub);
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
morph(B, S = string)(S delegate (const S) dg)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
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
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
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
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
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
        this() { oblivious_ = true; }
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            if (toParse.recovered)
                throw exc;
            else
                return toParse.succeed;
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
        throwOnFailure!int(new Exception("Parse failed"));

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
        this() { oblivious_ = true; }
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
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

/* Parses the rest of the input. */
auto
everything(B, S = string)()
    if (isSomeString!S)
{
    class Res: Parser!(B, S) 
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            auto res = toParse;
            res.parsed = toParse.left;
            res.left = "";
            return res.succeed;
        }
    }
    return new Res();
}
unittest
{
    string str1 = "foobar";

    auto s1 = ParserState!int(str1);

    auto p1 = everything!int;
    auto p2 = literal!int("foo") / everything!int;

    auto res1_1 = p1.run(s1);
    assert(res1_1.success);
    assert(res1_1.parsed == "foobar");

    auto res2_1 = p2.run(s1);
    assert(res2_1.success);
    assert(res2_1.parsed == "bar");
}

/* Catches an exception that might occur in another parser. If an exception is
   thrown inside 'main' parser, it is considered failed and 'onException'
   parser is run on the original input. */
auto
except(E, B, S = string)(Parser!(B, S) main, Parser!(B, S) onException)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            try {
                return main.run(toParse);
            } catch (E e) {
                return onException.run(toParse);
            }
        }
    }
    return new Res();
}
unittest
{
    import std.conv;

    string str1 = "12";
    string str2 = "12d";

    auto s1 = ParserState!int(str1);
    auto s2 = ParserState!int(str2);

    auto base = everything!int % (res, s) => s.to!int;

    auto p1 = base.except!(ConvException, int)(build!int((res, s) => 0));

    auto res1_1 = p1.run(s1);
    assert(res1_1.success);
    assert(res1_1.value == 12);

    auto res1_2 = p1.run(s2);
    assert(res1_2.success);
    assert(res1_2.value == 0);
}

/* Catches an exception that might occur in another parser. If such an
   exception is thrown, this overload fails. */
auto
except(E, B, S = string)(Parser!(B, S) main)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            try {
                return main.run(toParse);
            } catch (E e) {
                return toParse.fail;
            }
        }
    }
    return new Res();
}
unittest
{
    import std.conv;

    string str1 = "12";
    string str2 = "12d";

    auto s1 = ParserState!int(str1);
    auto s2 = ParserState!int(str2);

    auto base = everything!int % (res, s) => s.to!int;

    auto p1 = base.except!(ConvException, int)();

    auto res1_1 = p1.run(s1);
    assert(res1_1.success);
    assert(res1_1.value == 12);

    auto res1_2 = p1.run(s2);
    assert(!res1_2.success);
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
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
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
repeatWhile(B, S = string)(bool delegate (B, const S, int) test, Parser!(B, S) p)
    if (isSomeString!S)
{
    class Res: Parser!(B, S)
    {
        override ParserState!(B, S) parse(ParserState!(B, S) toParse)
        {
            if (!toParse.success) return toParse.fail;

            auto old = toParse;
            auto cur = toParse;
            int n = 0;
            while (true) {
                old = cur;
                cur = p.parse(cur);
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
repeatUntil(B, S = string)(bool delegate (B, const S, int) test, Parser!(B, S) p)
    if (isSomeString!S)
{
    return repeatWhile((b, s, i) => !test(b, s, i), p);
}
