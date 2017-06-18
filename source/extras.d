/* Extras module

   Provides some nice-to-have, often useful things.

   It publicly import core, so there's no need to import it if you use extras.

   */

module parsed.extras;

import std.traits;

public import parsed.core;

/* ---------- single-character parsers ---------- */

/* Parses a single char of whitespace. Fails on non-whitespace input and on
   empty strings. Returns parsed character as a string with unit length. */
auto
whitespace(I, T = char)(bool acceptNewline = true)
    if (isSomeChar!T)
{
    alias S = immutable(T)[];
    bool acceptable(T ch)
    {
        import std.uni;

        if (!ch.isWhite) return false;
        if (!acceptNewline && (ch == '\n' || ch == '\r')) return false;
        return true;
    }
    return singleChar!(I, T)(&acceptable);
}
unittest
{
    string str1 = "foo bar";
    string str2 = "foo\nbar";
    string str3 = "foobar";

    auto p = literal!string("foo")
        * whitespace!string;
    assert(p.match(str1));
    assert(p.match(str2));
    assert(!p.match(str3));
}

/* Parses a single non-whitespace character. Fails on whitespace and empty
   strings. Returns parsed character as a string with unit lenght. 
   */
auto
nonwhite(I, T = char)()
    if (isSomeChar!T)
{
    import std.uni;

    return singleChar!(I, T)(ch => !ch.isWhite);
}
unittest
{
    string str = "foo bar";
    auto state = ParserState!string(str);

    auto p = many(nonwhite!string, -1, -1);
    auto s2 = p.run(state);
    assert(s2.returnValue == "foo");

    assert(p.match(" foo")); /* Note that as given 'many' will also match 0
                                characters. */
    auto p2 = many(nonwhite!string, 1, -1);
    assert(!p2.match(" foo"));
}

/* Parses a single alphanumeric character. Returns parsed character as a 
   string with unit length.
   */
auto
alnum(I, T = char)()
    if (isSomeChar!T)
{
    import std.uni;

    return singleChar!(I, T)(ch => ch.isAlphaNum);
}
unittest
{
    import std.stdio;

    string str = "foo12'";
    auto initial = ParserState!string(str);

    auto p1 = many(alnum!string, 1, -1);
    auto s1 = p1.run(initial);
    assert(s1.returnValue == "foo12");
    assert(s1.left == "'");

    auto s2 = p1.run(s1);
    assert(!s2.success);
}

/* Parses a single alphabetic character. Returns parsed character as a string
   with unit length.
   */
auto
alpha(I, T = char)()
    if (isSomeChar!T)
{
    import std.uni;

    return singleChar!(I, T)(ch => ch.isAlpha);
}
unittest
{
    string str = "ab12";
    auto initial = ParserState!string(str);

    auto p1 = many(alpha!string, 1, -1);
    auto s1 = p1.run(initial);
    assert(s1.returnValue == "ab");
    assert(s1.left == "12");
}

/* Parses a single deciman digit. Returns parsed character as a string with
   unit length.
   */
auto
digit(I, T = char)()
    if (isSomeChar!T)
{
    import std.uni;

    return singleChar!(I, T)(ch => ch.isNumber);
}
unittest
{
    string str = "12a";
    auto initial = ParserState!string(str);

    auto p1 = many(digit!string, 1, -1);
    auto s1 = p1.run(initial);
    assert(s1.returnValue == "12");
    assert(s1.left == "a");
}

/* ---------- misc ---------- */

/* Parses a whole line and returns it. Always succeeds. */
auto
line(I, T = char)(bool keepNewline = true)
{
    import std.string;

    auto res = collectUntil!(I, T)(ch => ch == '\n' || ch == '\r', true);
    if (keepNewline)
        return res;
    else
        return res.morph(s => s.chomp);
}
unittest
{
    string str = "foo\nbar";
    auto initial = ParserState!string(str);

    auto p = line!string(false);
    auto s1 = p.run(initial);
    assert(s1.returnValue == "foo");

    auto s2 = p.run(s1);
    assert(s2.returnValue == "bar");
}

/* Strips away leading and trailing whitespace from previous parser's return
   value and returns resulting string. */
auto
strip(I, S = string)()
    if (isSomeString!I)
{
    import std.string;

    return morph!(I, I, S)(delegate (I s) { return std.string.strip(s); });
}
unittest
{
    string str = "   foo  \n";
    auto initial = ParserState!string(str);

    auto p = line!string(true) / strip!string;
    auto s1 = p.run(initial);
    assert(s1.returnValue == "foo");
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
word(I, T = char)(Word type, int minLength = 1, int maxLength = -1)
{
    final switch (type) {
        case Word.any: 
            return many(nonwhite!(I, T), minLength, maxLength);
        case Word.alnum:
            return many(alnum!(I, T), minLength, maxLength);
        case Word.alpha:
            return many(alpha!(I, T), minLength, maxLength);
    }
}
unittest
{
    string str = "foo12( bar1 ";
    auto initial = ParserState!string(str);

    auto p1 = word!string(Word.any);
    auto s1 = p1.run(initial);
    assert(s1.success);
    assert(s1.returnValue == "foo12(");

    auto p2 = word!string(Word.alnum);
    auto s2 = (whitespace!string / p2).run(s1); /* Note the 'whitespace'. */
    assert(s2.success);
    assert(s2.returnValue == "bar1");
    auto s3 = p2.run(initial);
    assert(s3.success);
    assert(s3.returnValue == "foo12");
}
