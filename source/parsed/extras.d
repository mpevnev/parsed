/* Extras module

   Provides some nice-to-have, often useful things.

   It publicly imports 'core', so there's no need to import it if you use
   extras.

   */

module parsed.extras;

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

/* ---------- misc ---------- */

/* Parses a whole line (with or without terminating newline). Always succeeds. */
auto
line(B, C = char)(bool keepTerminator)
    if (isSomeChar!C)
{
    import std.string;
    alias S = immutable(C)[];

    auto res = parseUntil!(B, C)(ch => ch == '\n' || ch == '\r', true);
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
