
enum Gender
{
    male,
    female
}

struct Person
{
    string name;
    int age;
    Gender gender;
}

void main()
{
    /* This example showcases parsing a string into an array of structs. */

    import std.conv;
    import std.stdio;

    import parsed.extras;

    string toParse = "Andrew male 18\nBoris male 22\nCaroline female 21\n";
    alias Out = Person[];
    auto state = ParserState!Out(toParse);

    /* Gender parser. */
    alias G = Gender;
    auto gender = (literal!G("male") | literal!G("female"))
        % (g, s) => s == "male" ? G.male : G.female;

    /* A parser for a single person. */
    alias P = Person;
    auto person = 
        word!P(Word.alpha) 
        % (delegate (P p, string s) { p.name = s; return p; })
        / whitespace!P
        / absorb!(P, G)(
                delegate (P p, G g, string s) { p.gender = g; return p; },
                gender)
        / whitespace!P
        / number!P
        % (delegate (P p, string s) { p.age = to!int(s); return p; })
        / maybe(newline!P);
    auto res1 = person.run(ParserState!P(toParse));

    auto total = many(3, -1,
            absorb!(Out, Person)(
                (array, p, s) => array ~ p,
                person));
    auto res = total.run(state);

    writeln("---");
    foreach (p; res.value) {
        writeln("Name: ", p.name);
        writeln("Gender: ", p.gender);
        writeln("Age: ", p.age);
        writeln("---");
    }
}
