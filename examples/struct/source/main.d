
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

    import parsed.extras;

    string toParse = "Andrew male 18\nBoris male 22\nCaroline female 21\n";
    auto state = ParserState!string(toParse);

    /* Gender parser. */
    auto gender = (literal("male") | literal("female"))
        / fork!string(s => s == "male", Gender.male, Gender.female);

    /* A parser for a single person. */

}
