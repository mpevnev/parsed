void
main()
{
    /* This example showcases parsing recursive structures. */

    /* The format for binary trees is "(value) | (left, right)". */

    string str = "((12), ((10), (45)))";

    auto tree = parse(str);
    assert(tree !is null);
    tree.print;
}

class BinaryTree
{
    enum NodeType
    {
        leaf,
        fork
    }

    NodeType type;
    union
    {
        int value;
        struct {
            BinaryTree left;
            BinaryTree right;
        }
    }

    this(int val)
    {
        type = NodeType.leaf;
        value = val;
    }

    this(BinaryTree left, BinaryTree right)
    {
        type = NodeType.fork;
        this.left = left;
        this.right = right;
    }

    void print(int depth = 0)
    {
        import std.stdio;

        final switch (type) {
            case NodeType.leaf: 
                foreach (i; 0 .. depth) write("-");
                writeln(value);
                break;
            case NodeType.fork:
                foreach (i; 0 .. depth) write("-");
                writeln("|");
                left.print(depth + 2);
                right.print(depth + 2);
                break;
        }
    }
}

BinaryTree
parse(string s)
{
    import std.conv;

    import parsed.extras;

    auto block = balanced!string('(', ')') % (res, s) => s;
    auto state = ParserState!string(s);
    auto res = block.run(state);

    if (!res.success) return null;
    alias F = string[];
    auto fork = 
        balanced!F('(', ')', true) % ((res, s) => res ~ s)
        / literal!F(",")
        / many(-1, -1, whitespace!F)
        / balanced!F('(', ')', true) % (res, s) => res ~ s;
    auto forkstate = ParserState!F(res.parsed);
    auto forked = fork.run(forkstate);
    if (forked.success) {
        /* This is a fork. */
        auto left = parse(forked.value[0]);
        auto right = parse(forked.value[1]);
        if (left is null || right is null) return null;
        return new BinaryTree(left, right);
    } else {
        /* This is either a leaf or an invalid tree. */
        auto leaf = number!int % (res, s) => to!int(s);
        auto leafstate = ParserState!int(res.parsed);
        auto leafed = leaf.run(leafstate);
        if (leafed.success) {
            /* It is a leaf all right. */
            return new BinaryTree(leafed.value);
        } else {
            /* Invalid tree. */
            return null;
        }
    }
}

bool hasComma(string s)
{
    import std.string;
    return s.count(',') > 0;
}
