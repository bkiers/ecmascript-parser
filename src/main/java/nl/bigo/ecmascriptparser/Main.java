package nl.bigo.ecmascriptparser;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.util.ArrayList;
import java.util.List;

public class Main {

    public static String toStringASCII(ParseTree tree) {

        StringBuilder builder = new StringBuilder();
        walk(tree, builder);

        return builder.toString();
    }

    @SuppressWarnings("unchecked")
    private static void walk(ParseTree tree, StringBuilder builder) {

        List<ParseTree> firstStack = new ArrayList<ParseTree>();
        firstStack.add(tree);

        List<List<ParseTree>> childListStack = new ArrayList<List<ParseTree>>();
        childListStack.add(firstStack);

        while (!childListStack.isEmpty()) {

            List<ParseTree> childStack = childListStack.get(childListStack.size() - 1);

            if (childStack.isEmpty()) {
                childListStack.remove(childListStack.size() - 1);
            }
            else {
                tree = childStack.remove(0);

                String node = tree.getClass().getSimpleName().replace("Context", "");
                node = Character.toLowerCase(node.charAt(0)) + node.substring(1);

                String indent = "";

                for (int i = 0; i < childListStack.size() - 1; i++) {
                    indent += (childListStack.get(i).size() > 0) ? "|  " : "   ";
                }

                builder.append(indent)
                        .append(childStack.isEmpty() ? "'- " : "|- ")
                        .append(node.startsWith("terminal") ? tree.getText() : node)
                        .append("\n");

                if (tree.getChildCount() > 0) {
                    List<ParseTree> children = new ArrayList<ParseTree>();
                    for (int i = 0; i < tree.getChildCount(); i++) {
                        children.add(tree.getChild(i));
                    }
                    childListStack.add(children);
                }
            }
        }
    }

    public static void main(String[] args) throws Exception {

        ECMAScriptLexer lexer = new ECMAScriptLexer(new ANTLRInputStream("a = b\n/ hi / ;g.exec(c).map(d);"));
        ECMAScriptParser parser = new ECMAScriptParser(new CommonTokenStream(lexer));
        parser.addErrorListener(new DescriptiveBailErrorListener());

        ParseTree tree = parser.program();
        System.out.println(toStringASCII(tree));

//        while (true) {
//            Token token = parser.anyToken().getStart();
//            if (token.getType() == Token.EOF) {
//                break;
//            }
//            System.out.printf("%-30s%s%n", ECMAScriptParser.tokenNames[token.getType()], token.getText());
//        }
    }
}
