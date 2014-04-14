package nl.bigo.ecmascriptparser;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class Main {

    public static void main(String[] args) throws Exception {

        ECMAScriptLexer lexer = new ECMAScriptLexer(new ANTLRInputStream("try { doThat(); } catch (e) { say(e) } finally { cleanup(stuff) }"));
        ECMAScriptParser parser = new ECMAScriptParser(new CommonTokenStream(lexer));

        ParseTree tree = parser.program();
        System.out.println(tree.toStringTree(parser));

//        while (true) {
//            Token token = parser.anyToken().getStart();
//            if (token.getType() == Token.EOF) {
//                break;
//            }
//            System.out.printf("%-30s%s%n", ECMAScriptParser.tokenNames[token.getType()], token.getText());
//        }
    }
}
