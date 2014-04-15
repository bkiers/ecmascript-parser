package nl.bigo.ecmascriptparser;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;

public class ECMAScriptLexerTest {

    private static Token parseAnyToken(String source) {
        ECMAScriptLexer lexer = new ECMAScriptLexer(new ANTLRInputStream(source));
        ECMAScriptParser parser = new ECMAScriptParser(new CommonTokenStream(lexer));
        parser.anyToken();
        return null;
    }

    //@Test
    public void test() {
        System.out.println(parseAnyToken("123"));
    }
}
