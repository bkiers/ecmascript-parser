package nl.bigo.ecmascriptparser;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.Test;

import static org.junit.Assert.assertTrue;

public class ECMAScriptParserTest {

    public ECMAScriptParser parse(String source) {
        ECMAScriptLexer lexer = new ECMAScriptLexer(new ANTLRInputStream(source));
        ECMAScriptParser parser = new ECMAScriptParser(new CommonTokenStream(lexer));
        parser.addErrorListener(new DescriptiveBailErrorListener());
        return parser;
    }

    private static class Container {
        public boolean parsed = false;
    }

    @Test
    public void functionExpressionTest() throws Exception {

        // Function Identifier? '(' formalParameterList? ')' '{' functionBody '}' #functionExpression
        final Container container = new Container();

        ParseTreeWalker.DEFAULT.walk(new ECMAScriptBaseListener(){
            @Override
            public void enterFunctionExpression(@NotNull ECMAScriptParser.FunctionExpressionContext ctx) {
                container.parsed = true;
            }
        } , parse("function foo() { return 42; }").singleExpression());

        assertTrue(container.parsed);
    }
}
