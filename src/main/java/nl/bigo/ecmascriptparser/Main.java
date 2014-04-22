package nl.bigo.ecmascriptparser;

import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class Main {

    private static void printRegexLiteralsDemo() {

        String script = "// comment /./m                   \n" +
                "                                          \n" +
                "var x = a / b / m; // division, no regex! \n" +
                "                                          \n" +
                "/.*?\\/\\n\\\\(?!$)/.match('some text')   \n" +
                "                                          \n" +
                "/*                                        \n" +
                "another                                   \n" +
                "/no pattern!/                             \n" +
                "comment                                   \n" +
                "*/                                        \n" +
                "                                          \n" +
                "var z = /a/m                              \n";

        ECMAScriptParser parser = new Builder.Parser(script).build();

        ParseTreeWalker.DEFAULT.walk(new ECMAScriptBaseListener(){
            @Override
            public void enterLiteral(@NotNull ECMAScriptParser.LiteralContext ctx) {
                if (ctx.RegularExpressionLiteral() != null) {
                    System.out.println("regex: " + ctx.RegularExpressionLiteral().getText());
                }
            }
        }, parser.program());
    }

    public static void main(String[] args) throws Exception {

        printRegexLiteralsDemo();
    }
}
