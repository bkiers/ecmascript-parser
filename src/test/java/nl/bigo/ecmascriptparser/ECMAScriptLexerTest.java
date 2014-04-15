package nl.bigo.ecmascriptparser;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;
import org.junit.Test;

import static nl.bigo.ecmascriptparser.ECMAScriptLexer.*;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

public class ECMAScriptLexerTest {

    /**
     * Tokenizes {@code source} as a single token. There will
     * be an assertion error when there is more than 1 token
     * available in {@code source}.
     *
     * @param source
     *         a single token.
     *
     * @return the type of the token {@code source} represents.
     */
    private Integer tokenize(String source) {
        ECMAScriptLexer lexer = new ECMAScriptLexer(new ANTLRInputStream(source));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        Token token = tokens.LT(1);
        assertThat(tokens.LT(2).getType(), is(EOF));
        return token == null ? null : token.getType();
    }

    @Test
    public void lexerTests() {

        Object[][] tests = {
                new Object[]{"/", Divide},
                new Object[]{"/123/g", RegularExpressionLiteral},
                new Object[]{"123", DecimalLiteral},
                new Object[]{"a123", Identifier},
                new Object[]{"@", UnexpectedCharacter},
        };

        for (Object[] test : tests) {

            String input = (String) test[0];
            Integer expected = (Integer) test[1];

            Integer actual = tokenize(input);

            String error = String.format("expected \"%s\" to be tokenized as a `%s`, but got `%s` instead",
                    input, tokenNames[expected], tokenNames[actual]);

            assertThat(error, expected, is(actual));
        }
    }
}
