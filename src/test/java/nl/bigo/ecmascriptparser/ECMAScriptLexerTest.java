package nl.bigo.ecmascriptparser;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;
import org.junit.Test;

import static nl.bigo.ecmascriptparser.ECMAScriptLexer.*;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Unit tests for the lexer.
 */
public class ECMAScriptLexerTest {

    /**
     * Tokenizes {@code source} as a single token. There will
     * be an assertion error when there is more than 1 token
     * available in {@code source}.
     *
     * @param source
     *         a single token.
     * @param strictMode
     *         if the parser should parse in strict mode or not.
     *
     * @return the type of the token {@code source} represents.
     */
    private Integer tokenize(String source, boolean strictMode) {

        ECMAScriptLexer lexer = new ECMAScriptLexer(new ANTLRInputStream(source));
        lexer.setStrictMode(strictMode);
        CommonTokenStream tokens = new CommonTokenStream(lexer);

        // Get the first token.
        Token token = tokens.LT(1);

        // Make sure the second token is EOF.
        assertThat(tokens.LT(2).getType(), is(EOF));

        return token == null ? null : token.getType();
    }

    /**
     * Executes tests in strictMode.
     *
     * @param tests
     *         the tests to execute.
     */
    private void test(Object[][] tests) {
        test(tests, true);
    }

    /**
     * Executes tests.
     *
     * @param tests
     *         the tests to execute.
     * @param strictMode
     *         whether the tests should run in strict mode or not.
     */
    private void test(Object[][] tests, boolean strictMode) {

        for (Object[] test : tests) {

            String input = (String) test[0];
            Integer expected = (Integer) test[1];

            Integer actual = tokenize(input, strictMode);

            String error = String.format("expected \"%s\" to be tokenized as a `%s`, but got `%s` instead",
                    input, ruleNames[expected - 1], ruleNames[actual - 1]);

            assertThat(error, expected, is(actual));
        }
    }

    @Test
    public void unexpectedCharacterTest() {

        test(new Object[][]{
                new Object[]{"@", UnexpectedCharacter},
                new Object[]{"#", UnexpectedCharacter},
        });
    }

    @Test
    public void identifierTest() {

        test(new Object[][]{
                new Object[]{"a123", Identifier},
                new Object[]{"ಠ_ಠ", Identifier},
                new Object[]{"\\u006C\\u006F\\u006C\\u0077\\u0061\\u0074", Identifier},
        });
    }

    @Test
    public void hiddenTokensTest() {

        test(new Object[][]{
                new Object[]{"    /* spaces should be hidden */\t123", DecimalLiteral},
                new Object[]{"// comments should be hidden\n123", DecimalLiteral},
                new Object[]{"/* comments should be hidden */123", DecimalLiteral},
                new Object[]{"\n/* line breaks should be hidden */\n123", DecimalLiteral},
        });
    }

    @Test
    public void stringLiteralTest() {

        test(new Object[][]{
                new Object[]{"\"\"", StringLiteral},
                new Object[]{"\"   \\\"   \"", StringLiteral},
                new Object[]{"\"   \\\" \\n  \"", StringLiteral},
                new Object[]{"''", StringLiteral},
                new Object[]{"'   \\'   '", StringLiteral},
                new Object[]{"'   \\' \\n  '", StringLiteral},
        });
    }

    @Test
    public void keywordsTest() {

        test(new Object[][]{
                new Object[]{"break", Break},
                new Object[]{"do", Do},
                new Object[]{"instanceof", Instanceof},
                new Object[]{"typeof", Typeof},
                new Object[]{"case", Case},
                new Object[]{"else", Else},
                new Object[]{"new", New},
                new Object[]{"var", Var},
                new Object[]{"catch", Catch},
                new Object[]{"finally", Finally},
                new Object[]{"return", Return},
                new Object[]{"void", Void},
                new Object[]{"continue", Continue},
                new Object[]{"for", For},
                new Object[]{"switch", Switch},
                new Object[]{"while", While},
                new Object[]{"debugger", Debugger},
                new Object[]{"function", Function},
                new Object[]{"this", This},
                new Object[]{"with", With},
                new Object[]{"default", Default},
                new Object[]{"if", If},
                new Object[]{"throw", Throw},
                new Object[]{"delete", Delete},
                new Object[]{"in", In},
                new Object[]{"try", Try},
                new Object[]{"class", Class},
                new Object[]{"enum", Enum},
                new Object[]{"extends", Extends},
                new Object[]{"super", Super},
                new Object[]{"const", Const},
                new Object[]{"export", Export},
                new Object[]{"import", Import},
                new Object[]{"implements", Implements},
                new Object[]{"let", Let},
                new Object[]{"private", Private},
                new Object[]{"public", Public},
                new Object[]{"interface", Interface},
                new Object[]{"package", Package},
                new Object[]{"protected", Protected},
                new Object[]{"static", Static},
                new Object[]{"yield", Yield},
        });
    }

    @Test
    public void decimalLiteralTest() {

        test(new Object[][]{
                new Object[]{"123", DecimalLiteral},
                new Object[]{"123.3", DecimalLiteral},
                new Object[]{".3", DecimalLiteral},
                new Object[]{"3.", DecimalLiteral},
                new Object[]{"123e+100", DecimalLiteral},
                new Object[]{"123.3E+100", DecimalLiteral},
                new Object[]{".3e-100", DecimalLiteral},
                new Object[]{"3.e100", DecimalLiteral},
        });
    }

    @Test
    public void octalIntegerLiteralTest() {

        test(new Object[][]{
                new Object[]{"07", OctalIntegerLiteral},
        }, false); // OctalIntegerLiteral is only available when strictMode is disabled.
    }

    @Test
    public void hexIntegerLiteralTest() {

        test(new Object[][]{
                new Object[]{"0x7", HexIntegerLiteral},
                new Object[]{"0X7", HexIntegerLiteral},
                new Object[]{"0xffabc", HexIntegerLiteral},
        });
    }

    @Test
    public void booleanLiteralTest() {

        test(new Object[][]{
                new Object[]{"true", BooleanLiteral},
                new Object[]{"false", BooleanLiteral},
        });
    }

    @Test
    public void nullLiteralTest() {

        test(new Object[][]{
                new Object[]{"null", NullLiteral},
        });
    }

    @Test
    public void operatorAndOtherCharactersTest() {

        test(new Object[][]{
                new Object[]{"[", OpenBracket},
                new Object[]{"]", CloseBracket},
                new Object[]{"(", OpenParen},
                new Object[]{")", CloseParen},
                new Object[]{"{", OpenBrace},
                new Object[]{"}", CloseBrace},
                new Object[]{";", SemiColon},
                new Object[]{",", Comma},
                new Object[]{"=", Assign},
                new Object[]{"?", QuestionMark},
                new Object[]{":", Colon},
                new Object[]{".", Dot},
                new Object[]{"++", PlusPlus},
                new Object[]{"--", MinusMinus},
                new Object[]{"+", Plus},
                new Object[]{"-", Minus},
                new Object[]{"~", BitNot},
                new Object[]{"!", Not},
                new Object[]{"*", Multiply},
                new Object[]{"/", Divide},
                new Object[]{"%", Modulus},
                new Object[]{">>", RightShiftArithmetic},
                new Object[]{"<<", LeftShiftArithmetic},
                new Object[]{">>>", RightShiftLogical},
                new Object[]{"<", LessThan},
                new Object[]{">", MoreThan},
                new Object[]{"<=", LessThanEquals},
                new Object[]{">=", GreaterThanEquals},
                new Object[]{"==", Equals},
                new Object[]{"!=", NotEquals},
                new Object[]{"===", IdentityEquals},
                new Object[]{"!==", IdentityNotEquals},
                new Object[]{"&", BitAnd},
                new Object[]{"^", BitXOr},
                new Object[]{"|", BitOr},
                new Object[]{"&&", And},
                new Object[]{"||", Or},
                new Object[]{"*=", MultiplyAssign},
                new Object[]{"/=", DivideAssign},
                new Object[]{"%=", ModulusAssign},
                new Object[]{"+=", PlusAssign},
                new Object[]{"-=", MinusAssign},
                new Object[]{"<<=", LeftShiftArithmeticAssign},
                new Object[]{">>=", RightShiftArithmeticAssign},
                new Object[]{">>>=", RightShiftLogicalAssign},
                new Object[]{"&=", BitAndAssign},
                new Object[]{"^=", BitXorAssign},
                new Object[]{"|=", BitOrAssign},
        });
    }

    @Test
    public void regularExpressionLiteralTest() {

        test(new Object[][]{
                new Object[]{"/./", RegularExpressionLiteral},
                new Object[]{"/\\/(?!=.)$/gmi", RegularExpressionLiteral},
        });
    }
}
