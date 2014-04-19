package nl.bigo.ecmascriptparser;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.junit.Test;

import java.lang.reflect.Method;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

public class ECMAScriptParserTest {

    private void test(String source, String rule) throws Exception {
        this.test(source, rule, true);
    }

    /**
     * Tests some provided input source by letting it be parsed by a
     * specific parser rule.
     *
     * @param source
     *         the input to parse.
     * @param rule
     *         the parser rule to invoke to get the `source` to be parsed.
     */
    private void test(String source, String rule, boolean strictMode) throws Exception {
        ECMAScriptLexer lexer = new ECMAScriptLexer(new ANTLRInputStream(source));
        lexer.setStrictMode(strictMode);
        ECMAScriptParser parser = new ECMAScriptParser(new CommonTokenStream(lexer));
        parser.addErrorListener(new DescriptiveBailErrorListener());
        Method method = ECMAScriptParser.class.getDeclaredMethod(rule, new Class[]{});
        ParserRuleContext context = (ParserRuleContext) method.invoke(parser);
        assertThat(context, is(not(nullValue())));
        parser.eof();
    }

    @Test
    public void expressionTest() throws Exception {

        final String rule = "expression";

        /// Expression :
        ///     AssignmentExpression
        ///     Expression , AssignmentExpression
        test("a = b, this, foo()", rule);

        /// AssignmentExpression :
        ///     ConditionalExpression
        ///     LeftHandSideExpression = AssignmentExpression
        ///     LeftHandSideExpression AssignmentOperator AssignmentExpression
        ///
        /// AssignmentOperator : one of
        ///     *=	/=	%=	+=	-=	<<=	>>=	>>>=	&=	^=	|=
        test("a = b", rule);
        test("a *= b", rule);
        test("a /= b", rule);
        test("a %= b", rule);
        test("a += b", rule);
        test("a -= b", rule);
        test("a <<= b", rule);
        test("a >>= b", rule);
        test("a >>>= b", rule);
        test("a &= b", rule);
        test("a ^= b", rule);
        test("a |= b", rule);

        /// ConditionalExpression :
        ///     LogicalORExpression
        ///     LogicalORExpression ? AssignmentExpression : AssignmentExpression
        test("a ? b : c", rule);

        /// LogicalORExpression :
        ///     LogicalANDExpression
        ///     LogicalORExpression || LogicalANDExpression
        test("a || b", rule);

        /// LogicalANDExpression :
        ///     BitwiseORExpression
        ///     LogicalANDExpression && BitwiseORExpression
        test("a && b", rule);

        /// BitwiseORExpression :
        ///     BitwiseXORExpression
        ///     BitwiseORExpression | BitwiseXORExpression
        test("a | b", rule);

        /// BitwiseXORExpression :
        ///     BitwiseANDExpression
        ///     BitwiseXORExpression ^ BitwiseANDExpression
        test("a ^ b", rule);

        /// BitwiseANDExpression :
        ///     EqualityExpression
        ///     BitwiseANDExpression & EqualityExpression
        test("a & b", rule);

        /// EqualityExpression :
        ///     RelationalExpression
        ///     EqualityExpression == RelationalExpression
        ///     EqualityExpression != RelationalExpression
        ///     EqualityExpression === RelationalExpression
        ///     EqualityExpression !== RelationalExpression
        test("a == b", rule);
        test("a != b", rule);
        test("a === b", rule);
        test("a!==b", rule);

        /// RelationalExpression :
        ///     ShiftExpression
        ///     RelationalExpression < ShiftExpression
        ///     RelationalExpression > ShiftExpression
        ///     RelationalExpression <= ShiftExpression
        ///     RelationalExpression >= ShiftExpression
        ///     RelationalExpression instanceof ShiftExpression
        ///     RelationalExpression in ShiftExpression
        test("a < b", rule);
        test("a > b", rule);
        test("a <= b", rule);
        test("a >= b", rule);
        test("a instanceof b", rule);
        test("a in b", rule);

        /// ShiftExpression :
        ///     AdditiveExpression
        ///     ShiftExpression << AdditiveExpression
        ///     ShiftExpression >> AdditiveExpression
        ///     ShiftExpression >>> AdditiveExpression
        test("a << b", rule);
        test("a >> b", rule);
        test("a >>> b", rule);

        /// AdditiveExpression :
        ///     MultiplicativeExpression
        ///     AdditiveExpression + MultiplicativeExpression
        ///     AdditiveExpression - MultiplicativeExpression
        test("a + b", rule);
        test("a - b + c - d", rule);

        /// MultiplicativeExpression :
        ///     UnaryExpression
        ///     MultiplicativeExpression * UnaryExpression
        ///     MultiplicativeExpression / UnaryExpression
        ///     MultiplicativeExpression % UnaryExpression
        test("a * b", rule);
        test("a / b / c", rule);
        test("a % b", rule);

        /// UnaryExpression :
        ///     PostfixExpression
        ///     delete UnaryExpression
        ///     void UnaryExpression
        ///     typeof UnaryExpression
        ///     ++ UnaryExpression
        ///     -- UnaryExpression
        ///     + UnaryExpression
        ///     - UnaryExpression
        ///     ~ UnaryExpression
        ///     ! UnaryExpression
        test("delete 1", rule);
        test("void x", rule);
        test("typeof s", rule);
        test("++z", rule);
        test("--z", rule);
        test("+z", rule);
        test("-z", rule);
        test("~z", rule);
        test("!z", rule);

        /// PostfixExpression :
        ///     LeftHandSideExpression
        ///     LeftHandSideExpression [no LineTerminator here] ++
        ///     LeftHandSideExpression [no LineTerminator here] --
        test("x++", rule);
        test("x--", rule);
        test("bar()++", rule);

        /// LeftHandSideExpression :
        ///     NewExpression
        ///     CallExpression
        ///
        /// CallExpression :
        ///     MemberExpression Arguments
        ///     CallExpression Arguments
        ///     CallExpression [ Expression ]
        ///     CallExpression . IdentifierName
        test("foo()()", rule);
        test("foo()[123]", rule);
        test("bar().x", rule);

        /// NewExpression :
        ///     MemberExpression
        ///     new NewExpression
        test("new new X()", rule);

        /// MemberExpression :
        ///     PrimaryExpression
        ///     FunctionExpression
        ///     MemberExpression [ Expression ]
        ///     MemberExpression . IdentifierName
        ///     new MemberExpression Arguments
        test("array[0]", rule);
        test("object.property", rule);
        test("new Something(1, 2, 3)", rule);

        /// FunctionExpression :
        ///     function Identifier? ( FormalParameterList? ) { FunctionBody }
        test("function mu() { /* something */ }", rule);
        test("function () { return x; }", rule);

        /// PrimaryExpression :
        ///     this
        ///     Identifier
        ///     Literal
        ///     ArrayLiteral
        ///     ObjectLiteral
        ///     ( Expression )
        test("this", rule);
        test("foo", rule);
        test("\"str\"", rule);
        test("'x\\ny\\tz'", rule);
        test("null", rule);
        test("[1, 2, 3, 4]", rule);
        test("{\"map\":{\"bool\":true}}", rule);
        test("( 3.14159265 )", rule);
    }

    @Test
    public void assignmentOperatorTest() throws Exception {

        final String rule = "assignmentOperator";

        /// AssignmentOperator : one of
        ///     *=	/=	%=	+=	-=	<<=	>>=	>>>=	&=	^=	|=
        test("*=", rule);
        test("/=", rule);
        test("%=", rule);
        test("+=", rule);
        test("-=", rule);
        test("<<=", rule);
        test(">>=", rule);
        test(">>>=", rule);
        test("&=", rule);
        test("^=", rule);
        test("|=", rule);
    }

    @Test
    public void literalTest() throws Exception {

        final String rule = "literal";

        // literal
        //  : ( NullLiteral
        //    | BooleanLiteral
        //    | StringLiteral
        //    | RegularExpressionLiteral
        //    )
        //  | numericLiteral
        //  ;
        test("null", rule);
    }

    @Test
    public void numericLiteralTest() throws Exception {

        final String rule = "numericLiteral";

        //  numericLiteral
        //   : DecimalLiteral
        //   | HexIntegerLiteral
        //   | OctalIntegerLiteral
        //   ;
        test("10", rule);
        test("0x10", rule);
        test("0XFFeAb49", rule);
        test("077", rule, false);
    }
}
