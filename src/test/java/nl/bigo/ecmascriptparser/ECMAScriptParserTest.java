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

    private void test(String[] tests, String rule) throws Exception {
        this.test(tests, rule, true);
    }

    private void test(String[] tests, String rule, boolean strictMode) throws Exception {
        for (String test : tests) {
            this.test(test, rule, strictMode);
        }
    }

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

    // program
    //  : sourceElements? EOF
    //  ;
    @Test
    public void programTest() throws Exception {

        final String rule = "program";

        this.test(new String[]{}, rule);
    }

    // sourceElements
    //  : sourceElement+
    //  ;
    @Test
    public void sourceElementsTest() throws Exception {

        final String rule = "sourceElements";

        this.test(new String[]{}, rule);
    }

    // sourceElement
    //  : statement
    //  | functionDeclaration
    //  ;
    @Test
    public void sourceElementTest() throws Exception {

        final String rule = "sourceElement";

        this.test(new String[]{}, rule);
    }

    // statement
    //  : block
    //  | variableStatement
    //  | emptyStatement
    //  | expressionStatement
    //  | ifStatement
    //  | iterationStatement
    //  | continueStatement
    //  | breakStatement
    //  | returnStatement
    //  | withStatement
    //  | labelledStatement
    //  | switchStatement
    //  | throwStatement
    //  | tryStatement
    //  | debuggerStatement
    //  ;
    @Test
    public void statementTest() throws Exception {

        final String rule = "statement";

        this.test(new String[]{}, rule);
    }

    // block
    //  : '{' statementList? '}'
    //  ;
    @Test
    public void blockTest() throws Exception {

        final String rule = "block";

        this.test(new String[]{}, rule);
    }

    // statementList
    //  : statement+
    //  ;
    @Test
    public void statementListTest() throws Exception {

        final String rule = "statementList";

        this.test(new String[]{}, rule);
    }

    // variableStatement
    //  : Var variableDeclarationList eos
    //  ;
    @Test
    public void variableStatementTest() throws Exception {

        final String rule = "variableStatement";

        this.test(new String[]{}, rule);
    }

    // variableDeclarationList
    //  : variableDeclaration ( ',' variableDeclaration )*
    //  ;
    @Test
    public void variableDeclarationListTest() throws Exception {

        final String rule = "variableDeclarationList";

        this.test(new String[]{}, rule);
    }

    // variableDeclaration
    //  : Identifier initialiser?
    //  ;
    @Test
    public void variableDeclarationTest() throws Exception {

        final String rule = "variableDeclaration";

        this.test(new String[]{}, rule);
    }

    // initialiser
    //  : '=' singleExpression
    //  ;
    @Test
    public void initialiserTest() throws Exception {

        final String rule = "initialiser";

        this.test(new String[]{}, rule);
    }

    // emptyStatement
    //  : SemiColon
    //  ;
    @Test
    public void emptyStatementTest() throws Exception {

        final String rule = "emptyStatement";

        this.test(new String[]{}, rule);
    }

    // expressionStatement
    //  : expressionSequence
    //  ;
    @Test
    public void expressionStatementTest() throws Exception {

        final String rule = "expressionStatement";

        this.test(new String[]{}, rule);
    }

    // ifStatement
    //  : If '(' expressionSequence ')' statement ( Else statement )?
    //  ;
    @Test
    public void ifStatementTest() throws Exception {

        final String rule = "ifStatement";

        this.test(new String[]{}, rule);
    }

    // iterationStatement
    //  : Do statement While '(' expressionSequence ')' eos
    //  | While '(' expressionSequence ')' statement
    //  | For '(' expressionSequence? ';' expressionSequence? ';' expressionSequence? ')' statement
    //  | For '(' Var variableDeclarationList ';' expressionSequence? ';' expressionSequence? ')' statement
    //  | For '(' singleExpression In expressionSequence ')' statement
    //  | For '(' Var variableDeclaration In expressionSequence ')' statement
    //  ;
    @Test
    public void iterationStatementTest() throws Exception {

        final String rule = "iterationStatement";

        this.test(new String[]{}, rule);
    }

    // continueStatement
    //  : Continue Identifier? eos
    //  ;
    @Test
    public void continueStatementTest() throws Exception {

        final String rule = "continueStatement";

        this.test(new String[]{}, rule);
    }

    // breakStatement
    //  : Break Identifier? eos
    //  ;
    @Test
    public void breakStatementTest() throws Exception {

        final String rule = "breakStatement";

        this.test(new String[]{}, rule);
    }

    // returnStatement
    //  : Return expressionSequence? eos
    //  ;
    @Test
    public void returnStatementTest() throws Exception {

        final String rule = "returnStatement";

        this.test(new String[]{}, rule);
    }

    // withStatement
    //  : With '(' expressionSequence ')' statement
    //  ;
    @Test
    public void withStatementTest() throws Exception {

        final String rule = "withStatement";

        this.test(new String[]{}, rule);
    }

    // switchStatement
    //  : Switch '(' expressionSequence ')' caseBlock
    //  ;
    @Test
    public void switchStatementTest() throws Exception {

        final String rule = "switchStatement";

        this.test(new String[]{}, rule);
    }

    // caseBlock
    //  : '{' caseClauses? ( defaultClause caseClauses? )? '}'
    //  ;
    @Test
    public void caseBlockTest() throws Exception {

        final String rule = "caseBlock";

        this.test(new String[]{}, rule);
    }

    // caseClauses
    //  : caseClause+
    //  ;
    @Test
    public void caseClausesTest() throws Exception {

        final String rule = "caseClauses";

        this.test(new String[]{}, rule);
    }

    // caseClause
    //  : Case expressionSequence ':' statementList?
    //  ;
    @Test
    public void caseClauseTest() throws Exception {

        final String rule = "caseClause";

        this.test(new String[]{}, rule);
    }

    // defaultClause
    //  : Default ':' statementList?
    //  ;
    @Test
    public void defaultClauseTest() throws Exception {

        final String rule = "defaultClause";

        this.test(new String[]{}, rule);
    }

    // labelledStatement
    //  : Identifier ':' statement
    //  ;
    @Test
    public void labelledStatementTest() throws Exception {

        final String rule = "labelledStatement";

        this.test(new String[]{}, rule);
    }

    // throwStatement
    //  : Throw expressionSequence eos
    //  ;
    @Test
    public void throwStatementTest() throws Exception {

        final String rule = "throwStatement";

        this.test(new String[]{}, rule);
    }

    // tryStatement
    //  : Try block catchProduction
    //  | Try block finallyProduction
    //  | Try block catchProduction finallyProduction
    //  ;
    @Test
    public void tryStatementTest() throws Exception {

        final String rule = "tryStatement";

        this.test(new String[]{}, rule);
    }

    // catchProduction
    //  : Catch '(' Identifier ')' block
    //  ;
    @Test
    public void catchProductionTest() throws Exception {

        final String rule = "catchProduction";

        this.test(new String[]{}, rule);
    }

    // finallyProduction
    //  : Finally block
    //  ;
    @Test
    public void finallyProductionTest() throws Exception {

        final String rule = "finallyProduction";

        this.test(new String[]{}, rule);
    }

    // debuggerStatement
    //  : Debugger eos
    //  ;
    @Test
    public void debuggerStatementTest() throws Exception {

        final String rule = "debuggerStatement";

        this.test(new String[]{}, rule);
    }

    // functionDeclaration
    //  : Function Identifier '(' formalParameterList? ')' '{' functionBody '}'
    //  ;
    @Test
    public void functionDeclarationTest() throws Exception {

        final String rule = "functionDeclaration";

        this.test(new String[]{}, rule);
    }

    // formalParameterList
    //  : Identifier ( ',' Identifier )*
    //  ;
    @Test
    public void formalParameterListTest() throws Exception {

        final String rule = "formalParameterList";

        this.test(new String[]{}, rule);
    }

    // functionBody
    //  : sourceElements?
    //  ;
    @Test
    public void functionBodyTest() throws Exception {

        final String rule = "functionBody";

        this.test(new String[]{}, rule);
    }

    // arrayLiteral
    //  : '[' elementList? ','? elision? ']'
    //  ;
    @Test
    public void arrayLiteralTest() throws Exception {

        final String rule = "arrayLiteral";

        this.test(new String[]{}, rule);
    }

    // elementList
    //  : elision? singleExpression ( ',' elision? singleExpression )*
    //  ;
    @Test
    public void elementListTest() throws Exception {

        final String rule = "elementList";

        this.test(new String[]{}, rule);
    }

    // elision
    //  : ','+
    //  ;
    @Test
    public void elisionTest() throws Exception {

        final String rule = "elision";

        this.test(new String[]{}, rule);
    }

    // objectLiteral
    //  : '{' propertyNameAndValueList? ','? '}'
    //  ;
    @Test
    public void objectLiteralTest() throws Exception {

        final String rule = "objectLiteral";

        this.test(new String[]{}, rule);
    }

    // propertyNameAndValueList
    //  : propertyAssignment ( ',' propertyAssignment )*
    //  ;
    @Test
    public void propertyNameAndValueListTest() throws Exception {

        final String rule = "propertyNameAndValueList";

        this.test(new String[]{}, rule);
    }

    // propertyAssignment
    //  : propertyName ':' singleExpression
    //  | getter '(' ')' '{' functionBody '}'
    //  | setter '(' propertySetParameterList ')' '{' functionBody '}'
    //  ;           
    @Test
    public void propertyAssignmentTest() throws Exception {

        final String rule = "propertyAssignment";

        this.test(new String[]{}, rule);
    }

    // propertyName
    //  : identifierName
    //  | StringLiteral
    //  | numericLiteral
    //  ;
    @Test
    public void propertyNameTest() throws Exception {

        final String rule = "propertyName";

        this.test(new String[]{}, rule);
    }

    // propertySetParameterList
    //  : Identifier
    //  ;
    @Test
    public void propertySetParameterListTest() throws Exception {

        final String rule = "propertySetParameterList";

        this.test(new String[]{}, rule);
    }

    // arguments
    //  : '(' argumentList? ')'
    //  ;
    @Test
    public void argumentsTest() throws Exception {

        final String rule = "arguments";

        this.test(new String[]{}, rule);
    }

    // argumentList
    //  : singleExpression ( ',' singleExpression )*
    //  ;
    @Test
    public void argumentListTest() throws Exception {

        final String rule = "argumentList";

        this.test(new String[]{}, rule);
    }

    // expressionSequence
    //  : singleExpression ( ',' singleExpression )*
    //  ;
    @Test
    public void expressionSequenceTest() throws Exception {

        final String rule = "expressionSequence";

        this.test(new String[]{}, rule);
    }

    // singleExpression
    //  : Function Identifier? '(' formalParameterList? ')' '{' functionBody '}'
    //  | singleExpression '[' expressionSequence ']'
    //  | singleExpression '.' identifierName
    //  | singleExpression arguments
    //  | New singleExpression arguments?
    //  | singleExpression {!here(LineTerminator)}? '++'
    //  | singleExpression {!here(LineTerminator)}? '--'
    //  | Delete singleExpression
    //  | Void singleExpression
    //  | Typeof singleExpression
    //  | '++' singleExpression
    //  | '--' singleExpression
    //  | '+' singleExpression
    //  | '-' singleExpression
    //  | '~' singleExpression
    //  | '!' singleExpression
    //  | singleExpression '*' singleExpression
    //  | singleExpression '/' singleExpression
    //  | singleExpression '%' singleExpression
    //  | singleExpression '+' singleExpression
    //  | singleExpression '-' singleExpression
    //  | singleExpression '<<' singleExpression
    //  | singleExpression '>>' singleExpression
    //  | singleExpression '>>>' singleExpression
    //  | singleExpression '<' singleExpression
    //  | singleExpression '>' singleExpression
    //  | singleExpression '<=' singleExpression
    //  | singleExpression '>=' singleExpression
    //  | singleExpression Instanceof singleExpression
    //  | singleExpression In singleExpression
    //  | singleExpression '==' singleExpression
    //  | singleExpression '!=' singleExpression
    //  | singleExpression '===' singleExpression
    //  | singleExpression '!==' singleExpression
    //  | singleExpression '&' singleExpression
    //  | singleExpression '^' singleExpression
    //  | singleExpression '|' singleExpression
    //  | singleExpression '&&' singleExpression
    //  | singleExpression '||' singleExpression
    //  | singleExpression '?' singleExpression ':' singleExpression
    //  | singleExpression '=' expressionSequence
    //  | singleExpression assignmentOperator expressionSequence
    //  | This
    //  | Identifier
    //  | literal
    //  | arrayLiteral
    //  | objectLiteral
    //  | '(' expressionSequence ')'
    //  ;
    @Test
    public void singleExpressionTest() throws Exception {

        final String rule = "singleExpression";

        test("a = b, this, foo()", rule);

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

        test("a ? b : c", rule);

        test("a || b", rule);
        test("a && b", rule);
        test("a | b", rule);
        test("a ^ b", rule);
        test("a & b", rule);

        test("a == b", rule);
        test("a != b", rule);
        test("a === b", rule);
        test("a!==b", rule);

        test("a < b", rule);
        test("a > b", rule);
        test("a <= b", rule);
        test("a >= b", rule);
        test("a instanceof b", rule);
        test("a in b", rule);

        test("a << b", rule);
        test("a >> b", rule);
        test("a >>> b", rule);

        test("a + b", rule);
        test("a - b + c - d", rule);

        test("a * b", rule);
        test("a / b / c", rule);
        test("a % b", rule);

        test("delete 1", rule);
        test("void x", rule);
        test("typeof s", rule);
        test("++z", rule);
        test("--z", rule);
        test("+z", rule);
        test("-z", rule);
        test("~z", rule);
        test("!z", rule);

        test("x++", rule);
        test("x--", rule);
        test("bar()++", rule);

        test("foo()()", rule);
        test("foo()[123]", rule);
        test("bar().x", rule);

        test("new new X()", rule);

        test("array[0]", rule);
        test("object.property", rule);
        test("new Something(1, 2, 3)", rule);

        test("function mu() { /* something */ }", rule);
        test("function () { return x; }", rule);

        test("this", rule);
        test("foo", rule);
        test("\"str\"", rule);
        test("'x\\ny\\tz'", rule);
        test("null", rule);
        test("[1, 2, 3, 4]", rule);
        test("{\"map\":{\"bool\":true}}", rule);
        test("( 3.14159265 )", rule);
    }

    // assignmentOperator
    //  : '*='
    //  | '/='
    //  | '%='
    //  | '+='
    //  | '-='
    //  | '<<='
    //  | '>>='
    //  | '>>>='
    //  | '&='
    //  | '^='
    //  | '|='
    //  ;
    @Test
    public void assignmentOperatorTest() throws Exception {

        final String rule = "assignmentOperator";

        this.test(new String[]{
                        "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", ">>>=", "&=", "^=", "|="
                },
                rule
        );
    }

    // literal
    //  : ( NullLiteral
    //    | BooleanLiteral
    //    | StringLiteral
    //    | RegularExpressionLiteral
    //    )
    //  | numericLiteral
    //  ;
    @Test
    public void literalTest() throws Exception {

        final String rule = "literal";

        this.test(new String[]{
                        "null", "true", "false", "\"foo\"", "'\\n'", "/./", "/\\/\\n/mi"
                },
                rule
        );
    }

    // numericLiteral
    //  : DecimalLiteral
    //  | HexIntegerLiteral
    //  | OctalIntegerLiteral
    //  ;
    @Test
    public void numericLiteralTest() throws Exception {

        final String rule = "numericLiteral";

        this.test(new String[]{
                        "10", "0x10", "0XFFeAb49"
                },
                rule
        );

        this.test("077", rule, false);
    }

    // identifierName
    //  : Identifier
    //  | reservedWord
    //  ;
    @Test
    public void identifierNameTest() throws Exception {

        final String rule = "identifierName";

        this.test(new String[]{
                        "_mu", "foo", "BAR",
                        "break", "do", "instanceof", "typeof", "case", "else", "new", "var", "catch", "finally",
                        "return", "void", "continue", "for", "switch", "while", "debugger", "function", "this",
                        "with", "default", "if", "throw", "delete", "in", "try", "class", "enum", "extends",
                        "super", "const", "export", "import", "implements", "let", "private", "public", "interface",
                        "package", "protected", "static", "yield", "null", "true", "false"
                },
                rule
        );
    }

    // reservedWord
    //  : keyword
    //  | futureReservedWord
    //  | ( NullLiteral
    //    | BooleanLiteral
    //    )
    //  ;
    @Test
    public void reservedWordTest() throws Exception {

        final String rule = "reservedWord";

        this.test(new String[]{
                        "break", "do", "instanceof", "typeof", "case", "else", "new", "var", "catch", "finally",
                        "return", "void", "continue", "for", "switch", "while", "debugger", "function", "this",
                        "with", "default", "if", "throw", "delete", "in", "try", "class", "enum", "extends",
                        "super", "const", "export", "import", "implements", "let", "private", "public", "interface",
                        "package", "protected", "static", "yield", "null", "true", "false"
                },
                rule
        );
    }

    // keyword
    //  : Break
    //  | Do
    //  | Instanceof
    //  | Typeof
    //  | Case
    //  | Else
    //  | New
    //  | Var
    //  | Catch
    //  | Finally
    //  | Return
    //  | Void
    //  | Continue
    //  | For
    //  | Switch
    //  | While
    //  | Debugger
    //  | Function
    //  | This
    //  | With
    //  | Default
    //  | If
    //  | Throw
    //  | Delete
    //  | In
    //  | Try
    //  ;
    @Test
    public void keywordTest() throws Exception {

        final String rule = "keyword";

        this.test(new String[]{
                        "break", "do", "instanceof", "typeof", "case", "else", "new", "var", "catch", "finally",
                        "return", "void", "continue", "for", "switch", "while", "debugger", "function", "this",
                        "with", "default", "if", "throw", "delete", "in", "try"
                },
                rule
        );
    }

    // futureReservedWord
    //  : Class
    //  | Enum
    //  | Extends
    //  | Super
    //  | Const
    //  | Export
    //  | Import
    //  | Implements
    //  | Let
    //  | Private
    //  | Public
    //  | Interface
    //  | Package
    //  | Protected
    //  | Static
    //  | Yield
    //  ;
    @Test
    public void futureReservedWordTest() throws Exception {

        final String rule = "futureReservedWord";

        this.test(new String[]{
                        "class", "enum", "extends", "super", "const", "export", "import", "implements", "let",
                        "private", "public", "interface", "package", "protected", "static", "yield"
                },
                rule
        );
    }

    // getter
    //  : {_input.LT(1).getText().startsWith("get")}? Identifier
    //  ;
    @Test
    public void getterTest() throws Exception {

        final String rule = "getter";

        this.test("getSomething", rule);
    }

    // setter
    //  : {_input.LT(1).getText().startsWith("set")}? Identifier
    //  ;
    @Test
    public void setterTest() throws Exception {

        final String rule = "setter";

        this.test("setSomething", rule);
    }
}

