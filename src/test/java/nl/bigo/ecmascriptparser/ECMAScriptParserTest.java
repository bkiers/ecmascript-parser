package nl.bigo.ecmascriptparser;

import org.junit.Test;
import java.lang.reflect.Method;
import static org.junit.Assert.fail;

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
        try {
            // Create the lexer and parser.
            ECMAScriptLexer lexer = new Builder.Lexer(source)
                    .withStrictMode(strictMode)
                    .build();

            ECMAScriptParser parser = new Builder.Parser(lexer).build();

            // Invoke the parser's rule.
            Method method = ECMAScriptParser.class.getDeclaredMethod(rule, new Class[]{});
            method.invoke(parser);

            // Make sure the entire input is parsed.
            parser.eof();
        }
        catch (NoSuchMethodException e) {
            fail("no such parser rule: `" + rule + "`");
        }
        catch (Exception e) {
            fail("could not parse source: `" + source + "`");
            e.printStackTrace();
        }
    }

    // program
    //  : sourceElements? EOF
    //  ;
    @Test
    public void programTest() throws Exception {

        final String rule = "program";

        this.test(new String[]{
                        "",
                        "{}",
                        ";;;;;"
                },
                rule
        );
    }

    // sourceElements
    //  : sourceElement+
    //  ;
    @Test
    public void sourceElementsTest() throws Exception {

        final String rule = "sourceElements";

        this.test(new String[]{
                        "var x = 1;",
                        "foo()\n {;;;} mu[a][a*a][1][v_v][0](1,2,3)(null).xyz"
                },
                rule
        );
    }

    // sourceElement
    //  : statement
    //  | functionDeclaration
    //  ;
    @Test
    public void sourceElementTest() throws Exception {

        final String rule = "sourceElement";

        this.test(new String[]{
                        "var sum = add(1,2,3)\n",
                        "function add(a,b,c) { return a+b+c }"
                },
                rule
        );
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

        this.test(new String[]{
                        "{}",
                        "var x = mu;",
                        ";",
                        "foo(), log(error)",
                        "if (true) { /* ... */ } else if(x) { /* ... */ } else { /* ... */ } ",
                        "for (;;) { /* ... */ }",
                        "continue;",
                        "break\n",
                        "return null;",
                        "with (x) { /* ... */ }",
                        "x : { /* ... */ }",
                        "switch (x) { /* ... */ }",
                        "throw 'message'\n",
                        "try { /* ... */ } catch (e) { /* ... */ }",
                        "debugger\n",
                },
                rule
        );
    }

    // block
    //  : '{' statementList? '}'
    //  ;
    @Test
    public void blockTest() throws Exception {

        final String rule = "block";

        this.test(new String[]{
                        "{}",
                        "{print();log()\nfoo(1,2,3)}"
                },
                rule
        );
    }

    // statementList
    //  : statement+
    //  ;
    @Test
    public void statementListTest() throws Exception {

        final String rule = "statementList";

        this.test(new String[]{
                        "print('1'); for(;;){}\n{/pattern/i.match('PATTERN')}console.log('mu')",
                },
                rule
        );
    }

    // variableStatement
    //  : Var variableDeclarationList eos
    //  ;
    @Test
    public void variableStatementTest() throws Exception {

        final String rule = "variableStatement";

        this.test(new String[]{
                        "var x = null;",
                        "var x = y, z = 1234567/876543/1234556\n"
                },
                rule
        );
    }

    // variableDeclarationList
    //  : variableDeclaration ( ',' variableDeclaration )*
    //  ;
    @Test
    public void variableDeclarationListTest() throws Exception {

        final String rule = "variableDeclarationList";

        this.test(new String[]{
                        "a=b",
                        "a=1,b=(2)/(((4))),c=/3/m"
                },
                rule
        );
    }

    // variableDeclaration
    //  : Identifier initialiser?
    //  ;
    @Test
    public void variableDeclarationTest() throws Exception {

        final String rule = "variableDeclaration";

        this.test(new String[]{
                        "a = 21",
                        "ID=1/1==3||5-(40000000000000000000)"
                },
                rule
        );
    }

    // initialiser
    //  : '=' singleExpression
    //  ;
    @Test
    public void initialiserTest() throws Exception {

        final String rule = "initialiser";

        this.test(new String[]{ "= true", "= 1+2+9*122/foo[x]" }, rule);
    }

    // emptyStatement
    //  : SemiColon
    //  ;
    @Test
    public void emptyStatementTest() throws Exception {

        final String rule = "emptyStatement";

        this.test(new String[]{ ";" }, rule);
    }

    // expressionStatement
    //  : expressionSequence
    //  ;
    @Test
    public void expressionStatementTest() throws Exception {

        final String rule = "expressionStatement";

        this.test(new String[]{
                        "1",
                        "true, false, null, array[0+12*58/111111], f(1,2,3)",
                },
                rule
        );
    }

    // ifStatement
    //  : If '(' expressionSequence ')' statement ( Else statement )?
    //  ;
    @Test
    public void ifStatementTest() throws Exception {

        final String rule = "ifStatement";

        this.test(new String[]{
                        "if (true || false) { /* ... */ }",
                        "if (true || false) { /* ... */ } else if (x) { /* ... */ }",
                        "if (true || false) { /* ... */ } else { /* ... */ }",
                        "if (true || false) { /* ... } else if (1 < 2) { /* ... */ } else { /* ... */ }",
                },
                rule
        );
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

        this.test(new String[]{
                        "do { /* statement/block */ } while(true);",
                        "do { /* statement/block */ } while(true)\n",
                        "while (1,foo,null) { /* statement/block */ }",
                        "for (;;) { /* statement/block */ }",
                        "for (var i = 0; i < x; ++i) { /* statement/block */ }",
                        "for (item in array) { /* statement/block */ }",
                        "for (var a in []) { /* statement/block */ }",
                },
                rule
        );
    }

    // continueStatement
    //  : Continue Identifier? eos
    //  ;
    @Test
    public void continueStatementTest() throws Exception {

        final String rule = "continueStatement";

        this.test(new String[]{
                        "continue something;", "continue now\n", "continue"
                },
                rule
        );
    }

    // breakStatement
    //  : Break Identifier? eos
    //  ;
    @Test
    public void breakStatementTest() throws Exception {

        final String rule = "breakStatement";

        this.test(new String[]{
                        "break something;", "break now\n", "break"
                },
                rule
        );
    }

    // returnStatement
    //  : Return expressionSequence? eos
    //  ;
    @Test
    public void returnStatementTest() throws Exception {

        final String rule = "returnStatement";

        this.test(new String[]{
                        "return something;", "return 1+2*3\n", "return"
                },
                rule
        );
    }

    // withStatement
    //  : With '(' expressionSequence ')' statement
    //  ;
    @Test
    public void withStatementTest() throws Exception {

        final String rule = "withStatement";

        this.test(new String[]{
                        "with(1,2,3){}",
                        "with (x) throw 'error'",
                },
                rule
        );
    }

    // switchStatement
    //  : Switch '(' expressionSequence ')' caseBlock
    //  ;
    @Test
    public void switchStatementTest() throws Exception {

        final String rule = "switchStatement";

        this.test(new String[]{ "switch (variable) { /* caseBlock */ }" }, rule);
    }

    // caseBlock
    //  : '{' caseClauses? ( defaultClause caseClauses? )? '}'
    //  ;
    @Test
    public void caseBlockTest() throws Exception {

        final String rule = "caseBlock";

        this.test(new String[]{ "{ /* caseClauses ... */  }" }, rule);
    }

    // caseClauses
    //  : caseClause+
    //  ;
    @Test
    public void caseClausesTest() throws Exception {

        final String rule = "caseClauses";

        this.test(new String[]{
                        "case 1: break;",
                        "case foo: doSomething(); break; case mu: case bu: break;"
                },
                rule
        );
    }

    // caseClause
    //  : Case expressionSequence ':' statementList?
    //  ;
    @Test
    public void caseClauseTest() throws Exception {

        final String rule = "caseClause";

        this.test(new String[]{
                        "case 1:",
                        "case foo: doSomething(); break;"
                },
                rule
        );
    }

    // defaultClause
    //  : Default ':' statementList?
    //  ;
    @Test
    public void defaultClauseTest() throws Exception {

        final String rule = "defaultClause";

        this.test(new String[]{
                        "default:",
                        "default: mu(); break;"
                },
                rule
        );
    }

    // labelledStatement
    //  : Identifier ':' statement
    //  ;
    @Test
    public void labelledStatementTest() throws Exception {

        final String rule = "labelledStatement";

        this.test(new String[]{ "label: error()" }, rule);
    }

    // throwStatement
    //  : Throw expressionSequence eos
    //  ;
    @Test
    public void throwStatementTest() throws Exception {

        final String rule = "throwStatement";

        this.test(new String[]{
                        "throw 'err';",
                        "throw 'another-err', null, false\n"
                },
                rule
        );
    }

    // tryStatement
    //  : Try block catchProduction
    //  | Try block finallyProduction
    //  | Try block catchProduction finallyProduction
    //  ;
    @Test
    public void tryStatementTest() throws Exception {

        final String rule = "tryStatement";

        this.test(new String[]{
                        "try { /* block */ } catch (error) { /* block */ }",
                        "try { /* block */ } finally { /* block */ }",
                        "try { /* block */ } catch (error) { /* block */ } finally { /* block */ }"
                },
                rule
        );
    }

    // catchProduction
    //  : Catch '(' Identifier ')' block
    //  ;
    @Test
    public void catchProductionTest() throws Exception {

        final String rule = "catchProduction";

        this.test(new String[]{ "catch (error) { /* block */ }" }, rule);
    }

    // finallyProduction
    //  : Finally block
    //  ;
    @Test
    public void finallyProductionTest() throws Exception {

        final String rule = "finallyProduction";

        this.test(new String[]{ "finally { /* block */ }" }, rule);
    }

    // debuggerStatement
    //  : Debugger eos
    //  ;
    @Test
    public void debuggerStatementTest() throws Exception {

        final String rule = "debuggerStatement";

        this.test(new String[]{ "debugger;", "debugger" }, rule);
    }

    // functionDeclaration
    //  : Function Identifier '(' formalParameterList? ')' '{' functionBody '}'
    //  ;
    @Test
    public void functionDeclarationTest() throws Exception {

        final String rule = "functionDeclaration";

        this.test(new String[]{
                        "function foo () { /* functionBody */ }",
                        "function mu (a, b, c) { /* functionBody */ }"
                },
                rule
        );
    }

    // formalParameterList
    //  : Identifier ( ',' Identifier )*
    //  ;
    @Test
    public void formalParameterListTest() throws Exception {

        final String rule = "formalParameterList";

        this.test(new String[]{ "a", "a, b, c" }, rule);
    }

    // functionBody
    //  : sourceElements?
    //  ;
    @Test
    public void functionBodyTest() throws Exception {

        final String rule = "functionBody";

        this.test(new String[]{ "", "expr(); throw 'something';" }, rule);
    }

    // arrayLiteral
    //  : '[' elementList? ','? elision? ']'
    //  ;
    @Test
    public void arrayLiteralTest() throws Exception {

        final String rule = "arrayLiteral";

        this.test(new String[]{ "[]", "[1, 2 ,3]", "[,,,1,,2,,,3,,,,]", "[,,,,,,,]" }, rule);
    }

    // elementList
    //  : elision? singleExpression ( ',' elision? singleExpression )*
    //  ;
    @Test
    public void elementListTest() throws Exception {

        final String rule = "elementList";

        this.test(new String[]{ "1, 2, 3", ",,,1,,2,,,3" }, rule);
    }

    // elision
    //  : ','+
    //  ;
    @Test
    public void elisionTest() throws Exception {

        final String rule = "elision";

        this.test(new String[]{ ",", ",,,,,,," }, rule);
    }

    // objectLiteral
    //  : '{' propertyNameAndValueList? ','? '}'
    //  ;
    @Test
    public void objectLiteralTest() throws Exception {

        final String rule = "objectLiteral";

        this.test(new String[]{
                        "{}",
                        "{a:1, b:2}",
                        "{a:1, b:2,}",
                        "{,}",
                        "{ get mu(){}, set mu(mu){} }"
                },
                rule
        );
    }

    // propertyNameAndValueList
    //  : propertyAssignment ( ',' propertyAssignment )*
    //  ;
    @Test
    public void propertyNameAndValueListTest() throws Exception {

        final String rule = "propertyNameAndValueList";

        this.test(new String[]{
                        "a:b",
                        "c:'ddd', 'foo':true, 1:null"
                },
                rule
        );
    }

    // propertyAssignment
    //  : propertyName ':' singleExpression
    //  | getter '(' ')' '{' functionBody '}'
    //  | setter '(' propertySetParameterList ')' '{' functionBody '}'
    //  ;
    @Test
    public void propertyAssignmentTest() throws Exception {

        final String rule = "propertyAssignment";

        this.test(new String[]{
                        "mu : null",
                        "'mu' : null",
                        "\"mu\" : null",
                        "1 : null",
                        "0xff : null",
                        "get mu(){}",
                        "set mu(mu){}",
                },
                rule
        );
    }

    // propertyName
    //  : identifierName
    //  | StringLiteral
    //  | numericLiteral
    //  ;
    @Test
    public void propertyNameTest() throws Exception {

        final String rule = "propertyName";

        this.test(new String[]{ "mu", "'mu'", "\"mu\"", "42", "0Xabc" }, rule);
    }

    // propertySetParameterList
    //  : Identifier
    //  ;
    @Test
    public void propertySetParameterListTest() throws Exception {

        final String rule = "propertySetParameterList";

        this.test(new String[]{ "mu" }, rule);
    }

    // arguments
    //  : '(' argumentList? ')'
    //  ;
    @Test
    public void argumentsTest() throws Exception {

        final String rule = "arguments";

        this.test(new String[]{
                        "()",
                        "( 1+2, b/3, /./mi )"
                },
                rule
        );
    }

    // argumentList
    //  : singleExpression ( ',' singleExpression )*
    //  ;
    @Test
    public void argumentListTest() throws Exception {

        final String rule = "argumentList";

        this.test(new String[]{
                        "a", "1+2", "a,b   ,   c"
                },
                rule
        );
    }

    // expressionSequence
    //  : singleExpression ( ',' singleExpression )*
    //  ;
    @Test
    public void expressionSequenceTest() throws Exception {

        final String rule = "expressionSequence";

        this.test(new String[]{ "1, 2*3, mu, foo(), bar[0*0]" }, rule);
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

        test("value = 42", rule);
        test("a = 1+2, 3, b", rule);

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

        this.test("get something", rule);
    }

    // setter
    //  : {_input.LT(1).getText().startsWith("set")}? Identifier
    //  ;
    @Test
    public void setterTest() throws Exception {

        final String rule = "setter";

        this.test("set something", rule);
    }
}

