package nl.bigo.ecmascriptparser;

import org.junit.Test;
import java.lang.reflect.Method;
import static org.junit.Assert.fail;

public class ECMAScriptParserErrorTest {

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
     * specific parser rule. Note that this method expects the input
     * to be invalid (it is expected to throw an exception).
     *
     * @param source
     *         the input to parse.
     * @param rule
     *         the parser rule to invoke to get the `source` to be parsed.
     */
    private void test(String source, String rule, boolean strictMode) throws Exception {
        try {
            // Create the lexer and parser.
            ECMAScriptLexer lexer = new ECMAScriptBuilder.Lexer(source)
                    .withStrictMode(strictMode)
                    .build();

            ECMAScriptParser parser = new ECMAScriptBuilder.Parser(lexer).build();

            // Invoke the parser's rule.
            Method method = ECMAScriptParser.class.getDeclaredMethod(rule, new Class[]{});
            method.invoke(parser);

            // Make sure the entire input is parsed.
            parser.eof();

            fail("expected an exception for source: `" + source + "`");
        }
        catch (NoSuchMethodException e) {
            fail("no such parser rule: `" + rule + "`");
        }
        catch (Exception e) {
            /* expected a failed parse */
        }
    }

    // program
    //  : sourceElements? EOF
    //  ;
    @Test
    public void programErrorTest() throws Exception {

        final String rule = "program";

        this.test(new String[]{
                        "{"
                },
                rule
        );
    }

    // sourceElements
    //  : sourceElement+
    //  ;
    @Test
    public void sourceElementsErrorTest() throws Exception {

        final String rule = "sourceElements";

        this.test(new String[]{
                        "var x = ",
                        "foo()\n {;;;} mu[a [a*a][1][v_v][0](1,2,3)(null).xyz"
                },
                rule
        );
    }

    // sourceElement
    //  : statement
    //  | functionDeclaration
    //  ;
    @Test
    public void sourceElementErrorTest() throws Exception {

        final String rule = "sourceElement";

        this.test(new String[]{
                        "var sum = add(1,2,3 \n",
                        "function add(a,b,c) return a+b+c }"
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
    public void statementErrorTest() throws Exception {

        final String rule = "statement";

        this.test(new String[]{
                        "{",
                        "var x mu;",
                        "foo), log(error)",
                        "if true) { /* ... */ } else if(x) { /* ... */ } else { /* ... */ } ",
                        "for (;) { /* ... */ }",
                        "with ( ) { /* ... */ }",
                        "x : /* ... */ }",
                        "switch x) { /* ... */ }",
                        "throw message'\n",
                        "try  /* ... */ } catch (e) { /* ... */ }",
                },
                rule
        );
    }

    // block
    //  : '{' statementList? '}'
    //  ;
    @Test
    public void blockErrorTest() throws Exception {

        final String rule = "block";

        this.test(new String[]{
                        "{",
                        "{print();log()\nfoo(1 2,3)}"
                },
                rule
        );
    }

    // statementList
    //  : statement+
    //  ;
    @Test
    public void statementListErrorTest() throws Exception {

        final String rule = "statementList";

        this.test(new String[]{
                        "print('1'); for(;;){}\n{/pattern/i.match('PATTERN')}console.log(mu')",
                },
                rule
        );
    }

    // variableStatement
    //  : Var variableDeclarationList eos
    //  ;
    @Test
    public void variableStatementErrorTest() throws Exception {

        final String rule = "variableStatement";

        this.test(new String[]{
                        "var x null;",
                        "var x = y z = 1234567/876543/1234556\n"
                },
                rule
        );
    }

    // variableDeclarationList
    //  : variableDeclaration ( ',' variableDeclaration )*
    //  ;
    @Test
    public void variableDeclarationListErrorTest() throws Exception {

        final String rule = "variableDeclarationList";

        this.test(new String[]{
                        "a=",
                        "a=1,b=(2)/((4))),c=/3/m"
                },
                rule
        );
    }

    // variableDeclaration
    //  : Identifier initialiser?
    //  ;
    @Test
    public void variableDeclarationErrorTest() throws Exception {

        final String rule = "variableDeclaration";

        this.test(new String[]{
                        " = 21",
                        "ID=1/1==3||5- 40000000000000000000)"
                },
                rule
        );
    }

    // initialiser
    //  : '=' singleExpression
    //  ;
    @Test
    public void initialiserErrorTest() throws Exception {

        final String rule = "initialiser";

        this.test(new String[]{ " true", "= 1+2+9*122/foo[x" }, rule);
    }

    // emptyStatement
    //  : SemiColon
    //  ;
    @Test
    public void emptyStatementErrorTest() throws Exception {

        final String rule = "emptyStatement";

        this.test(new String[]{ "" }, rule);
    }

    // expressionStatement
    //  : expressionSequence
    //  ;
    @Test
    public void expressionStatementErrorTest() throws Exception {

        final String rule = "expressionStatement";

        this.test(new String[]{
                        "",
                        "true false, null, array[0+12*58/111111], f(1,2,3)",
                },
                rule
        );
    }

    // ifStatement
    //  : If '(' expressionSequence ')' statement ( Else statement )?
    //  ;
    @Test
    public void ifStatementErrorTest() throws Exception {

        final String rule = "ifStatement";

        this.test(new String[]{
                        "if (true false) { /* ... */ }",
                        "if (true || false { /* ... */ } else if (x) { /* ... */ }",
                        "if (true || false) { /* ... */  else { /* ... */ }",
                        "if (true || false) { /* ... } else if (1 < 2) { /* ... */ } else /* ... */ }",
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
    public void iterationStatementErrorTest() throws Exception {

        final String rule = "iterationStatement";

        this.test(new String[]{
                        "do { /* statement/block */ } while(true;",
                        "do { /* statement/block */ } while true)",
                        "while (1,foo null) { /* statement/block */ }",
                        "for (;) { /* statement/block */ }",
                        "for (var i = 0; i < x ++i) { /* statement/block */ }",
                        "for (item in array)  /* statement/block */ }",
                        "for (var a in [) { /* statement/block */ }",
                },
                rule
        );
    }

    // continueStatement
    //  : Continue Identifier? eos
    //  ;
    @Test
    public void continueStatementErrorTest() throws Exception {

        final String rule = "continueStatement";

        this.test(new String[]{
                        "Continue something;"
                },
                rule
        );
    }

    // breakStatement
    //  : Break Identifier? eos
    //  ;
    @Test
    public void breakStatementErrorTest() throws Exception {

        final String rule = "breakStatement";

        this.test(new String[]{
                        "Break something;"
                },
                rule
        );
    }

    // returnStatement
    //  : Return expressionSequence? eos
    //  ;
    @Test
    public void returnStatementErrorTest() throws Exception {

        final String rule = "returnStatement";

        this.test(new String[]{
                        "Return something;"
                },
                rule
        );
    }

    // withStatement
    //  : With '(' expressionSequence ')' statement
    //  ;
    @Test
    public void withStatementErrorTest() throws Exception {

        final String rule = "withStatement";

        this.test(new String[]{
                        "with(1,2 3){}",
                        "with (x throw 'error'",
                },
                rule
        );
    }

    // switchStatement
    //  : Switch '(' expressionSequence ')' caseBlock
    //  ;
    @Test
    public void switchStatementErrorTest() throws Exception {

        final String rule = "switchStatement";

        this.test(new String[]{ "switch (variable { /* caseBlock */ }" }, rule);
    }

    // caseBlock
    //  : '{' caseClauses? ( defaultClause caseClauses? )? '}'
    //  ;
    @Test
    public void caseBlockErrorTest() throws Exception {

        final String rule = "caseBlock";

        this.test(new String[]{ "{ /* caseClauses ... */  " }, rule);
    }

    // caseClauses
    //  : caseClause+
    //  ;
    @Test
    public void caseClausesErrorTest() throws Exception {

        final String rule = "caseClauses";

        this.test(new String[]{
                        "case 1 break;",
                        "case foo: doSomething(); break; case mu case bu: break;"
                },
                rule
        );
    }

    // caseClause
    //  : Case expressionSequence ':' statementList?
    //  ;
    @Test
    public void caseClauseErrorTest() throws Exception {

        final String rule = "caseClause";

        this.test(new String[]{
                        "case 1",
                        "Case foo: doSomething(); break;"
                },
                rule
        );
    }

    // defaultClause
    //  : Default ':' statementList?
    //  ;
    @Test
    public void defaultClauseErrorTest() throws Exception {

        final String rule = "defaultClause";

        this.test(new String[]{
                        "default",
                        "default: mu); break;"
                },
                rule
        );
    }

    // labelledStatement
    //  : Identifier ':' statement
    //  ;
    @Test
    public void labelledStatementErrorTest() throws Exception {

        final String rule = "labelledStatement";

        this.test(new String[]{ "label error()" }, rule);
    }

    // throwStatement
    //  : Throw expressionSequence eos
    //  ;
    @Test
    public void throwStatementErrorTest() throws Exception {

        final String rule = "throwStatement";

        this.test(new String[]{
                        "throw 'err;",
                        "throW 'another-err', null, false\n"
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
    public void tryStatementErrorTest() throws Exception {

        final String rule = "tryStatement";

        this.test(new String[]{
                        "tr { /* block */ } catch (error) { /* block */ }",
                        "try { /* block */  finally { /* block */ }",
                        "try { /* block */ } catch (error { /* block */ } finally { /* block */ }"
                },
                rule
        );
    }

    // catchProduction
    //  : Catch '(' Identifier ')' block
    //  ;
    @Test
    public void catchProductionErrorTest() throws Exception {

        final String rule = "catchProduction";

        this.test(new String[]{ "catch (error { /* block */ }" }, rule);
    }

    // finallyProduction
    //  : Finally block
    //  ;
    @Test
    public void finallyProductionErrorTest() throws Exception {

        final String rule = "finallyProduction";

        this.test(new String[]{ "finally { /* block */ " }, rule);
    }

    // debuggerStatement
    //  : Debugger eos
    //  ;
    @Test
    public void debuggerStatementErrorTest() throws Exception {

        final String rule = "debuggerStatement";

        this.test(new String[]{ "Debugger;" }, rule);
    }

    // functionDeclaration
    //  : Function Identifier '(' formalParameterList? ')' '{' functionBody '}'
    //  ;
    @Test
    public void functionDeclarationErrorTest() throws Exception {

        final String rule = "functionDeclaration";

        this.test(new String[]{
                        "function foo ( { /* functionBody */ }",
                        "function mu (a b, c) { /* functionBody */ }"
                },
                rule
        );
    }

    // formalParameterList
    //  : Identifier ( ',' Identifier )*
    //  ;
    @Test
    public void formalParameterListErrorTest() throws Exception {

        final String rule = "formalParameterList";

        this.test(new String[]{ "a, b c" }, rule);
    }

    // functionBody
    //  : sourceElements?
    //  ;
    @Test
    public void functionBodyErrorTest() throws Exception {

        final String rule = "functionBody";

        this.test(new String[]{ "expr(; throw 'something';" }, rule);
    }

    // arrayLiteral
    //  : '[' elementList? ','? elision? ']'
    //  ;
    @Test
    public void arrayLiteralErrorTest() throws Exception {

        final String rule = "arrayLiteral";

        this.test(new String[]{ "[" }, rule);
    }

    // elementList
    //  : elision? singleExpression ( ',' elision? singleExpression )*
    //  ;
    @Test
    public void elementListErrorTest() throws Exception {

        final String rule = "elementList";

        this.test(new String[]{ "1 2, 3" }, rule);
    }

    // elision
    //  : ','+
    //  ;
    @Test
    public void elisionErrorTest() throws Exception {

        final String rule = "elision";

        this.test(new String[]{ "" }, rule);
    }

    // objectLiteral
    //  : '{' propertyNameAndValueList? ','? '}'
    //  ;
    @Test
    public void objectLiteralErrorTest() throws Exception {

        final String rule = "objectLiteral";

        this.test(new String[]{
                        "{",
                        "{a 1, b:2}",
                        "{a:1, b:2,",
                        ",}",
                        "{ GetMu(){}, setMu(mu){} }"
                },
                rule
        );
    }

    // propertyNameAndValueList
    //  : propertyAssignment ( ',' propertyAssignment )*
    //  ;
    @Test
    public void propertyNameAndValueListErrorTest() throws Exception {

        final String rule = "propertyNameAndValueList";

        this.test(new String[]{
                        "a b",
                        "c: ddd', 'foo':true, 1:null"
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
    public void propertyAssignmentErrorTest() throws Exception {

        final String rule = "propertyAssignment";

        this.test(new String[]{
                        "mu   null",
                        "'mu : null",
                        "\"mu\" :",
                        " : null",
                        "xff  null",
                        "GetMu(){}",
                        "SetMu(mu){}",
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
    public void propertyNameErrorTest() throws Exception {

        final String rule = "propertyName";

        this.test(new String[]{ "", "'mu", "mu\"" }, rule);
    }

    // propertySetParameterList
    //  : Identifier
    //  ;
    @Test
    public void propertySetParameterListErrorTest() throws Exception {

        final String rule = "propertySetParameterList";

        this.test(new String[]{ "", "for", "null" }, rule);
    }

    // arguments
    //  : '(' argumentList? ')'
    //  ;
    @Test
    public void argumentsErrorTest() throws Exception {

        final String rule = "arguments";

        this.test(new String[]{
                        "(",
                        "( 1 2, b/3, /./mi )"
                },
                rule
        );
    }

    // argumentList
    //  : singleExpression ( ',' singleExpression )*
    //  ;
    @Test
    public void argumentListErrorTest() throws Exception {

        final String rule = "argumentList";

        this.test(new String[]{
                        "", "1+", "a b   ,   c"
                },
                rule
        );
    }

    // expressionSequence
    //  : singleExpression ( ',' singleExpression )*
    //  ;
    @Test
    public void expressionSequenceErrorTest() throws Exception {

        final String rule = "expressionSequence";

        this.test(new String[]{ ", 2*3, mu, foo(), bar[0*0]" }, rule);
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
    public void singleExpressionErrorTest() throws Exception {

        final String rule = "singleExpression";

        test("a = b  this, foo()", rule);

        test("a  b", rule);
        test("a =* b", rule);

        test("a ? b c", rule);

        test("value 42", rule);
        test("a = 1+, 3, b", rule);

        test("a ||| b", rule);
        test("a &&& b", rule);

        test("a == ", rule);
        test("a != ", rule);
        test(" === b", rule);
        test("!==b", rule);

        test("a instancof b", rule);
        test("a In b", rule);

        test("Delete 1", rule);
        test("Void x", rule);
        test("Typeof s", rule);

        test("x\n++", rule);
        test("x\n--", rule);
        test("bar()\n++", rule);

        test("foo())", rule);
        test("foo()123]", rule);
        test("bar()x", rule);

        test("new New X()", rule);

        test("array[0", rule);
        test("object property", rule);
        test("new Something 1, 2, 3)", rule);

        test("function mu() /* something */ }", rule);
        test("function () { return x;", rule);
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
    public void assignmentOperatorErrorTest() throws Exception {

        final String rule = "assignmentOperator";

        this.test(new String[]{
                        "=*", "=/", "=="
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
    public void literalErrorTest() throws Exception {

        final String rule = "literal";

        this.test(new String[]{
                        "nul", "True", "falsE", "foo\"", "'\\n", "/.//", "/\\/\\n\\/mi"
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
    public void numericLiteralErrorTest() throws Exception {

        final String rule = "numericLiteral";

        this.test(new String[]{
                        "010", "x10", "0XFFZeAb49"
                },
                rule
        );

        this.test("078", rule, false);
    }

    // identifierName
    //  : Identifier
    //  | reservedWord
    //  ;
    @Test
    public void identifierNameErrorTest() throws Exception {

        final String rule = "identifierName";

        this.test(new String[]{
                        "0mu", "d@d"
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
    public void reservedWordErrorTest() throws Exception {

        final String rule = "reservedWord";

        this.test(new String[]{
                        "Break"
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
    public void keywordErrorTest() throws Exception {

        final String rule = "keyword";

        this.test(new String[]{
                        "Do"
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
    public void futureReservedWordErrorTest() throws Exception {

        final String rule = "futureReservedWord";

        this.test(new String[]{
                        "Public"
                },
                rule
        );
    }

    // getter
    //  : {_input.LT(1).getText().startsWith("get")}? Identifier
    //  ;
    @Test
    public void getterErrorTest() throws Exception {

        final String rule = "getter";

        this.test("geTSomething", rule);
    }

    // setter
    //  : {_input.LT(1).getText().startsWith("set")}? Identifier
    //  ;
    @Test
    public void setterErrorTest() throws Exception {

        final String rule = "setter";

        this.test("sEtSomething", rule);
    }
}

