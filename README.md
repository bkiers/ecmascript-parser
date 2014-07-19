# ECMAScript parser &nbsp; [![Build Status](https://travis-ci.org/bkiers/ecmascript-parser.png)](https://travis-ci.org/bkiers/ecmascript-parser)

An ANTLR4 grammar for ECMAScript based on the
[Standard ECMA-262 5.1 Edition from June 2011](http://www.ecma-international.org/ecma-262/5.1).

## Install

To install this library, do the following:

```bash
git clone https://github.com/bkiers/ecmascript-parser
cd ecmascript-parser
mvn clean install
```

## Example

Let's say you would like to extract all regex literals from the following script:

```js
// comment /./m

var x = a / b / m; // division, no regex!

/.*?\/\n\\(?!$)/.match('some text')

/*
another
/no pattern!/
comment
*/

var z = /a/m
```

If you look at the parser rule for a JS literal:

```antlr
literal
 : ( NullLiteral
   | BooleanLiteral
   | StringLiteral
   | RegularExpressionLiteral
   )
 | numericLiteral
 ;
```

you see that it can potentially match a `RegularExpressionLiteral`.

We can now retrieve all regex literals by attaching a tree listener to
the parser and upon entering the `literal` rule, we simply check if
`RegularExpressionLiteral()` is not `null`, in which case we print the
contents of this token:

```java
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

// Create the parser.
ECMAScriptParser parser = new Builder.Parser(script).build();

// Walk the parse tree and listen when the `literal` is being entered.
ParseTreeWalker.DEFAULT.walk(new ECMAScriptBaseListener(){
    @Override
    public void enterLiteral(@NotNull ECMAScriptParser.LiteralContext ctx) {
        if (ctx.RegularExpressionLiteral() != null) {
            System.out.println("regex: " + ctx.RegularExpressionLiteral().getText());
        }
    }
}, parser.program());
```

Running the code above will print the following:

```
regex: /.*?\/\n\\(?!$)/
regex: /a/m
```
