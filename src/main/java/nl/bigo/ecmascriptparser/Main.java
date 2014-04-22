package nl.bigo.ecmascriptparser;

public class Main {

    public static void main(String[] args) throws Exception {

        String script = "    $(document).ready(function () {\n" +
                "        var myVar = new cinema(json_structure); \n" +
                "    });";

        System.out.println(Builder.Tree.toStringASCII(script));
    }
}
