package nl.bigo.ecmascriptparser;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

public class DescriptiveBailErrorListener extends BaseErrorListener {

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                            int line, int charPositionInLine,
                            String msg, RecognitionException e) {

        String sourceName = recognizer.getInputStream().getSourceName();

        if (sourceName != null && !sourceName.isEmpty()) {
            sourceName = String.format("%s:%d:%d: ", sourceName, line, charPositionInLine);
        }

        throw new RuntimeException(sourceName + ", line " + line + ":" + charPositionInLine + " " + msg);
    }
}