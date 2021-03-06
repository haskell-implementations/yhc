def main():
    build("IOSupplement.hs", "-cpp")
    build("CommandLine.hs")
    build("Parsers.hs")
    build("StringMatch.hs")
    build("Fonts.hs")
    build("Lexers.hs")
    build("AbstractSyntax.hs")
    build("EbnfGrammar.hs")
    build("HappyParser.hs")
    build("GrammarTransform.hs")
    build("Color.hs")
    build("Info.hs")
    build("EbnfLayout.hs")
    build("FigOutput.hs")
    build("PsOutput.hs")
    build("Color.hs")
    build("Main.hs")
    run("Main")
