from pyparsing import *

class Adjacent(TokenConverter):
    """Matcher which enforces that matching patterns be contiguous in the input string
    """
    def __init__( self, expr ):
        super(Adjacent,self).__init__( expr )
        # suppress whitespace-stripping in contained parse expressions, but re-enable it on the Combine itself
        self.leaveWhitespace()
        self.skipWhitespace = True
        self.callPreparse = True

    def ignore( self, other ):
        if self.adjacent:
            ParserElement.ignore(self, other)
        else:
            super( Combine, self).ignore( other )
        return self

    def postParse( self, instring, loc, tokenlist ):
        return tokenlist

