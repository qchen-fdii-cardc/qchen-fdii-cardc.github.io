macro package exploreCangjieExpr.macros

import std.ast.*

public macro tokenValue(expr: Tokens): Tokens {    
    return quote(
        { =>
        return $(expr)        
        }()
    )
}