package exploreCangjieExpr

import std.ast.*
import exploreCangjieExpr.macros.tokenValue

let AddToken = Token(TokenKind.ADD, "+")
let SubToken = Token(TokenKind.SUB, "-")
let MulToken = Token(TokenKind.MUL, "*")
let DivToken = Token(TokenKind.DIV, "/")

func mutateOp(expr: BinaryExpr, op: Token) {
    expr.op = op
}



main(): Int64 {
    let expr = quote(1 + 2 * 3)
    println("expr: ${expr.toString()}")
    let xx = @tokenValue(expr)
    println("exprExpr-values = ${xx}")

    let xv = quote(
        { =>
        $(expr)        
        }()
    )

    println("xv: ${xv}")


    let x = {=> return 1 + 2 * 3}()
    println("x: ${x}")

    let exprExpr = BinaryExpr(expr)
    println("=======================================")
    println("exprExpr: ${exprExpr.leftExpr.toTokens().size}")
    println("exprExpr: ${exprExpr.op.kind == TokenKind.ADD}, value = ${exprExpr.op.value}")
    println("exprExpr: ${exprExpr.rightExpr.toTokens().size}")
    
    return 0
}