package muse

import scala.collection.mutable.ListBuffer

class Interpreter(val tokens: List[Token[Any]]) {
    import Token.Type
    private var token_ptr: Int = 0
    private def current_token: Token[Any] = tokens(token_ptr)

    private def factor(): Int = {
        val token = current_token
        
        if(token.tpe == Type.Numerical) {
            eat(Type.Numerical)
            return token.value.asInstanceOf[Int]
        }
        else if (token.tpe == Type.LeftParenthesis) {
            eat(Type.LeftParenthesis)
            val result = expression()
            eat(Type.RightParenthesis)
            return result
        } else {
            throw new BadToken(s"Bad token type when reading factor: '$token.tpe'")
        }
    }

    private def term(): Int = {
        var result = factor()
        while(current_token.tpe == Type.Multiply || current_token.tpe == Type.Divide) {
            if(current_token.tpe == Type.Multiply) {
                eat(Type.Multiply)
                result = result * factor()
            }
            else if(current_token.tpe == Type.Divide) {
                eat(Type.Divide)
                result = result / factor()
            }
        }
        result
    }

    private def eat(tpe: Type): Unit = {
        if(tpe == current_token.tpe) {
            token_ptr += 1
        } else {
            throw new BadToken("Token not recognized!")
        }
    }

    def expression(): Int = {
        /*
        * expr   : term ((Plus | Minus) term)*
        * term   : factor ((Multiply | Divide) factor)*
        * factor : Numerical | LeftParenthesis expr RightParenthesis  
        */
        var result = this.term()
        while(current_token.tpe == Type.Plus || current_token.tpe == Type.Minus) {
            if(current_token.tpe == Type.Plus) {
                eat(Type.Plus)
                result = result + term()
            }
            else if(current_token.tpe == Type.Minus) {
                eat(Type.Minus)
                result = result - term()
            }
        }
        result
    }
}