enum TokenKind {
    Identifier,
    Literal,
    If,
    Then,
    Else,
    End,
    Print,
    Equal,
    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    NotEqual,
    And,
    Or,
    Not,
    LeftParenthesis,
    RightParenthesis,
    Comma,
    SemiColon,
    Dollar,
    EndOfFile,
    Error,
}

type LiteralType = number | string;
interface Token {
    kind: TokenKind,
    line: number,
    lexeme: string,
    literal?: LiteralType,
}

type Expression = { kind: "Expression" }
type Binary = { lhs: Expression, op: TokenKind, rhs: Expression } & Expression
type Unary = { op: TokenKind, value: Expression } & Expression
type Primary = { value: LiteralType } & Expression

function createBinaryExpression(lhs: Expression, op: TokenKind, rhs: Expression): Binary {
    return { kind: "Expression", lhs, op, rhs };
}
  
function createUnaryExpression(op: TokenKind, value: Expression): Unary {
    return { kind: "Expression", op, value };
}
  
function createPrimaryExpression(value: LiteralType): Primary {
    return { kind: "Expression", value };
}

function isLetter(character: string) {
    return /[a-zA-Z]/.test(character);
}

function isNumber(character: string) {
    return /[0-9]/.test(character);
}

const Tokenizer = function (source: string) {
    return {
        keywords: {
            "IF": TokenKind.If,
            "THEN": TokenKind.Then,
            "ELSE": TokenKind.Else,
            "END": TokenKind.End,
            "PRINT": TokenKind.Print,
        },

        source,
        line: 0,
        column: 0,
        
        start: 0,
        current: 0,

        ended() {
            return this.source.length <= this.current;
        },

        peek() {
            return this.source[this.current];
        },
        
        check(char: string) {
            if (char === this.peek()){
                return true;
            }
            return false;
        },

        advance() {
            this.current += 1;
            return this.source[this.current - 1];
        },

        match(char: string){
            if (char === this.peek()){
                this.advance();
                return true;
            }
            return false;
        },

        newToken(kind: TokenKind) {
            let result = {
                kind,
                line: this.line,
                lexeme: this.source.substring(this.start, this.current),
            };

            this.start = this.current;

            return result;
        },

        newLiteralToken(kind: TokenKind, literal: number | string) {
            let result = {
                kind,
                line: this.line,
                lexeme: this.source.substring(this.start, this.current),
                literal,
            };

            this.start = this.current;

            return result;
        },

        parseToken(): Token {
            if (this.ended()) return this.newToken(TokenKind.EndOfFile)
            if (this.match("!")) return (this.match("=")) ? this.newToken(TokenKind.NotEqual) : this.newToken(TokenKind.Not);
            if (this.match(">")) return (this.match("=")) ? this.newToken(TokenKind.GreaterThanOrEqual) : this.newToken(TokenKind.GreaterThan);
            if (this.match("<")) return (this.match("=")) ? this.newToken(TokenKind.LessThanOrEqual) : this.newToken(TokenKind.LessThan);
            if (this.match("=")) return this.newToken(TokenKind.Equal);
            if (this.match("-")) return this.newToken(TokenKind.Minus);
            if (this.match("+")) return this.newToken(TokenKind.Plus);
            if (this.match("*")) return this.newToken(TokenKind.Multiply);
            if (this.match("/")) return this.newToken(TokenKind.Divide);
            if (this.match(",")) return this.newToken(TokenKind.Comma);
            if (this.match("$")) return this.newToken(TokenKind.Dollar);
            if (this.match(";")) return this.newToken(TokenKind.SemiColon);
            if (this.match("(")) return this.newToken(TokenKind.LeftParenthesis);
            if (this.match(")")) return this.newToken(TokenKind.RightParenthesis);
            if (this.match("\"")) {
                while(!this.ended() && !this.match("\"")) {
                    this.advance();
                }
                return this.newLiteralToken(TokenKind.Literal, this.source.substring(this.start + 1, this.current - 1))
            }

            if (this.match(" ") || this.match("\t")) {
                this.start = this.current;
                return this.parseToken();
            }
            if (this.match("\n")) {
                this.line += 1;
                return this.parseToken();
            }

            if (isLetter(this.peek())) {
                while(!this.ended() && isLetter(this.peek())) { this.advance(); }
                let keyword = Object(this.keywords)[this.source.substring(this.start, this.current)];
                if (keyword !== undefined) {
                    return this.newToken(keyword);                
                }
                return this.newToken(TokenKind.Identifier);
            }

            if (isNumber(this.peek())){
                while(!this.ended() && isNumber(this.peek())) {
                    this.advance();
                    if (this.match(".")){
                        while(!this.ended() && isNumber(this.peek())) { this.advance(); }
                        break;
                    }
                }
                return this.newLiteralToken(TokenKind.Literal, parseFloat(this.source.substring(this.start, this.current)));            
            }

            return this.newToken(TokenKind.Error);
        }
    }
}

const Compiler = function (source: string) {
    return {
        tokenizer: Tokenizer(source),

        current: {} as Token,
        previous: {} as Token,

        ended() {
            return this.current.kind === TokenKind.EndOfFile;
        },
    
        check(kind: TokenKind) {
            if (this.current.kind === kind) {
                return true;
            }
            return false;
        },
        
        advance() {
            this.previous = this.current;
            this.current = this.tokenizer.parseToken();
        },
  
        match(kinds: TokenKind[]) {
            for (let kind of kinds) {
                if (this.current.kind === kind) {
                    this.advance();
                    return true;
                }
            }
            return false;
        },

        expect(kind: TokenKind, message: string) {
            if(!this.match([kind])){
                throw {
                    error: "Syntax",
                    previousToken: this.previous,
                    currentToken: this.current,
                    message,
                }
            }
        },

        parsePrimaryExpression() {
            if(this.match([TokenKind.Literal])) {
                return createPrimaryExpression(this.previous.literal!);
            } else if (this.match([TokenKind.LeftParenthesis])) {
                const expression = this.parseExpression();
                this.expect(TokenKind.RightParenthesis, "Expecting closing parenthesis!");
                return expression;
            } else {
                throw {
                    error: "Syntax",
                    previousToken: this.previous,
                    currentToken: this.current,
                    message: "Expecting expression!",
                }
            }
        },

        parseUnaryExpression() {

        },

        parseBinaryExpression() {
            let lhs: Expression = this.parsePrimaryExpression() as Expression;

            while (!this.ended() && this.match([TokenKind.Minus, TokenKind.Plus, TokenKind.Multiply, TokenKind.Divide])) {
                const op = this.previous.kind;
                const rhs: Expression = this.parsePrimaryExpression() as Expression;
                lhs = createBinaryExpression(lhs, op, rhs);
            }

            return lhs;
        },

        parseExpression() {
            return this.parseBinaryExpression();
        },

        compile() {
            this.current = this.tokenizer.parseToken();
            console.log(this.parseExpression());
        }
    }
}

let compiler = Compiler("10 - 10");

try {
    compiler.compile();
} catch (e) {
    console.error(e);
}