enum TokenKind {
    Identifier,
    Literal,
    If,
    Then,
    Else,
    End,
    Print,
    Goto,
    Rem,
    Input,
    Let,
    Equal,
    EqualEqual,
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

type BindingPower = {fst: number, snd: number}
type Identifier = {variableName: string}
type LiteralType = number | string | Identifier;
interface Token {
    kind: TokenKind,
    line: number,
    lexeme: string,
    literal?: LiteralType,
}

enum Expressions {
    Binary = "BinaryExpression",
    Unary = "UnaryExpression",
    Primary = "PrimaryExpression"
}

type Expression = { kind: Expressions }
type Binary = { lhs: Expression, op: TokenKind, rhs: Expression } & Expression
type Unary = { op: TokenKind, value: Expression } & Expression
type Primary = { value: LiteralType } & Expression

function createBinaryExpression(lhs: Expression, op: TokenKind, rhs: Expression): Binary {
    return { kind: Expressions.Binary, lhs, op, rhs };
}
  
function createUnaryExpression(op: TokenKind, value: Expression): Unary {
    return { kind: Expressions.Unary, op, value };
}
  
function createPrimaryExpression(value: LiteralType): Primary {
    return { kind: Expressions.Primary, value };
}

enum Statements {
    BaseStatement = "Statement",
    PrintStatement = "PrintStatement",
    GotoStatement = "GotoStatement",
    IfStatement = "IfStatement",
    ExpressionStatement = "ExpressionStatement",
    InputStatement = "InputStatement",
    LetStatement = "LetStatement",
}

type Statement = { kind: Statements }
type PrintStatement = { expression: Expression } & Statement
type GotoStatement = { expression: Expression } & Statement
type IfStatement = { condition: Expression, left: Statement[], right?: Statement[]} & Statement
type ExpressionStatement = { expression: Expression } & Statement
type InputStatement = { expression: Expression, variable: Token } & Statement
type LetStatement = { variableName: string, value: Expression } & Statement

function createBaseStatement(): Statement{
    return { kind: Statements.BaseStatement }
}

function createPrintStatement(expression: Expression): PrintStatement {
    return { kind: Statements.PrintStatement, expression }
}

function createGotoStatement(expression: Expression): GotoStatement {
    return { kind: Statements.GotoStatement, expression }
}

function createIfStatement(condition: Expression, left: Statement[], right?: Statement[]): IfStatement {
    return { kind: Statements.IfStatement, condition, left, right }
}

function createExpressionStatement(expression: Expression): ExpressionStatement {
    return { kind: Statements.ExpressionStatement, expression}
}

function createInputStatement(expression: Expression, variable: Token): InputStatement {
    return { kind: Statements.InputStatement, expression, variable }
}

function createLetStatement(variableName: string, value: Expression): LetStatement {
    return { kind: Statements.LetStatement, variableName, value, }
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
            "GOTO": TokenKind.Goto,
            "AND": TokenKind.And,
            "OR": TokenKind.Or,
            "INPUT": TokenKind.Input,
            "REM": TokenKind.Rem,
            "LET": TokenKind.Let,
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
            if (this.match("=")) return (this.match("=")) ? this.newToken(TokenKind.EqualEqual) : this.newToken(TokenKind.Equal);
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
                    if (keyword === TokenKind.Rem) {
                        while(!this.ended()) {
                            this.advance();
                            if (this.peek() == "\n") {
                                break;
                            }
                        }
                        this.start = this.current;
                        return this.parseToken();
                    } else {
                        return this.newToken(keyword);
                    }
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
    
        check(kinds: TokenKind[]) {
            for(let kind of kinds) {
                if (this.current.kind === kind) {
                    return true;
                }
            }
            return false;
        },
        
        advance() {
            this.previous = this.current;
            this.current = this.tokenizer.parseToken();
            return this.previous.kind;
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

        error(error: string, message: string) {
            throw {
                error,
                previousToken: this.previous,
                currentToken: this.current,
                message,
            }
        },

        expect(kind: TokenKind, message: string) {
            if(!this.match([kind])){
                this.error("Syntax", message);
            }
        },


        infix_bp(kind: TokenKind): BindingPower {
            switch(kind) {
                case TokenKind.Minus:
                case TokenKind.Plus:
                    return { fst: 1, snd: 2 }
                case TokenKind.Multiply:
                case TokenKind.Divide:
                    return { fst: 3, snd: 4 }
                case TokenKind.GreaterThan:
                case TokenKind.LessThan:
                case TokenKind.GreaterThanOrEqual:
                case TokenKind.LessThanOrEqual:
                    return { fst: 5, snd: 6 }
                case TokenKind.EqualEqual:
                case TokenKind.NotEqual:
                    return { fst: 7, snd: 8 }
                case TokenKind.Equal:
                    return { fst: 9, snd: 9 }
                default:
                    return { fst: 0, snd: 0 }
            }
        },

        expression_bp(min: number): Expression {
            let lhs: Expression = {} as Expression;
            if (this.match([TokenKind.LeftParenthesis])) {
                lhs = this.expression_bp(0);
                this.expect(TokenKind.RightParenthesis, "Need closing right paren!");   
            } else if (this.check([
                    TokenKind.Minus, TokenKind.Plus, TokenKind.Multiply, TokenKind.Divide,
                    TokenKind.LessThan, TokenKind.GreaterThan, TokenKind.LessThanOrEqual, TokenKind.GreaterThanOrEqual,
                    TokenKind.And, TokenKind.Or, TokenKind.Equal, TokenKind.NotEqual, TokenKind.EqualEqual,
                ])) {
                const op = this.advance();
                const {fst, snd} = this.infix_bp(op);
                const rhs = this.expression_bp(snd);
                lhs = createBinaryExpression(lhs, op, rhs);
            } else if (this.match([TokenKind.Literal])) {
                lhs = createPrimaryExpression(this.previous.literal!);
            } else if (this.match([TokenKind.Identifier])) {
                lhs = createPrimaryExpression({variableName: this.previous.lexeme })
            } else {
                this.error("Sytax", "Not a valid expression!");
            }
            
            while(true) {
                if (this.check([TokenKind.EndOfFile])) break;
                if (this.check([
                    TokenKind.Minus, TokenKind.Plus, TokenKind.Multiply, TokenKind.Divide,
                    TokenKind.LessThan, TokenKind.GreaterThan, TokenKind.LessThanOrEqual, TokenKind.GreaterThanOrEqual,
                    TokenKind.And, TokenKind.Or, TokenKind.Equal, TokenKind.NotEqual, TokenKind.EqualEqual
                ])) {
                    const op = this.current.kind;
                    const {fst, snd} = this.infix_bp(op);
                    if (fst < min) {
                        break;
                    }

                    this.advance();
                    let rhs = this.expression_bp(snd);
                    lhs = createBinaryExpression(lhs, op, rhs);

                    continue;
                }
                break;
            }

            return lhs;
        },

        expression() {
            return this.expression_bp(0);
        },

        ifStatement() {
            this.match([TokenKind.If]);
            let condition = this.expression();
            this.expect(TokenKind.Then, "Expected THEN after IF!");
            let left: Statement[] = this.blockStatement([TokenKind.Else, TokenKind.End]);
            let right: Statement[] | undefined = undefined;
            if (this.match([TokenKind.Else])) {
                right = this.blockStatement([TokenKind.End]);
            }
            this.expect(TokenKind.End, "Expected END IF at the end of IF!");
            this.expect(TokenKind.If, "Expected END IF at the end of IF!");

            return createIfStatement(condition, left, right);
        },

        gotoStatement() {
            this.match([TokenKind.Goto]);
            return createGotoStatement(this.expression());
        },

        printStatement() {
            this.match([TokenKind.Print]);
            return createPrintStatement(this.expression());
        },

        expressionStatement() {
            return createExpressionStatement(this.expression());
        },

        inputStatement() {
            this.match([TokenKind.Input]);
            let expr = this.expression();
            this.expect(TokenKind.Comma, "Expected comma after expression, then a variable name for INPUT statement!")
            this.expect(TokenKind.Identifier, "Expected identifier after comma for INPUT statement!")
            return createInputStatement(expr, this.previous);
        },

        letStatement() {
            this.match([TokenKind.Let]);
            this.expect(TokenKind.Identifier, "Expected Identifier after Let");
            let variableName = this.previous.lexeme;
            this.expect(TokenKind.Equal, "Expected = after identifiers name")
            let expression = this.expression();

            return createLetStatement(variableName, expression);
        },

        statement() {
            switch(this.current.kind) {
                case TokenKind.If: return this.ifStatement();
                case TokenKind.Print: return this.printStatement();
                case TokenKind.Goto: return this.gotoStatement();
                case TokenKind.Input: return this.inputStatement();
                case TokenKind.Let: return this.letStatement();
            }

            if (this.check([TokenKind.Literal, TokenKind.Identifier])) {
                return this.expressionStatement();
            }

            this.error("Syntax", "Cannot parse statement!");
        },

        blockStatement(until?: TokenKind[]): Statement[] {
            let program: Statement[] = [];
            while(!this.ended()) {
                let statement: Statement = this.statement() as Statement;
                program.push(statement);

                this.match([TokenKind.SemiColon]);

                if (until != undefined) {
                    for (let kind of until) {
                        if (this.check([kind])){
                            return program;
                        }
                    }
                }
            }
            return program;
        },

        compile() {
            this.current = this.tokenizer.parseToken();
            return this.blockStatement();
        }
    }
}

const Interpreter = function (_source: string) {
    let source = ``;
    let program: Record<number, number> = {};
    let programLength = 0

    let tmp: Record<string, string> = {}
    _source.split("\n").forEach(line => {
        let words = line.trim().split(" ");
        let number = words[0];
        words.shift();
        tmp[number] = words.join(" "); 
    });

    _source = ``;

    for (let key in tmp) {
        _source += key.toString() + " " + tmp[key] + "\n";
    }

    _source.split("\n").forEach(line => {
        let words = line.trim().split(" ");
        let number = parseInt(words[0]);
        words.shift();
        let code = words.join(" ");

        if (!Number.isNaN(number)) {
            source += code + "\n ";
            program[number] = programLength;
            programLength += code.length + 2;
        }
    });

    let ast = Compiler(source).compile();

    return {
        variables: {},
        interpretExpression(expression: Expression): LiteralType | undefined {
            let result: LiteralType | undefined = undefined;

            let expr;
            switch (expression.kind) {
                case Expressions.Binary:
                    expr = expression as Binary;
                    
                    if (expr.lhs.kind === Expressions.Primary) {
                        let left = expr.lhs as Primary;
                        if (typeof left.value === "object") {
                            switch(expr.op) {
                                case TokenKind.Equal:
                                    Object(this.variables)[left.value.variableName] = this.interpretExpression(expr.rhs);
                                    return result;
                            }
                        }
                    }

                    let left  = this.interpretExpression(expr.lhs);
                    let right = this.interpretExpression(expr.rhs);
                    

                    if (typeof left === "number" && typeof right === "number") {
                        switch(expr.op) {
                            case TokenKind.Plus:
                                result = left + right;
                                break;
                            case TokenKind.Minus:
                                result = left - right;
                                break;
                            case TokenKind.Multiply:
                                result = left * right;
                                break;
                            case TokenKind.Divide:
                                result = left / right;
                                break;
                            case TokenKind.LessThan:
                                result = +(left < right);
                                break
                            case TokenKind.GreaterThan:
                                result = +(left > right);
                                break;
                            case TokenKind.LessThanOrEqual:
                                result = +(left <= right);
                                break
                            case TokenKind.GreaterThanOrEqual:
                                result = +(left >= right);
                                break;
                            case TokenKind.EqualEqual:
                                result = +(left == right);
                                break
                            case TokenKind.NotEqual:
                                result = +(left != right);
                                break;
                        }
                    }
                    if (typeof left === "string" && typeof right === "string") {
                        switch(expr.op) {
                            case TokenKind.Plus:
                                result = left + right;
                                break;
                            case TokenKind.EqualEqual:
                                result = +(left == right);
                                break
                            case TokenKind.NotEqual:
                                result = +(left != right);
                                break;
                        }
                    }
                    break;
                case Expressions.Primary:
                    expr = expression as Primary;
                    if (typeof expr.value === "object") {
                        // Is variable
                        result = Object(this.variables)[expr.value.variableName];
                        if (result === undefined) {
                            throw {
                                error: "Interpret",
                                message: "Undefined variable: " + expr.value.variableName
                            }
                        }
                    } else {
                        result = expr.value;
                    }
                    break;
                case Expressions.Unary:
                    break;
            }

            return result;
        },

        interpretPrint(stmt: PrintStatement) {
            console.log(
                this.interpretExpression(stmt.expression)
            );
        },

        interpretInput(stmt: InputStatement) {
            // prompt(this.interpretExpression(stmt.expression)?.toString());
        },

        interpretIfStatement(stmt: IfStatement) {
            if (this.interpretExpression(stmt.condition)) {
                this._interpet(stmt.left);
            } else if (stmt.right != undefined) {
                this._interpet(stmt.right);
            }
        },

        interpretGoto(stmt: GotoStatement) {
            let expr = this.interpretExpression(stmt.expression) ?? 0;
            let number: number = parseInt(expr.toString());
            ast = Compiler(source.substring(program[number])).compile();
        },

        intepretLetStatement(stmt: LetStatement) {
            Object(this.variables)[stmt.variableName] = this.interpretExpression(stmt.value);
        },

        _interpet(_ast: Statement[]){
            while (_ast.length > 0) {
                let statement = _ast.shift();
                switch (statement?.kind) {
                    case Statements.PrintStatement:
                        this.interpretPrint(statement as PrintStatement);
                        break;
                    case Statements.InputStatement:
                        this.interpretInput(statement as InputStatement);
                        break;
                    case Statements.IfStatement:
                        this.interpretIfStatement(statement as IfStatement);
                        _ast = ast;
                        break;
                    case Statements.GotoStatement:
                        this.interpretGoto(statement as GotoStatement);
                        _ast = ast;
                        break
                    case Statements.ExpressionStatement:
                        let tmp = statement as ExpressionStatement;
                        this.interpretExpression(tmp.expression);
                        break
                    case Statements.LetStatement:
                        this.intepretLetStatement(statement as LetStatement);
                        break;
                }
            }
        },

        interpet(){
            this._interpet(ast)
        }
    }
}

Interpreter(`

10 LET WOW = "";
20 LET I = 0;
30 IF I < 10 THEN
31    I = (I + 1);
32    WOW = (WOW + "!");
33    GOTO 30;
34 END IF
40 PRINT WOW

`).interpet();
