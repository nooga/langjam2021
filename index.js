const util = require("util");
const arg = require("arg");
const prettier = require("prettier");
const fs = require("fs");
const path = require("path");
const vm = require("vm");

const fnil = (x, d) => x || d;

const sourcePos = ({ source, line, column }) =>
    `(${source || "input"} ${line + 1}:${column + 1})`;

const Tok = Object.freeze({
    NUM: Symbol("TokNum"),
    ID: Symbol("TokId"),
    STRING: Symbol("TokString"),
    LET: Symbol("TokLet"),
    DO: Symbol("TokDo"),
    IF: Symbol("TokIf"),
    IN: Symbol("TokIn"),
    PIPE: Symbol("TokPipe"),
    THEN: Symbol("TokThen"),
    ELSE: Symbol("TokElse"),
    FN: Symbol("TokFn"),
    DEF: Symbol("TokDef"),
    LEFT: Symbol("TokLeft"),
    RIGHT: Symbol("TokRight"),
    OPERATOR: Symbol("TokOpDef"),
    CMT: Symbol("TokComment"),
    LCMT: Symbol("TokLComment"),
    LPAR: Symbol("TokLParen"),
    RPAR: Symbol("TokRParen"),
    LCURL: Symbol("TokLCurl"),
    RCURL: Symbol("TokRCurl"),
    LSQR: Symbol("TokLSqr"),
    RSQR: Symbol("TokRSqr"),
    OP: Symbol("TokOp"),
    COMMA: Symbol("TokComma"),
    COLON: Symbol("TokColon"),
    ARROW: Symbol("TokArrow"),
    EQUALS: Symbol("TokEquals"),
    SEMICOLON: Symbol("TokSemicolon"),
    EOF: Symbol("TokEOF"),
    WS: Symbol("TokWS"),
});

const lexerRules = {
    [Tok.WS]: {
        match: /^\s+/,
        ignore: true,
        transit: (state, match) => {
            let newlines;
            if ((newlines = match.match(/\n/g))) {
                state.ws = true;
                state.line += newlines.length;
                state.column = match.length - match.lastIndexOf("\n");
            }
        },
    },
    [Tok.NUM]: {
        match: /^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?/,
        vmap: parseFloat,
    },
    [Tok.ID]: {
        match: /^[A-Za-z]+[A-Za-z0-9_?!'-]*/,
        classify: {
            let: Tok.LET,
            in: Tok.IN,
            do: Tok.DO,
            if: Tok.IF,
            then: Tok.THEN,
            else: Tok.ELSE,
            fn: Tok.FN,
            def: Tok.DEF,
            op: Tok.OPERATOR,
            left: Tok.LEFT,
            right: Tok.RIGHT,
        },
    },
    [Tok.CMT]: {
        match: /^\/\*/,
        until: /^\*\//,
        vmap: (c) => c.trim(),
        ignore: true,
    },
    [Tok.LCMT]: {
        match: /^\/\/.*$/m,
        vmap: (c) => c.trim(),
        ignore: true,
    },
    [Tok.LPAR]: {
        match: /^\(/,
    },
    [Tok.RPAR]: {
        match: /^\)/,
    },
    [Tok.LCURL]: {
        match: /^\{/,
    },
    [Tok.RCURL]: {
        match: /^\}/,
    },
    [Tok.LSQR]: {
        match: /^\[/,
    },
    [Tok.RSQR]: {
        match: /^\]/,
    },
    [Tok.COMMA]: {
        match: /^,/,
    },
    [Tok.SEMICOLON]: {
        match: /^\;+/,
    },
    [Tok.COLON]: {
        match: /^:/,
    },
    [Tok.STRING]: {
        match: /^"(?:\\"|[^"])*"/,
        vmap: (s) => JSON.parse(s),
    },
    [Tok.OP]: {
        match: /^[`~!@#$%^&*_+=:<>./?|-]+/,
        classify: {
            "=": Tok.EQUALS,
            "=>": Tok.ARROW,
            "|": Tok.PIPE,
        },
    },
};

const lexer = (rules) => (input) => {
    function* genfunc(input) {
        let offset = 0;
        let state = { ws: false, line: 0, column: 0 };
        loop: while (input.length > 0) {
            for (let r of Object.getOwnPropertySymbols(rules)) {
                const { match, vmap, ignore, until, classify, transit } =
                    rules[r];
                let [m] = fnil(input.match(match), []);
                if (m) {
                    if (until) {
                        let level = 1;
                        input = input.slice(m.length);
                        let ret = m;
                        while (level > 0 || input.length <= 0) {
                            m = (input.match(match) || [])[0];
                            if (m) {
                                input = input.slice(m.length);
                                ret += m;
                                level++;
                            }
                            m = (input.match(until) || [])[0];
                            if (m) {
                                input = input.slice(m.length);
                                ret += m;
                                level--;
                                if (level == 0) {
                                    break;
                                }
                            }
                            ret += input[0];
                            input = input.slice(1);
                        }
                        if (level != 0) {
                            return new Error(
                                `Unclosed comment starting at ${sourcePos({
                                    offset,
                                    column: state.column,
                                    line: state.line,
                                })}: ${input.substr(0, 20)}...`
                            );
                        }
                        yield {
                            kind: r,
                            value: (vmap || ((x) => x))(ret),
                            pos: {
                                offset,
                                precNL: state.ws,
                                column: state.column,
                                line: state.line,
                            },
                        };
                        offset += ret.length;
                        if (transit) {
                            transit(state, m);
                        } else {
                            state.column += m.length;
                            state.ws = false;
                        }
                        continue loop;
                    }
                    if (!ignore)
                        yield {
                            kind: classify ? classify[m] || r : r,
                            value: (vmap || ((x) => x))(m),
                            pos: {
                                offset,
                                precNL: state.ws,
                                column: state.column,
                                line: state.line,
                            },
                        };
                    input = input.slice(m.length);
                    offset += m.length;
                    if (transit) {
                        transit(state, m);
                    } else {
                        state.column += m.length;
                        state.ws = false;
                    }
                    continue loop;
                }
            }
            return new Error(
                `Unexpected input at ${sourcePos({
                    offset: state.offset,
                    column: state.column,
                    line: state.line,
                })}: ☞${input.substr(0, 20)}...`
            );
        }
        return {
            kind: Tok.EOF,
            pos: {
                offset,
                column: state.column,
                line: state.line,
                precNL: state.ws,
            },
        };
    }

    const gen = genfunc(input);
    return {
        input,
        gen,
        cur: gen.next(),
        peek() {
            return this.cur.value;
        },
        is(t) {
            return this.peek().kind == t;
        },
        isNot(t) {
            return !this.is(t);
        },
        report(n = 40) {
            const { offset } = this.peek().pos;
            let ret = "",
                start = offset,
                end = offset;
            for (let i = 0; i < n; i++) {
                if (offset + i >= this.input.length) break;
                const c = this.input[offset + i];
                if (c == "\n") break;
                end++;
            }
            for (let i = 1; i < n; i++) {
                if (offset - i < 0) break;

                const c = this.input[offset - i];
                if (c == "\n") break;
                start--;
            }
            ret = "┃ " + this.input.substr(start, end - start) + "\n┃ ";
            for (let i = 0; i < offset - start; i++) ret += " ";
            ret += "☝︎";
            return ret;
        },
        accept(kind) {
            if (this.peek().kind == kind) {
                this.next();
                return this.peek();
            } else
                throw new Error(
                    `SyntaxError: Expected ${kind.toString()} at ${sourcePos(
                        this.peek().pos
                    )} but got:\n${this.report()} `
                );
        },
        next() {
            this.cur = this.gen.next();
            //console.log(this.cur);
            if (this.cur.error) throw this.cur.error;
        },
    };
};

const Assoc = Object.freeze({
    LEFT: Symbol("Assoc.LEFT"),
    RIGHT: Symbol("Assoc.RIGHT"),
});

const ops = {
    $: { prec: 1, assoc: Assoc.RIGHT },
    "|>": { prec: 1, assoc: Assoc.LEFT },
    "+": { prec: 20, assoc: Assoc.LEFT, js: true },
    "-": { prec: 20, assoc: Assoc.LEFT, js: true },
    "*": { prec: 30, assoc: Assoc.LEFT, js: true },
    "/": { prec: 30, assoc: Assoc.LEFT, js: true },
    "^": { prec: 40, assoc: Assoc.RIGHT },
    "==": { prec: 15, assoc: Assoc.LEFT, js: true },
    "!=": { prec: 15, assoc: Assoc.LEFT, js: true },
    ">": { prec: 16, assoc: Assoc.LEFT, js: true },
    "<": { prec: 16, assoc: Assoc.LEFT, js: true },
    ">=": { prec: 16, assoc: Assoc.LEFT, js: true },
    "<=": { prec: 16, assoc: Assoc.LEFT, js: true },
    "==": { prec: 17, assoc: Assoc.LEFT, js: true },
    "!=": { prec: 17, assoc: Assoc.LEFT, js: true },
    "||": { prec: 18, assoc: Assoc.LEFT, js: true },
    "@": { prec: 1000, assoc: Assoc.RIGHT },
    APPLY: { prec: 1000, assoc: Assoc.LEFT },
    ".": { prec: 2000, assoc: Assoc.LEFT, js: true },
};

const isTerminator = (t) => {
    return (
        t.kind == Tok.RPAR ||
        t.kind == Tok.RCURL ||
        t.kind == Tok.RSQR ||
        t.kind == Tok.SEMICOLON ||
        t.kind == Tok.COMMA ||
        t.kind == Tok.COLON ||
        t.kind == Tok.THEN ||
        t.kind == Tok.ELSE ||
        t.kind == Tok.EOF ||
        t.kind == Tok.IN ||
        t.kind == Tok.PIPE ||
        t.kind == Tok.EQUALS ||
        t.pos.precNL
    );
};

const astComments = (ast, c) => {
    if (!c || c.length == 0) return ast;
    if (!ast.comments) ast.comments = [];
    if (Array.isArray(c)) ast.comments = ast.comments.concat(c);
    else ast.comments.push(c);
    return ast;
};

const eatComments = (ctx, all) => {
    const { tok } = ctx;
    const comments = [];
    while (true) {
        if (tok.is(Tok.CMT) || tok.is(Tok.LCMT)) {
            if (!all && tok.peek().pos.precNL) break;
            comments.push(tok.peek().value);
            tok.accept(tok.peek().kind);
        } else break;
    }
    return comments;
};

const parseExpr = (ctx, minPrec) => {
    const { optable, tok } = ctx;
    let ret = parseAtom(ctx); // eats comments
    while (true) {
        const cur = tok.peek();
        //console.log(cur, isTerminator(cur));
        let op = null;
        if (cur.kind == Tok.OP) {
            if (optable[cur.value].prec < minPrec) break;
            op = cur.value;
            tok.accept(Tok.OP);
        } else if (isTerminator(cur)) {
            //console.log("D");
            break;
        } else {
            op = "APPLY";
            if (optable[op].prec < minPrec) break;
        }

        let comments = eatComments(ctx);

        const { prec, assoc } = optable[op];
        let rhs = parseExpr(ctx, assoc == Assoc.LEFT ? prec + 1 : prec);
        ret = {
            t: "ap",
            op,
            lhs: ret,
            rhs: rhs,
        };
        if (comments.length > 0) astComments(ret, comments);
    }
    return ret;
};

const parseDo = (ctx) => {
    const { tok } = ctx;
    const ret = { t: "do", exprs: [] };
    tok.accept(Tok.DO);
    let postDoComments = eatComments(ctx);
    tok.accept(Tok.LCURL);
    let postOpenComments = eatComments(ctx);
    let exprsComments = [];
    while (tok.isNot(Tok.RCURL)) {
        let preExp = eatComments(ctx);
        exprsComments = exprsComments.concat(preExp);
        let expr = parseExpr(ctx, 1);
        if (expr.comments) exprsComments = exprsComments.concat(expr.comments);
        astComments(expr, preExp);
        ret.exprs.push(expr);
        if (tok.isNot(Tok.RCURL)) {
            if (tok.is(Tok.SEMICOLON)) tok.accept(Tok.SEMICOLON);
        } else break;
    }
    let preCloseComments = eatComments(ctx);
    if (tok.is(Tok.RCURL)) tok.accept(Tok.RCURL);
    else
        throw new Error(
            `SyntaxError: Expected \`}\` at ${sourcePos(
                tok.peek().pos
            )} to close \`}\` from ${sourcePos(start.pos)}`
        );
    let allComments = postDoComments
        .concat(postOpenComments)
        .concat(exprsComments)
        .concat(preCloseComments);
    astComments(ret, allComments);
    return ret;
};

const parseLet = (ctx) => {
    const { tok } = ctx;
    const ret = { t: "let", eqns: [], exp: null };
    tok.accept(Tok.LET);
    while (tok.isNot(Tok.IN)) {
        ret.eqns.push(parseEquation(ctx));
        if (tok.isNot(Tok.IN)) {
            if (tok.is(Tok.SEMICOLON)) tok.accept(Tok.SEMICOLON);
        } else break;
    }
    tok.accept(Tok.IN);
    ret.exp = parseExpr(ctx, 1);
    return ret;
};

const parseIf = (ctx) => {
    const { tok } = ctx;
    const ret = { t: "if", cond: null, then: null, els: { t: "nil" } };
    tok.accept(Tok.IF);
    let postIfComments = eatComments(ctx);
    ret.cond = astComments(parseExpr(ctx, 1), postIfComments);
    tok.accept(Tok.THEN);
    let postThenComments = eatComments(ctx);
    ret.then = astComments(parseExpr(ctx, 1), postThenComments);
    let postElseComments = [];
    if (tok.is(Tok.ELSE)) {
        tok.accept(Tok.ELSE);
        postElseComments = eatComments(ctx);
        ret.els = astComments(parseExpr(ctx, 1), postElseComments);
    }
    let allComments = postIfComments
        .concat(postThenComments)
        .concat(postElseComments);
    return ret;
};

const parseArg = (ctx) => {
    const { tok } = ctx;
    const cur = tok.peek();
    eatComments(ctx);
    switch (cur.kind) {
        case Tok.ID:
            tok.accept(Tok.ID);
            return { t: "id", value: cur.value };
        default:
            throw new Error(
                `SyntaxError: Expected formal argument at ${sourcePos(
                    tok.peek().pos
                )} but got this instead:\n${tok.report()}`
            );
    }
};

const parseEquation = (ctx) => {
    const { tok } = ctx;
    const ret = {
        t: "eqn",
        lhs: { t: "pattern", name: null, args: [] },
        rhs: null,
    };
    eatComments(ctx);
    if (tok.isNot(Tok.ID)) {
        if (tok.is(Tok.LPAR)) {
            tok.accept(Tok.LPAR);
            if (tok.isNot(Tok.OP))
                throw new Error(
                    `SyntaxError: Expected operator at ${sourcePos(
                        tok.peek().pos
                    )} but got this instead:\n${tok.report()}`
                );
            //console.log(tok.peek());
            ret.lhs.name = tok.peek().value;
            tok.accept(Tok.OP);
            tok.accept(Tok.RPAR);
        } else
            throw new Error(
                `SyntaxError: Expected identifier at ${sourcePos(
                    tok.peek().pos
                )} but got this instead:\n${tok.report()}`
            );
    } else {
        ret.lhs.name = tok.peek().value;
        tok.accept(Tok.ID);
    }
    eatComments(ctx);
    while (tok.isNot(Tok.EQUALS) && tok.isNot(Tok.PIPE)) {
        ret.lhs.args.push(parseArg(ctx));
    }

    if (tok.is(Tok.PIPE)) {
        let options = [];
        tok.accept(Tok.PIPE);
        let els = null;
        while (true) {
            let cond = null;
            if (tok.is(Tok.ELSE)) tok.accept(Tok.ELSE);
            else cond = parseExpr(ctx, 1);
            tok.accept(Tok.EQUALS);
            const expr = parseExpr(ctx, 1);
            if (!cond) els = expr;
            else options.push({ cond, expr });
            if (tok.isNot(Tok.PIPE)) {
                break;
            }
            tok.accept(Tok.PIPE);
        }

        ret.rhs = els || { t: "nil" };
        for (let i = options.length - 1; i >= 0; i--)
            ret.rhs = {
                t: "if",
                cond: options[i].cond,
                then: options[i].expr,
                els: ret.rhs,
            };
    } else {
        tok.accept(Tok.EQUALS);
        let exprComments = eatComments(ctx);
        ret.rhs = astComments(parseExpr(ctx, 1), exprComments);
    }
    return ret;
};

// this mutates the optable :shrug:
const parseOpDef = (ctx) => {
    const { optable, tok } = ctx;
    tok.accept(Tok.OPERATOR);
    eatComments(ctx);
    if (tok.isNot(Tok.OP))
        throw new Error(
            `SyntaxError: Expected operator at ${sourcePos(
                tok.peek().pos
            )} but got this instead:\n${tok.report()}`
        );

    const op = tok.peek().value;
    tok.accept(Tok.OP);
    eatComments(ctx);
    if (tok.isNot(Tok.NUM))
        throw new Error(
            `SyntaxError: Expected a number (operator precedence) at ${sourcePos(
                tok.peek().pos
            )} but got this instead:\n${tok.report()}`
        );

    const prec = tok.peek().value;
    if (prec < 1 || Math.floor(prec) != prec)
        throw new Error(
            `SyntaxError: Invalid operator precedence number at ${sourcePos(
                tok.peek().pos
            )}: ${prec}`
        );

    tok.accept(Tok.NUM);
    let assoc = null;
    eatComments(ctx);
    if (tok.is(Tok.LEFT)) {
        assoc = Assoc.LEFT;
        tok.accept(Tok.LEFT);
    } else if (tok.is(Tok.RIGHT)) {
        assoc = Assoc.RIGHT;
        tok.accept(Tok.RIGHT);
    } else
        throw new Error(
            `SyntaxError: Expected \`left\` or \`right\` at ${sourcePos(
                tok.peek().pos
            )} but got this:\n${tok.report()}`
        );
    eatComments(ctx);
    optable[op] = { prec, assoc };
    return { t: "void" }; // not sure if good idea
};

const parseDef = (ctx) => {
    const { tok } = ctx;
    tok.accept(Tok.DEF);
    return parseEquation(ctx);
};

const parseAtom = (ctx) => {
    const { tok } = ctx;
    const atom = parseAtomRaw(ctx);
    if (
        atom.t == "id" ||
        atom.t == "vec" ||
        atom.t == "map" ||
        atom.t == "nil"
    ) {
        if (tok.is(Tok.ARROW)) {
            tok.accept(Tok.ARROW);
            return { t: "lambda", arg: atom, body: parseExpr(ctx, 1) };
        }
    }
    if (tok.is(Tok.CMT) || tok.is(Tok.LCMT)) {
        astComments(atom, tok.peek().value);
        tok.accept(tok.peek().kind);
    }
    return atom;
};

const parseMapKey = (ctx) => {
    const { tok } = ctx;
    switch (tok.peek().kind) {
        case Tok.ID:
        case Tok.STRING:
        case Tok.NUM:
            return parseAtomRaw(ctx);
        default:
            throw new Error(
                `SyntaxError: Expected map key at ${sourcePos(
                    tok.peek().pos
                )} but got this:\n${tok.report()}`
            );
    }
};

const parseAtomRaw = (ctx) => {
    const { optable, tok } = ctx;
    let ret = null;
    const cur = tok.peek();
    switch (cur.kind) {
        case Tok.NUM:
            tok.accept(Tok.NUM);
            return { t: "num", value: cur.value };
        case Tok.STRING:
            tok.accept(Tok.STRING);
            return { t: "string", value: cur.value };
        case Tok.ID:
            tok.accept(Tok.ID);
            return { t: "id", value: cur.value };
        case Tok.DO:
            return parseDo(ctx);
        case Tok.IF:
            return parseIf(ctx);
        case Tok.LET:
            return parseLet(ctx);
        case Tok.LPAR: {
            let start = tok.accept(Tok.LPAR);
            if (tok.is(Tok.RPAR)) {
                tok.accept(Tok.RPAR);
                return { t: "nil" };
            }
            if (tok.is(Tok.OP)) {
                const op = tok.peek();
                tok.accept(Tok.OP);
                const opd = optable[op.value];
                if (tok.is(Tok.RPAR)) {
                    tok.accept(Tok.RPAR);
                    return { t: "op", op: op.value };
                } else {
                    const rhs = parseExpr(
                        ctx,
                        opd.assoc == Assoc.LEFT ? opd.prec + 1 : opd.prec
                    );
                    tok.accept(Tok.RPAR);
                    return {
                        t: "ap",
                        op: "APPLY",
                        lhs: { t: "op", op: op.value, swap: true },
                        rhs,
                    };
                }
            }
            ret = parseExpr(ctx, 1);
            if (tok.is(Tok.RPAR)) tok.accept(Tok.RPAR);
            else
                throw new Error(
                    `SyntaxError: Expected \`)\` at ${sourcePos(
                        tok.peek().pos
                    )} to close ``)`` from ${sourcePos(start.pos)}`
                );

            return ret;
        }
        case Tok.LSQR: {
            let ret = { t: "vec", vals: [] };
            let start = tok.accept(Tok.LSQR);
            if (tok.is(Tok.RSQR)) {
                tok.accept(Tok.RSQR);
                return ret;
            }
            while (true) {
                ret.vals.push(parseExpr(ctx, 1));
                if (tok.is(Tok.COMMA)) tok.accept(Tok.COMMA);
                if (tok.is(Tok.RSQR)) {
                    break;
                }
            }
            if (tok.is(Tok.RSQR)) tok.accept(Tok.RSQR);
            else
                throw new Error(
                    `SyntaxError: Expected \`]\` at ${sourcePos(
                        tok.peek().pos
                    )} to close \`[\` from ${sourcePos(start.pos)}`
                );

            return ret;
        }
        case Tok.LCURL: {
            let ret = { t: "map", pairs: [] };
            let start = tok.accept(Tok.LCURL);
            if (tok.is(Tok.RCURL)) {
                tok.accept(Tok.RCURL);
                return ret;
            }
            while (true) {
                let key = parseMapKey(ctx);
                let val = null;
                if (tok.is(Tok.COLON)) {
                    tok.accept(Tok.COLON);
                    val = parseExpr(ctx, 1);
                }
                ret.pairs.push([key, val]);
                if (tok.is(Tok.COMMA)) tok.accept(Tok.COMMA);
                if (tok.is(Tok.RCURL)) {
                    break;
                }
            }
            if (tok.is(Tok.RCURL)) tok.accept(Tok.RCURL);
            else
                throw new Error(
                    `SyntaxError: Expected \`}\` at ${sourcePos(
                        tok.peek().pos
                    )} to close \`{\` from ${sourcePos(start.pos)}`
                );

            return ret;
        }
        default:
            throw new Error(
                `SyntaxError: Expected either number, identifier, \`(\` or \`do\` at ${sourcePos(
                    cur.pos
                )} but got:\n${tok.report()}`
            );
    }
};

const parseForm = (ctx) => {
    const { tok } = ctx;
    let comments = eatComments(ctx, true);
    const cur = tok.peek();
    if (cur.kind == Tok.EOF) return { t: "void" };
    let ret = null;

    switch (cur.kind) {
        case Tok.OPERATOR:
            ret = parseOpDef(ctx);
            break;
        case Tok.DEF:
            ret = parseDef(ctx);
            break;
        default:
            ret = parseExpr(ctx, 1);
    }
    if (!tok.is(Tok.EOF)) {
        if (tok.is(Tok.SEMICOLON)) tok.accept(Tok.SEMICOLON);
    }
    comments = comments.concat(eatComments(ctx));
    return astComments(ret, comments);
};

const parseProgram = (ctx) => {
    const { tok } = ctx;
    const ret = { t: "program", forms: [] };
    try {
        while (tok.isNot(Tok.EOF)) {
            const r = parseForm(ctx);
            if (r.t != "void") ret.forms.push(r);
        }
        return ret;
    } catch (e) {
        //console.log(e);
        return null;
    }
};

const debugAST = (ast, indent = 0) => {
    if (!ast) return "<failed parse>";
    let p = "";
    for (let i = 0; i < indent; i++) p += " ";
    p += ast.t + " " + ast.comments + ": ";
    switch (ast.t) {
        case "num":
        case "string":
        case "id":
            p += ast.value;
            break;
        case "ap":
            p += ast.op + "\n";
            p += debugAST(ast.lhs, indent + 1);
            p += debugAST(ast.rhs, indent + 1);
            break;
        case "op":
            p += ast.op + "\n";
            break;
        case "do":
            p += "\n";
            for (let e of ast.exprs) p += debugAST(e, indent + 1);
            break;
        case "let":
            p += "\n";
            for (let e of ast.eqns) p += debugAST(e, indent + 1);
            p += debugAST(ast.exp);
            break;
        case "vec":
            p += "\n";
            for (let e of ast.vals) p += debugAST(e, indent + 1);
            break;
        case "program":
            p += "\n";
            for (let e of ast.forms) p += debugAST(e, indent + 1);
            break;
        case "eqn":
            p += `${ast.lhs.name}\n`;
            for (let e of ast.lhs.args) p += debugAST(e, indent + 1);
            p += debugAST(ast.rhs, indent + 1);
            break;
        case "if":
            p += "\n";
            p += debugAST(ast.cond, indent + 1);
            p += debugAST(ast.then, indent + 1);
            p += debugAST(ast.els, indent + 1);
            break;
        case "nil":
            p += "nil";
            break;
        case "void":
            p += "<void>";
            break;
        default:
            p += "??";
    }
    if (p[p.length - 1] != "\n") p += "\n";
    return p;
};

const munge = (s) => {
    return s.replace(/[`~!@#%^&*_+=:<>./?|-]/g, (s) => {
        return {
            "`": "_BT_",
            "~": "_TD_",
            "!": "_EX_",
            "@": "_AT_",
            "#": "_HS_",
            "%": "_PC_",
            "^": "_CT_",
            "&": "_AA_",
            "*": "_ST_",
            "=": "_EQ_",
            ":": "_CO_",
            "<": "_LT_",
            ">": "_GT_",
            ".": "_DT_",
            "/": "_SL_",
            "?": "_QM_",
            "-": "_DH_",
            "+": "_PS_",
            "|": "_PI_",
        }[s];
    });
};

const isJSOp = ({ optable }, s) => {
    if (optable[s]) return optable[s].js;
    else return false;
};

const freshVar = (ctx) => {
    if (!ctx.varc) ctx.varc = 0;
    ctx.varc++;
    return `__${ctx.varc}`;
};

const wrapComment = (ast, s) => {
    let ret = s;
    if (ast.comments) {
        for (let c of ast.comments) {
            ret = `RT.WC(${s})(${JSON.stringify(c)})`;
        }
    }
    return ret;
};

const compile = (ctx, ast) => {
    switch (ast.t) {
        case "program": {
            let ret = "";
            ret += "(()=>{\n";
            for (let i = 0; i < ast.forms.length; i++) {
                const f = ast.forms[i];
                if (i == ast.forms.length - 1)
                    ret += "return (" + compile(ctx, f) + ");\n";
                else ret += compile(ctx, f) + ";\n";
            }
            if (ast.forms.length == 0) {
                ret += "return _NIL_;";
            }
            ret += "})();\n";
            return wrapComment(ast, ret);
        }
        case "let": {
            let ret = "";
            ret += "(()=>{\n";
            for (let i = 0; i < ast.eqns.length; i++) {
                const f = ast.eqns[i];
                ret += compile(ctx, f) + ";\n";
            }
            ret += `return (${compile(ctx, ast.exp)}); })()\n`;
            return wrapComment(ast, ret);
        }
        case "do": {
            let ret = "";
            ret += "(()=>{\n";
            for (let i = 0; i < ast.exprs.length; i++) {
                const f = ast.exprs[i];

                if (i == ast.exprs.length - 1)
                    ret += "return (" + compile(ctx, f) + ");\n";
                else ret += compile(ctx, f) + ";\n";
            }
            if (ast.exprs.length == 0) {
                ret += "return _NIL_;";
            }
            ret += "})()\n";
            return wrapComment(ast, ret);
        }
        case "vec": {
            let ret = "[";
            for (let i = 0; i < ast.vals.length; i++) {
                ret +=
                    compile(ctx, ast.vals[i]) +
                    (i < ast.vals.length - 1 ? "," : "");
            }
            ret += "]";
            return wrapComment(ast, ret);
        }
        case "map": {
            let ret = "({";
            for (let i = 0; i < ast.pairs.length; i++) {
                let [key, val] = ast.pairs[i];
                let keyc = null;
                if (key.t == "string" || key.t == "id")
                    keyc = compile(ctx, key);
                else keyc = `[${compile(ctx, key)}]`;
                let valc = "";
                if (val) valc = `:${compile(ctx, val)}`;
                ret += keyc + valc + (i < ast.pairs.length - 1 ? "," : "");
            }
            ret += "})";
            return wrapComment(ast, ret);
        }
        case "eqn": {
            let lhs = `const ${munge(ast.lhs.name)} =`;
            let ret = "";
            for (let a of ast.lhs.args) ret += ` (${compile(ast, a)}) => `;
            ret += compile(ctx, ast.rhs);
            return lhs + wrapComment(ast, ret);
        }
        case "if":
            return wrapComment(
                ast,
                `((${compile(ctx, ast.cond)})?(${compile(
                    ctx,
                    ast.then
                )}):(${compile(ctx, ast.els)}))`
            );
        case "ap":
            let op = ast.op;
            if (op == "APPLY" || op == "$")
                return wrapComment(
                    ast,
                    `(${compile(ctx, ast.lhs)})(${compile(ctx, ast.rhs)})`
                );

            if (isJSOp(ctx, op)) {
                if (op == ".") {
                    if (ast.rhs.t == "id")
                        return wrapComment(
                            ast,
                            `((${compile(ctx, ast.lhs)})${op}${compile(
                                ctx,
                                ast.rhs
                            )})`
                        );
                    return wrapComment(
                        ast,
                        `RT.F(${compile(ctx, ast.lhs)})(${compile(
                            ctx,
                            ast.rhs
                        )})`
                    );
                }
                return wrapComment(
                    ast,
                    `((${compile(ctx, ast.lhs)})${op}(${compile(
                        ctx,
                        ast.rhs
                    )}))`
                );
            }

            return wrapComment(
                ast,
                `(${munge(op)})(${compile(ctx, ast.lhs)})(${compile(
                    ctx,
                    ast.rhs
                )})`
            );
        case "op":
            if (isJSOp(ctx, ast.op)) {
                const lhs = freshVar(ctx);
                const rhs = freshVar(ctx);
                if (ast.swap) {
                    if (ast.op == ".")
                        return wrapComment(
                            ast,
                            `((${lhs}) => (${rhs}) => RT.F(${rhs})(${lhs}))`
                        );

                    return wrapComment(
                        ast,
                        `((${lhs}) => (${rhs}) => ${rhs} ${ast.op} ${lhs})`
                    );
                }
                return wrapComment(
                    ast,
                    `((${lhs}) => (${rhs}) => ${lhs} ${ast.op} ${rhs})`
                );
            }
            if (ast.swap) {
                const lhs = freshVar(ctx);
                const rhs = freshVar(ctx);
                return wrapComment(
                    ast,
                    `((${lhs}) => (${rhs}) => ${munge(
                        ast.op
                    )}.valueOf()(${rhs})(${lhs}))`
                );
            }
            return wrapComment(ast, `${munge(ast.op)}`);
        case "id":
            return wrapComment(ast, `${munge(ast.value)}`);
        case "num":
            return wrapComment(ast, `${ast.value}`);
        case "string":
            return wrapComment(ast, JSON.stringify(ast.value));
        case "nil":
            return wrapComment(ast, "_NIL_");
        case "lambda":
            let arg = "";
            if (ast.arg.t != "nil") arg = compile(ctx, ast.arg);
            if (ast.arg.t != "map") arg = `(${arg})`;
            return `(${arg}=>(${compile(ctx, ast.body)}))`;
        default:
            throw new Error("compile error");
    }
};

const RT = {
    F(o) {
        return (f) => {
            const v = o[f];
            if (typeof v == "function") return v.bind(o);
            else return v;
        };
    },
    println(x) {
        console.log(x);
        return x;
    },
    map(f) {
        return (c) => c.map(f);
    },
    reduce(f) {
        return (c) => c.reduce((a, b) => f(a)(b));
    },
    range(n) {
        let ret = [];
        for (let i = 0; i < n; i++) ret.push(i);
        return ret;
    },
    not(x) {
        return !x;
    },
    all_QM_(p) {
        return (c) => {
            for (let x of c) {
                if (!p(x)) return false;
            }
            return true;
        };
    },
    curry2: (f) => (x) => (y) => f(x, y),
};

const _NIL_ = null;

const _PI__GT_ = (lhs) => (rhs) => {
    return rhs(lhs);
};

const _AT_ = (f) => (g) => {
    return (x) => f(g(x));
};

const globalObj = { RT, _NIL_, _PI__GT_, _AT_ };
vm.createContext(globalObj);

const evalL = (s) => {
    return vm.runInContext(s, globalObj);
};

const compileModule = (context, filename) => {
    try {
        const input = fs.readFileSync(filename, { encoding: "utf-8" });
        const tokens = lexer(lexerRules)(input);
        const context = { optable: ops, tok: tokens };
        const parsed = parseProgram(context, 1);
        const raw = compile(context, parsed);
        const compiled = prettier.format(raw, { parser: "babel" });
        return compiled;
    } catch (e) {
        console.log(e);
        return null;
    }
};

const repl = () => {
    console.log("TODO");
};

const runFile = (f) => {
    return evalL(compileModule({}, f));
};

const main = (args) => {
    const colors = !args["--nocolor"];

    if (args._.length == 0) {
        repl();
        return;
    }

    const command = args._[0];

    let result = null;
    switch (command) {
        case "compile":
        case "c":
            const f = args._[1];
            const compiled = compileModule({}, f);
            let outputFile = path.basename(f, ".iv") + ".js";
            if (args["--output"] == "-") {
                outputFile = null;
            } else if (arg["--output"]) {
                outputFile = args["--output"];
            }
            if (!outputFile) {
                console.log(compiled);
            } else fs.writeFileSync(outputFile, compiled);
            break;
        case "repl":
        case "i":
            result = repl();
            break;
        case "run":
        case "r":
            result = runFile(args._[1]);
            break;
        default:
            result = runFile(command);
    }

    if (args["--return"]) {
        util.inspect(result, {
            colors: colors,
            depth: Infinity,
        });
    }
};

main(
    arg({
        "--return": Boolean,
        "-R": "--return",

        "--ast": Boolean,
        "-A": "--ast",

        "--raw": Boolean,
        "-P": "--raw",

        "--output": String,
        "-o": "--output",

        "--nocolor": Boolean,
    })
);

// vain file.iv
// vain run file.iv
// vain r file.iv
// --- run file.iv

// vain compile file.iv
// vain c file.iv
// --- compile file.iv -> file.js

// vain c file.iv -o foo.js
// --- compile file.iv -> foo.js

// vain repl
// vain i
// --- launch repl

// vain repl file.iv
// --- launch repl and run file.iv

// console.log(
//     "returned value:\n" +
//         util.inspect(evalL(compiled), {
//             colors: true,
//             depth: Infinity,
//         })
// );
