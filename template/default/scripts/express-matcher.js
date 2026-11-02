// Boolean search expressions over substrings: expressionValidate checks one
// ("OK", or an ErrorInfo with the bad index), expressionParse compiles it to a
// decision graph, expressionMatch walks that graph against a string.
//
// A bare term matches when the string contains it; "&" and "|" have equal
// precedence and run strictly left to right ("a|b&c" means "(a|b)&c"), "~"
// negates what follows, parentheses group, "\" escapes an operator. Parsing
// assumes the expression already passed validation.

// State Transfer
// Rows are the current state, columns the class of the character just read.
const ST = {
    SPACE : 0, NORMAL: 1, AND_OR: 2, NOT: 3, PL: 4, PR: 5, ESC: 6, END: 7
};

// A cell holding one of these strings is a rejection; a number is a next state.
const OPT_MISS      = "Operator Missing"
const OPT_DUPL      = "Duplicate Operator"
const EMP_FIELD     = "Empty Field"
const INVALID_ESC   = "Invalid Escape"
const UNMATCH_P     = "Unmatched Parenthesis"

// The space column maps each state back to itself, so spaces never terminate a
// term -- except in esc, which accepts only & | ~ ( ) \. The end column answers
// whether the expression may stop in that state.
//             [space       , normal     , and or   , not      , left parenthesis, right parenthesis, escape   , end        ]
const space  = [ST.SPACE    , ST.NORMAL  , EMP_FIELD, ST.NOT   , ST.PL           , ST.PR            , ST.ESC   , ST.END     ]
const normal = [ST.NORMAL   , ST.NORMAL  , ST.AND_OR, OPT_MISS , OPT_MISS        , ST.PR            , ST.ESC   , ST.END     ]
const andOr  = [ST.AND_OR   , ST.NORMAL  , OPT_DUPL , ST.NOT   , ST.PL           , EMP_FIELD        , ST.ESC   , EMP_FIELD  ]
const not    = [ST.NOT      , ST.NORMAL  , EMP_FIELD, ST.NOT   , ST.PL           , EMP_FIELD        , ST.ESC   , EMP_FIELD  ]
const pl     = [ST.PL       , ST.NORMAL  , EMP_FIELD, ST.NOT   , ST.PL           , EMP_FIELD        , ST.ESC   , ST.END     ]
const pr     = [ST.PR       , OPT_MISS   , ST.AND_OR, OPT_MISS , OPT_MISS        , ST.PR            , OPT_MISS , ST.END     ]
const esc    = [INVALID_ESC , INVALID_ESC, ST.NORMAL, ST.NORMAL, ST.NORMAL       , ST.NORMAL        , ST.NORMAL, INVALID_ESC]

class ErrorInfo {
    exp
    index
    msg
    constructor(exp, index, msg) {
        this.exp = exp
        this.index = index
        this.msg = msg
    }
}

function expressionValidate(exp) {
    // Indexed by curST, so row order must match the ST values; ST.END has no row.
    var states = [space, normal, andOr, not, pl, pr, esc]
    var curST = ST.SPACE
    var openParenthesisIndex = []

    for (var i = 0; i < exp.length; ++i) {
        var c = exp[i]
        var result

        switch (c) {
            case "&":
            case "|":
                result = states[curST][ST.AND_OR]
                break
            case "~":
                result = states[curST][ST.NOT]
                break
            case "(":
                result = states[curST][ST.PL]
                // An escaped "(" is a literal and must not count as nesting.
                if (curST != ST.ESC) {
                    openParenthesisIndex.push(i)
                }
                break
            case ")":
                result = states[curST][ST.PR]
                if (curST != ST.ESC && undefined == openParenthesisIndex.pop()) {
                    result = UNMATCH_P
                }
                break
            case "\\":
                result = states[curST][ST.ESC]
                break
            case " ":
                result = states[curST][ST.SPACE]
                break
            default:
                result = states[curST][ST.NORMAL]
        }

        if (typeof(result) == "string") {
            return new ErrorInfo(exp, i, result)
        }

        curST = result
    }

    // i == exp.length here, so i - 1 blames the last character.
    result = states[curST][ST.END]
    if (typeof(result) == "string") {
        return new ErrorInfo(exp, i - 1, result)
    }

    if (openParenthesisIndex.length > 0) {
        return new ErrorInfo(exp, openParenthesisIndex.pop(), UNMATCH_P)
    }

    return "OK"
}

// A one-slot mutable cell. Every exit that should end up in the same place gets
// the same Ptr, so once shared it may only be redirected through its `.value` --
// reassigning the field pointing at it would move that one exit alone.
class Ptr {
    value
    constructor(value) {
        this.value = value
    }
}

// While parsing, successNext/failureNext are Ptr chains bottoming out at a
// MatchNode or a boolean; expressionCompress collapses each chain to that end.
class MatchNode {
    exp
    successNext
    failureNext
    constructor() {
        this.exp = ""
        this.successNext = new Ptr(new Ptr(true))
        this.failureNext = new Ptr(new Ptr(false))
    }
}

// One parenthesis level under construction:
//   t / f: the Ptr shared by every still-unbound success / failure exit, so
//          writing t.value redirects all of them at once
//   last : the node a following &, | or ~ applies to
//   head : entry node; a nested one is discarded at ")" once copied outward
class SubExpression {
    head
    last
    t
    f
    constructor() {
        this.head = new MatchNode()
        this.last = this.head
        this.t    = this.head.successNext
        this.f    = this.head.failureNext
    }
}

function expressionParse(exp) {
    var stack      = []
    var curExp     = new SubExpression()

    for (var i = 0; i < exp.length; ++i) {
        var c = exp[i]
        var next
        switch (c) {
            case "&":
                // Short-circuit wiring: the new node joins the level's failure
                // exit and takes over its pending success target.
                next = new MatchNode()
                next.failureNext = curExp.f
                next.successNext.value = curExp.t.value
                curExp.t.value = new Ptr(next)
                curExp.t = next.successNext
                curExp.last = next
                break;
            case "|":
                // Mirror of "&": failures fall through to the next alternative.
                next = new MatchNode()
                next.successNext = curExp.t
                next.failureNext.value = curExp.f.value
                curExp.f.value = new Ptr(next)
                curExp.f = next.failureNext
                curExp.last = next
                break
            case "~":
                // Swaps the fields, not the Ptrs, so curExp.t / curExp.f still
                // reference the right cells.
                [curExp.last.successNext, curExp.last.failureNext]
                = [curExp.last.failureNext, curExp.last.successNext]
                break
            case "(":
                stack.push(curExp)
                curExp = new SubExpression()
                break
            case ")":
                var exp0   = stack.pop()

                // Read before overwrite. A preceding "~" swapped these exits,
                // which is how a whole group gets negated.
                curExp.t.value = exp0.last.successNext
                curExp.f.value = exp0.last.failureNext

                // Predecessors already point at exp0.last, so copy into it.
                exp0.last.exp = curExp.head.exp
                exp0.last.successNext = curExp.head.successNext
                exp0.last.failureNext = curExp.head.failureNext

                exp0.last = curExp.last
                curExp = exp0
                break
            case "\\":
                i++;
                curExp.last.exp += exp[i]
                break
            default:
                curExp.last.exp += c
        }
    }

    return expressionCompress(curExp.head)
}

// Turns every exit into a direct MatchNode or boolean reference. Shared exits
// make this a DAG rather than a tree, hence the visited set.
function expressionCompress(matchTree) {
    var visited = new Set([matchTree])
    var nextNode = [matchTree]

    const compress = p => {
        var path = p
        while(p instanceof Ptr) p = p.value
        while(path instanceof Ptr) {
            var next = path.value
            path.value = p
            path = next
        }

        if (p instanceof MatchNode && !visited.has(p)) {
            nextNode.push(p)
            visited.add(p)
        }
        return p
    }

    for (var i = 0; i < nextNode.length; ++i) {
        const cur = nextNode[i]
        // Parsing keeps spaces, so trim only drops operator padding; inner ones count.
        cur.exp = cur.exp.trim()
        cur.successNext = compress(cur.successNext)
        cur.failureNext = compress(cur.failureNext)
    }

    return matchTree
}

// `hits`, when given, collects the terms that tested true along the path taken.
// Short-circuiting keeps untested terms out, but a negated term may still land
// there -- it is a highlighting hint, not a record of why the match succeeded.
function expressionMatch(matchTree, str, hits) {
    var next = matchTree
    while (typeof(next) != 'boolean') {
        var exp = next.exp
        var isSuccess = str.includes(exp)
        if (isSuccess && hits != undefined && exp != "") {
            hits.push(exp)
        }
        next = isSuccess ? next.successNext : next.failureNext
    }
    return next
}

export {expressionValidate, expressionParse, expressionMatch}
