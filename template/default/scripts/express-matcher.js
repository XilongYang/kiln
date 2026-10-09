// State Transfer
const ST = {
    INIT   : 0,
    NORMAL : 1,
    AND_OR : 2,
    NOT    : 3,
    PL     : 4,
    PR     : 5,
    ESC    : 6
};

const OPT_MISS      = "Operator Missing"
const OPT_DUPL      = "Duplicate Operator"
const EMP_FIELD     = "Empty Field"
const INVALID_ESC   = "Invalid Escape"
const UNMATCH_P     = "Unmatched Parenthesis"

//             [init   , normal     , and or   , not      , left parenthesis, right parenthesis, escape   ]
const init   = [ST.INIT, ST.NORMAL  , EMP_FIELD, ST.NOT   , ST.PL           , ST.PR            , ST.ESC   ]
const normal = [ST.INIT, ST.NORMAL  , ST.AND_OR, OPT_MISS , OPT_MISS        , ST.PR            , ST.ESC   ]
const andOr  = [ST.INIT, ST.NORMAL  , OPT_DUPL , ST.NOT   , ST.PL           , EMP_FIELD        , ST.ESC   ]
const not    = [ST.INIT, ST.NORMAL  , EMP_FIELD, ST.NOT   , ST.PL           , EMP_FIELD        , ST.ESC   ]
const pl     = [ST.INIT, ST.NORMAL  , EMP_FIELD, ST.NOT   , ST.PL           , EMP_FIELD        , ST.ESC   ]
const pr     = [ST.INIT, OPT_MISS   , ST.AND_OR, OPT_MISS , OPT_MISS        , ST.PR            , ST.ESC   ]
const esc    = [ST.INIT, INVALID_ESC, ST.NORMAL, ST.NORMAL, ST.NORMAL       , ST.NORMAL        , ST.NORMAL]

class Ptr {
    value
    constructor(value) {
        this.value = value
    }
}

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
    var states = [init, normal, andOr, not, pl, pr, esc]
    var curST = ST.INIT

    var openParenthesisIndex = []

    for (var i = 0; i < exp.length; ++i) {
        var c = exp[i]
        var result = "null"

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
                result = curST
                break
            default:
                result = states[curST][ST.NORMAL]
        }

        if (typeof(result) == "string") {
            return new ErrorInfo(exp, i, result)
        }

        curST = result
    }

    if (curST == ST.AND_OR || curST == ST.NOT) {
        return new ErrorInfo(exp, exp.length - 1, EMP_FIELD)
    }

    if (curST == ST.ESC) {
        return new ErrorInfo(exp, exp.length - 1, INVALID_ESC)
    }

    if (openParenthesisIndex.length > 0) {
        return new ErrorInfo(exp, openParenthesisIndex.pop(), UNMATCH_P)
    }

    return "OK"
}

function expressionParse(exp) {
    var stack      = []
    var head       = new MatchNode()
    var cur        = head

    for (var i = 0; i < exp.length; ++i) {
        var c = exp[i]
        switch (c) {
            case "&":
                var next = new MatchNode()
                if (cur.successNext.value.value == true) {
                    next.failureNext = cur.failureNext
                    next.successNext.value = cur.successNext.value
                    cur.successNext.value = new Ptr(next)
                } else {
                    next.failureNext = cur.successNext
                    next.successNext.value = cur.failureNext.value
                    cur.failureNext.value = new Ptr(next)
                }
                cur = next
                break;
            case "|":
                var next = new MatchNode()
                if (cur.successNext.value.value == true) {
                    next.successNext = cur.successNext
                    next.failureNext.value = cur.failureNext.value
                    cur.failureNext.value = new Ptr(next)
                } else {
                    next.successNext = cur.failureNext
                    next.failureNext.value = cur.successNext.value
                    cur.successNext.value = new Ptr(next)
                }
                cur = next
                break
            case "~":
                var temp = cur.failureNext
                cur.failureNext = cur.successNext
                cur.successNext = temp
                break
            case "(":
                stack.push([head, cur])
                head = new MatchNode()
                cur = head
                break
            case ")":
                var target = stack.pop()
                var head0  = target[0]
                var cur0   = target[1]

                cur.failureNext.value = cur0.failureNext.value
                cur.successNext.value = cur0.successNext.value

                cur0.failureNext = head.failureNext
                cur0.successNext = head.successNext
                cur0.exp = head.exp

                head = head0
                break
            case "\\":
                i++;
                cur.exp += exp[i]
                break
            default:
                cur.exp += c
        }
    }

    return head
}

function expressionMatch(matchTree, str) {
    var isSuccess = str.includes(matchTree.exp.trim())

    if (isSuccess) {
        var next = matchTree.successNext.value.value
        if (typeof(next) == 'boolean') {
            return next;
        }
        return expressionMatch(next, str)
    }

    var next = matchTree.failureNext.value.value
    if (typeof(next) == 'boolean') {
        return next;
    }
    return expressionMatch(next, str)
}



export {expressionValidate, expressionParse, expressionMatch}
