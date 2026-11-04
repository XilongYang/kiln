import {expressionValidate, expressionParse} from './express-matcher.js'

// A contenteditable box holding a search expression: read it back, mark a bad
// character in place, commit on Enter or blur. The post filter and the search
// panel each drive one, so a caller only says what to do with the tree.

// What the user typed. The error tooltip is a child of the box, hidden by CSS
// alone, so its text is skipped rather than read back as part of the
// expression. A contenteditable keeps NBSP where a plain space would not
// survive as markup, so fold it back.
function inputText(el) {
    var text = ""
    for (var node of el.childNodes) {
        if (node.nodeType != Node.ELEMENT_NODE) {
            text += node.textContent
        } else if (!node.classList.contains('error-msg')) {
            text += inputText(node)
        }
    }
    return text.replace(/\n/g, "").replace(/\u00a0/g, " ")
}

function moveCursorToEnd(target) {
    const range = document.createRange();
    const selection = window.getSelection();

    range.selectNodeContents(target);
    range.collapse(false);
    selection.removeAllRanges();
    selection.addRange(range);
}

// Nodes rather than a string: the expression is shown back inside the box it
// came from, so anything tag-shaped has to stay text -- as innerHTML,
// "C++ & vector<int> &" loses its <int>.
function showError(el, info) {
    var errorMsg = document.createElement('span')
    errorMsg.className = 'error-msg'
    errorMsg.textContent = "Error: " + info.msg

    var error = document.createElement('span')
    error.className = 'error'
    // A bare space would collapse away, leaving nothing to highlight.
    error.textContent = info.exp[info.index] == ' ' ? '\u00a0' : info.exp[info.index]
    error.appendChild(errorMsg)

    var invalid = document.createElement('span')
    invalid.className = 'invalid-expression'
    invalid.textContent = info.exp.slice(info.index + 1)

    el.textContent = info.exp.slice(0, info.index)
    el.appendChild(error)
    el.appendChild(invalid)
}

// Answers whether a marker was actually stripped.
function clearError(el) {
    // Nothing to strip: leave the DOM alone so the caret keeps its position.
    if (el.getElementsByClassName('error').length == 0) {
        return false
    }
    for (var msg of Array.from(el.getElementsByClassName('error-msg'))) {
        msg.remove()
    }
    // Assigned as text, so the expression comes back exactly as it was typed.
    el.textContent = inputText(el)
    moveCursorToEnd(el)
    return true
}

// `apply` receives the match tree, or null for an empty box. A bad expression
// is reported in the box and apply is not called, so whatever the last good
// expression produced stays on screen.
function commitExpression(el, apply) {
    var exp = inputText(el).trim()
    if (exp == "") {
        // Drops any error markup along with the text.
        el.innerHTML = ""
        apply(null)
        return
    }

    var validateResult = expressionValidate(exp)
    if (validateResult != "OK") {
        showError(el, validateResult)
        return
    }

    apply(expressionParse(exp))
}

// Enter and blur commit; focus strips the error marker so the caret can move
// through the expression again. Returns the two ways a caller drives the box
// outside of those events: commit reads it, refresh does not.
function bindExpressionInput(el, apply) {
    // The expression as of the last commit, or null once focus wiped an error
    // marker. A commit replaces apply's output, cancelling a click landing on
    // it, so an unchanged expression commits nothing on blur.
    var lastText = null

    // The last applied tree, so it can be applied again without reading the
    // box. A rejected expression leaves it alone, hence the wrapper.
    var lastTree = null
    const record = tree => {
        lastTree = tree
        apply(tree)
    }

    const commit = () => {
        lastText = inputText(el).trim()
        commitExpression(el, record)
    }

    el.addEventListener('keypress', e => {
        if (e.key != 'Enter') {
            return
        }
        // Commit first, so the blur below finds nothing left to do.
        e.preventDefault()
        commit()
        moveCursorToEnd(el)
        el.blur()
    })

    el.addEventListener('input', () => {
        // A pasted newline would turn the single line into a block.
        if (el.innerText.includes('\n')) {
            el.innerText = el.innerText.replace(/\n/g, '')
            moveCursorToEnd(el)
        }

        // Deleting the last character leaves a <br>, and only a box with no
        // children at all counts as :empty -- which the placeholder needs.
        if (el.textContent == "" && el.childNodes.length > 0) {
            el.innerHTML = ""
            moveCursorToEnd(el)
        }
    })

    el.addEventListener('focus', () => {
        if (clearError(el)) {
            // The marker is gone, so blurring has to report the error again.
            lastText = null
        }
    })

    el.addEventListener('focusout', () => {
        if (inputText(el).trim() != lastText) {
            commit()
        }
    })

    return {
        // Read the box and apply what it says.
        commit,
        // Apply the last committed expression again, box untouched: for when
        // what apply draws on changed rather than the expression. Reading the
        // box would report an error over a half-typed one.
        refresh: () => apply(lastTree),
    }
}

export {bindExpressionInput}
