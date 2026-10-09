---
title: Code Highlighting with Prism
author: Kiln Example
date: 2026-01-05
---

Kiln rewrites every fenced code block's opening line before handing the page to Prism, so a plain triple-backtick fence with a language name ends up with line numbers and brace matching enabled for free.

## Haskell

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

## Python

```python
def factorial(n: int) -> int:
    return 1 if n == 0 else n * factorial(n - 1)
```

## JavaScript

```javascript
const factorial = (n) => (n === 0 ? 1 : n * factorial(n - 1));
```

## Unlabeled Fence

A fence without a language tag is left untouched and still renders as plain preformatted text:

```
no highlighting here, just monospace text
```
