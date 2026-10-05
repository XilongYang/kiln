---
title: Math Typesetting with MathJax
author: Kiln Example
date: 2025-11-20
---

Kiln passes `--mathjax` to Pandoc and loads MathJax in the page head, so both inline and display math render without any extra markup in the post itself.

## Inline Math

The roots of $ax^2 + bx + c = 0$ are given by $x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$.

## Display Math

Euler's identity:

$$e^{i\pi} + 1 = 0$$

A system of equations, using an aligned block:

$$
\begin{aligned}
x + y &= 5 \\
2x - y &= 1
\end{aligned}
$$

## Matrices

$$
\begin{pmatrix}
1 & 0 \\
0 & 1
\end{pmatrix}
$$
