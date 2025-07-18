# Expressions

<!--
Part of the Carbon Language project, under the Apache License v2.0 with LLVM
Exceptions. See /LICENSE for license information.
SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
-->

<!-- toc -->

## Table of contents

-   [Overview](#overview)
-   [Precedence](#precedence)
-   [Names](#names)
    -   [Unqualified names](#unqualified-names)
    -   [Qualified names and member access](#qualified-names-and-member-access)
-   [Operators](#operators)
-   [Suffix operators](#suffix-operators)
-   [Conversions and casts](#conversions-and-casts)
-   [`if` expressions](#if-expressions)
-   [Numeric type literal expressions](#numeric-type-literal-expressions)
-   [Alternatives considered](#alternatives-considered)
-   [References](#references)

<!-- tocstop -->

## Overview

Expressions are the portions of Carbon syntax that produce values. Because types
in Carbon are values, this includes anywhere that a type is specified.

```
fn Foo(a: i32*) -> i32 {
  return *a;
}
```

Here, the parameter type `i32*`, the return type `i32`, and the operand `*a` of
the `return` statement are all expressions.

## Precedence

Expressions are interpreted based on a partial
[precedence ordering](https://en.wikipedia.org/wiki/Order_of_operations).
Expression components which lack a relative ordering must be disambiguated by
the developer, for example by adding parentheses; otherwise, the expression will
be invalid due to ambiguity. Precedence orderings will only be added when it's
reasonable to expect most developers to understand the precedence without
parentheses.

The precedence diagram is defined thusly:

```mermaid
%%{init: {'themeVariables': {'fontFamily': 'monospace'}}}%%
graph BT
    parens["(...)"]

    braces["{...}"]
    click braces "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/classes.md#literals"

    unqualifiedName["x"]
    click unqualifiedName "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/README.md#unqualified-names"

    top((" "))

    suffixOps{"x.y
               x.(...)
               x->y
               x->(...)
               x(...)
               x[y]"}
    click suffixOps "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/README.md#suffix-operators"

    qualifiedType["const T
                   partial T"]
    click pointer-type "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/type_operators.md"

    pointerType{"T*"}
    click pointer-type "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/type_operators.md"

    pointer{"*x
             &x"}
    click pointer "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/pointer.md"

    negation["-x"]
    click negation "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/arithmetic.md"

    complement["^x"]
    click complement "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/bitwise.md"

    incDec["++x;
            --x;"]
    click incDec "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/assignment.md"

    unary((" "))

    as["x as T"]
    click as "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/implicit_conversions.md"

    multiplication>"x * y
                    x / y"]
    click multiplication "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/arithmetic.md"

    addition>"x + y
              x - y"]
    click addition "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/arithmetic.md"

    modulo["x % y"]
    click modulo "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/arithmetic.md"

    bitwise_and>"x & y"]
    bitwise_or>"x | y"]
    bitwise_xor>"x ^ y"]
    click bitwise_and "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/bitwise.md"
    click bitwise_or "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/bitwise.md"
    click bitwise_xor "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/bitwise.md"

    shift["x << y
           x >> y"]
    click shift "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/bitwise.md"

    binaryOps((" "))

    where["T where R"]

    comparison["x == y
                x != y
                x < y
                x <= y
                x > y
                x >= y"]
    click comparison "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/comparison_operators.md"

    not["not x"]
    click not "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/logical_operators.md"

    logicalOperand((" "))

    and>"x and y"]
    click and "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/logical_operators.md"

    or>"x or y"]
    click or "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/logical_operators.md"

    logicalExpression((" "))

    if>"if x then y else z"]
    click if "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/expressions/if.md"

    insideParens["(...)"]

    assignment["x = y;
                x $= y;"]
    click assignment "https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/assignment.md"

    expressionStatement["x;"]

    top --> parens & braces & unqualifiedName

    suffixOps --> top

    qualifiedType --> suffixOps
    pointerType --> qualifiedType

    pointer --> suffixOps
    negation & complement & incDec --> pointer
    unary --> pointerType & negation & complement

    %% Use a longer arrow here to put `not` next to other unary operators
    not ---> suffixOps

    %% `as` at the same level as `where` and comparisons
    as -----> unary

    multiplication & modulo & bitwise_and & bitwise_or & bitwise_xor & shift --> unary
    addition --> multiplication
    binaryOps --> addition & modulo & bitwise_and & bitwise_or & bitwise_xor & shift

    where --> binaryOps
    comparison --> binaryOps
    logicalOperand --> comparison & not

    %% This helps group `and` and `or` together
    classDef hidden display: none;
    HIDDEN:::hidden ~~~ logicalOperand

    and & or --> logicalOperand
    logicalExpression --> as & where & and & or
    if & expressionStatement --> logicalExpression
    insideParens & assignment --> if
```

The diagram's attributes are:

-   Each non-empty node represents a precedence group. Empty circles are used to
    simplify the graph, and do not represent a precedence group.

-   When an expression is composed from different precedence groups, the
    interpretation is determined by the precedence edges:

    -   A precedence edge A --> B means that A is lower precedence than B, so A
        can contain B without parentheses. For example, `or --> not` means that
        `not x or y` is treated as `(not x) or y`.

    -   Precedence edges are transitive. For example, `or --> == --> as` means
        that `or` is lower precedence than `as`.

-   When a binary operator expression is composed from a single precedence
    group, the interpretation is determined by the
    [associativity](https://en.wikipedia.org/wiki/Operator_associativity) of the
    precedence group:

    ```mermaid
    graph TD
        non["Non-associative"]
        left>"Left associative"]
    ```

    -   For example, `+` and `-` are left-associative and in the same precedence
        group, so `a + b + c - d` is treated as `((a + b) + c) - d`.

    -   Note that in Carbon, we currently only have left-associative operators.
        Unlike C++ and other languages, [assignment](/docs/design/assignment.md)
        isn't a right-associative operator, it uses its own statement.

-   When a unary operator expression is composed from a single precedence group,
    it can allow unparenthesized repetition or not:

    ```mermaid
    graph TD
        non["Non-repeating"]
        repeating{"Repeating"}
    ```

    This is analogous to associativity for binary operators.

## Names

### Unqualified names

An _unqualified name_ is a [word](../lexical_conventions/words.md) that is not a
keyword and is not preceded by a period (`.`).

**TODO:** Name lookup rules for unqualified names.

### Qualified names and member access

A _qualified name_ is a word that appears immediately after a period or
rightward arrow. Qualified names appear in the following contexts:

-   [Designators](/docs/design/classes.md#literals): `.` _word_
-   [Simple member access expressions](member_access.md): _expression_ `.`
    _word_
-   [Simple pointer member access expressions](member_access.md): _expression_
    `->` _word_

```
var x: auto = {.hello = 1, .world = 2};
                ^^^^^       ^^^^^ qualified name
               ^^^^^^      ^^^^^^ designator

x.hello = x.world;
  ^^^^^     ^^^^^ qualified name
^^^^^^^   ^^^^^^^ member access expression

x.hello = (&x)->world;
                ^^^^^ qualified name
          ^^^^^^^^^^^ pointer member access expression
```

Qualified names refer to members of an entity determined by the context in which
the expression appears. For a member access, the entity is named by the
expression preceding the period. In a struct literal, the entity is the struct
type. For example:

```
package Foo;
namespace N;
fn N.F() {}

fn G() {
  // Same as `(Foo.N).F()`.
  // `Foo.N` names namespace `N` in package `Foo`.
  // `(Foo.N).F` names function `F` in namespace `N`.
  Foo.N.F();
}

// `.n` refers to the member `n` of `{.n: i32}`.
fn H(a: {.n: i32}) -> i32 {
  // `a.n` is resolved to the member `{.n: i32}.n`,
  // and names the corresponding subobject of `a`.
  return a.n;
}

fn J() {
  // `.n` refers to the member `n of `{.n: i32}`.
  H({.n = 5 as i32});
}
```

Member access expressions associate left-to-right. If the member name is more
complex than a single _word_, a compound member access expression can be used,
with parentheses around the member name:

-   _expression_ `.` `(` _expression_ `)`
-   _expression_ `->` `(` _expression_ `)`

```
interface I { fn F[self: Self](); }
class X {}
impl X as I { fn F[self: Self]() {} }

// `x.I.F()` would mean `(x.I).F()`.
fn Q(x: X) { x.(I.F)(); }
```

Either simple or compound member access can be part of a _pointer_ member access
expression when an `->` is used instead of a `.`, where _expression_ `->` _..._
is syntactic sugar for `(` `*` _expression_ `)` `.` _..._.

## Operators

Most expressions are modeled as operators:

| Category   | Operator                            | Syntax    | Function                                                              |
| ---------- | ----------------------------------- | --------- | --------------------------------------------------------------------- |
| Call       | `()` (unary)                        | `x(...)`  | Function call: the value returned by calling the function `x`.        |
| Call       | [`[]`](indexing.md) (unary)         | `x[y]`    | Subscripting or indexing: returns the element `y` of `x`.             |
| Pointer    | [`*`](pointer_operators.md) (unary) | `*x`      | Pointer dereference: the object pointed to by `x`.                    |
| Pointer    | [`&`](pointer_operators.md) (unary) | `&x`      | Address-of: a pointer to the object `x`.                              |
| Arithmetic | [`-`](arithmetic.md) (unary)        | `-x`      | The negation of `x`.                                                  |
| Bitwise    | [`^`](bitwise.md) (unary)           | `^x`      | The bitwise complement of `x`.                                        |
| Arithmetic | [`+`](arithmetic.md)                | `x + y`   | The sum of `x` and `y`.                                               |
| Arithmetic | [`-`](arithmetic.md) (binary)       | `x - y`   | The difference of `x` and `y`.                                        |
| Arithmetic | [`*`](arithmetic.md)                | `x * y`   | The product of `x` and `y`.                                           |
| Arithmetic | [`/`](arithmetic.md)                | `x / y`   | `x` divided by `y`, or the quotient thereof.                          |
| Arithmetic | [`%`](arithmetic.md)                | `x % y`   | `x` modulo `y`.                                                       |
| Bitwise    | [`&`](bitwise.md)                   | `x & y`   | The bitwise AND of `x` and `y`.                                       |
| Bitwise    | [`\|`](bitwise.md)                  | `x \| y`  | The bitwise OR of `x` and `y`.                                        |
| Bitwise    | [`^`](bitwise.md) (binary)          | `x ^ y`   | The bitwise XOR of `x` and `y`.                                       |
| Bitwise    | [`<<`](bitwise.md)                  | `x << y`  | `x` bit-shifted left `y` places.                                      |
| Bitwise    | [`>>`](bitwise.md)                  | `x >> y`  | `x` bit-shifted right `y` places.                                     |
| Conversion | [`as`](as_expressions.md)           | `x as T`  | Converts the value `x` to the type `T`.                               |
| Comparison | [`==`](comparison_operators.md)     | `x == y`  | Equality: `true` if `x` is equal to `y`.                              |
| Comparison | [`!=`](comparison_operators.md)     | `x != y`  | Inequality: `true` if `x` is not equal to `y`.                        |
| Comparison | [`<`](comparison_operators.md)      | `x < y`   | Less than: `true` if `x` is less than `y`.                            |
| Comparison | [`<=`](comparison_operators.md)     | `x <= y`  | Less than or equal: `true` if `x` is less than or equal to `y`.       |
| Comparison | [`>`](comparison_operators.md)      | `x > y`   | Greater than: `true` if `x` is greater than to `y`.                   |
| Comparison | [`>=`](comparison_operators.md)     | `x >= y`  | Greater than or equal: `true` if `x` is greater than or equal to `y`. |
| Logical    | [`and`](logical_operators.md)       | `x and y` | A short-circuiting logical AND: `true` if both operands are `true`.   |
| Logical    | [`or`](logical_operators.md)        | `x or y`  | A short-circuiting logical OR: `true` if either operand is `true`.    |
| Logical    | [`not`](logical_operators.md)       | `not x`   | Logical NOT: `true` if the operand is `false`.                        |

The binary arithmetic and bitwise operators also have
[compound assignment](/docs/design/assignment.md) forms. These are statements
rather than expressions, and do not produce a value.

## Suffix operators

These operators act like unary postfix operators for purposes of precedence:

-   [Member access operators](member_access.md), like `x.y` and the
    dereferencing variant `x->y`, only have an expression on their left-hand
    side. The right-hand side is a name.
-   The [compound member access operators](member_access.md), `x.(...)` and
    `x->(...)`, have an expression as their second operand, but put that
    expression in parentheses and so it doesn't participate in the precedence
    considerations of its first operand.
-   The [indexing operator](indexing.md), `x[y]`, similarly puts its second
    operand in matching square brackets.
-   The call operator, `x(...)`, takes a comma-separated list of arguments, but
    again puts them in parentheses that clearly separate them for precedence
    purposes.

## Conversions and casts

When an expression appears in a context in which an expression of a specific
type is expected, [implicit conversions](implicit_conversions.md) are applied to
convert the expression to the target type.

Expressions can also be converted to a specific type using an
[`as` expression](as_expressions.md).

```
fn Bar(n: i32);
fn Baz(n: i64) {
  // OK, same as Bar(n as i32)
  Bar(n);
}
```

## `if` expressions

An [`if` expression](if.md) chooses between two expressions.

```
fn Run(args: Span(StringView)) {
  var file: StringView = if args.size() > 1 then args[1] else "/dev/stdin";
}
```

`if` expressions are analogous to `?:` ternary expressions in C and C++.

## Numeric type literal expressions

Carbon's syntax provides a simple way to represent different types of integers
and floating-point numbers. Each type is identified with a keyword-like syntax,
prefixed with either `i`, `u`, or `f` followed by a multiple of 8, representing
the size in bits of the data type.

These are referred to as
[numeric type literals](literals.md#numeric-type-literals).

## Alternatives considered

Other expression documents will list more alternatives; this lists alternatives
not noted elsewhere.

-   [Total order](/proposals/p0555.md#total-order)
-   [Different precedence for different operands](/proposals/p0555.md#different-precedence-for-different-operands)
-   [Require less than a partial order](/proposals/p0555.md#require-less-than-a-partial-order)

## References

Other expression documents will list more references; this lists references not
noted elsewhere.

-   Proposal
    [#555: Operator precedence](https://github.com/carbon-language/carbon-lang/pull/555).
