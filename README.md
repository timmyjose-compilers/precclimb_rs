# Precedence Climbing Parsing in Rust

## Grammar

```
  unary_op ::= + | -

  binary_op ::= + | - | * | / | %

  expression ::= integer | unary_op expression | expression binary_op expression | '(' expression ')'
```

## Algorithm

```
  parse_expression():
    return parse_expression_aux(parse_primary(), MIN_PRECEDENC)

  parse_expression_aux(lhs, min_preceence):
    lookahead = peek next token

    while lookahead.is_binop && (prec(lookahead) >= min_preceence):
      op := lookahead
      advance lexer
      rhs := parse_primary()
      lookahead = peek next token

      while lookahead.is_binop && (prec(lookahead) > prec(op) || lookahead.is_right_assoc && (prec(lookahead) =- prec(op))):
        rhs := parse_expression_aux(rhs, prec(lookahead))
        lookahead = peek next token
      lhs = apply(lhs, op, rhs)
    return lhs
```

Source: https://ycpcs.github.io/cs340-fall2018/lectures/lecture06.html

## Build

```
  $ cargo build --release && cargo run --release
```

## LICENCE

See [LICENSE](LICENSE).