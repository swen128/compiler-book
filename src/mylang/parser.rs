use chumsky::{prelude::*, Stream};

use super::{
    ast::*,
    document::{Span, Spanned},
    lexer::Token,
};

pub type ParseError<'src> = Simple<Token<'src>, Span>;

pub fn parse<'src>(tokens: Vec<Spanned<Token<'src>>>) -> Result<Program, Vec<ParseError<'src>>> {
    let len = tokens.len();
    let end_of_input = Span::new(len, len + 1);
    let stream = Stream::from_iter(
        end_of_input,
        tokens
            .into_iter()
            .map(|Spanned { span, value }| (value, span)),
    );

    let parser = program_parser();

    parser.parse(stream)
}

fn program_parser<'src>() -> impl Parser<Token<'src>, Program, Error = ParseError<'src>> {
    expr_parser()
        .map(|expr| Program(expr.value))
        .then_ignore(end())
        .labelled("program")
}

fn expr_parser<'src>() -> impl Parser<Token<'src>, Spanned<Expr>, Error = ParseError<'src>> + Clone
{
    recursive(|expr| {
        let literal = select! {
            Token::Num(n) => Expr::Num(n),
            Token::String(s) => Expr::String(s.to_string()),
        }
        .labelled("literal");

        let id = select! {
            Token::Id(id) => Id(id.to_string()),
        }
        .map_with_span(Spanned::new)
        .labelled("identifier");

        let lvalue = lvalue_parser(expr.clone());

        let no_value = just([Token::LParen, Token::RParen]).to(Expr::NoValue);

        let grouped = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .labelled("grouped expression");

        let decs = declarations_parser(expr.clone());
        let expseq = expr
            .clone()
            .separated_by(just(Token::Semicolon))
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(Expr::Seq)
            .labelled("sequence of expressions");

        // type [ size ] of init
        let array_size = expr
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket));
        let array = id
            .then(array_size)
            .then_ignore(just(Token::Of))
            .then(expr.clone())
            .map(|((ty, size), init)| Expr::array(ty, size, init));

        // type { key: value, ... }
        let record_field = id
            .then_ignore(just(Token::Colon))
            .then(expr.clone())
            .map(|(key, value)| RecordField { key, value });
        let record = id
            .then(
                record_field
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map(|(ty, fields)| Expr::record(ty, fields));

        let let_ = just(Token::Let)
            .ignore_then(decs)
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .then_ignore(just(Token::End))
            .map(|(decs, exps)| Expr::let_(decs, exps))
            .labelled("let expression");

        let else_clause = just(Token::Else).ignore_then(expr.clone());
        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then(else_clause.or_not())
            .map(|((cond, then), else_)| Expr::if_(cond, then, else_))
            .labelled("if expression");

        let while_ = just(Token::While)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Do))
            .then(expr.clone())
            .then_ignore(just(Token::End))
            .map(|(cond, body)| Expr::while_(cond, body))
            .labelled("while expression");

        let break_ = just(Token::Break)
            .to(Expr::Break)
            .labelled("break expression");

        let range = expr
            .clone()
            .then_ignore(just(Token::To))
            .then(expr.clone())
            .map(|(start, end)| Range { start, end });
        let for_ = just(Token::For)
            .ignore_then(id)
            .then_ignore(just(Token::ColonEq))
            .then(range)
            .then_ignore(just(Token::Do))
            .then(expr.clone())
            .map(|((id, iter), body)| Expr::for_(id, iter, body))
            .labelled("for expression");

        let assignment = lvalue
            .clone()
            .then_ignore(just(Token::ColonEq))
            .then(expr.clone())
            .map(|(lhs, rhs)| Expr::assign(lhs, rhs))
            .labelled("assignment");

        let func_args = expr
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .labelled("function arguments");
        let func_call = id
            .then(func_args)
            .map(|(name, args)| Expr::FuncCall(name, args))
            .labelled("function call");

        let atom = choice((
            no_value,
            expseq,
            literal,
            func_call,
            assignment,
            array,
            record,
            lvalue.map(|x| Expr::lvalue(x.value)),
            grouped.map(|x| x.value),
            let_,
            if_,
            while_,
            for_,
            break_,
        ))
        .map_with_span(Spanned::new)
        .labelled("atomic expression")
        .boxed();

        let product_operator = select! {
            Token::Times => BiOp::Mul,
            Token::Div => BiOp::Div,
        }
        .map_with_span(Spanned::new);

        let sum_operator = select! {
            Token::Plus => BiOp::Plus,
            Token::Minus => BiOp::Minus,
        }
        .map_with_span(Spanned::new);

        let comparison_operator = select! {
            Token::Eq => BiOp::Eq,
            Token::Neq => BiOp::Neq,
            Token::Lt => BiOp::Lt,
            Token::Le => BiOp::Le,
        }
        .map_with_span(Spanned::new);

        let negation = just(Token::Minus)
            .ignore_then(atom.clone())
            .map(Expr::neg)
            .map_with_span(Spanned::new)
            .labelled("negation");

        let scalar = atom.or(negation);

        // Product operators are all left-associative, i.e. `a * b / c` is parsed as `(a * b) / c`.
        let product = scalar
            .clone()
            .then(product_operator.then(scalar).repeated())
            .foldl(|left, (op, right)| {
                let span = left.span.merge(&right.span);
                let value = Expr::biop(op, left, right);
                Spanned::new(value, span)
            })
            .labelled("product expression")
            .boxed();

        // Sum operators are all left-associative.
        let arithmetic = product
            .clone()
            .then(sum_operator.then(product).repeated())
            .foldl(|left, (op, right)| {
                let span = left.span.merge(&right.span);
                let value = Expr::biop(op, left, right);
                Spanned::new(value, span)
            })
            .labelled("arithmetic expression");

        // Comparison operators do NOT associate, i.e. `a < b < c` is invalid.
        let comparison = arithmetic
            .clone()
            .then(comparison_operator.then(arithmetic).or_not())
            .map(|(left, right)| match right {
                None => left,
                Some((op, right)) => {
                    let span = left.span.merge(&right.span);
                    let value = Expr::biop(op, left, right);
                    Spanned::new(value, span)
                }
            })
            .labelled("comparison expression")
            .boxed();

        let and_operator = just(Token::And).to(BiOp::And).map_with_span(Spanned::new);
        let or_operator = just(Token::Or).to(BiOp::Or).map_with_span(Spanned::new);

        let and = comparison
            .clone()
            .then(and_operator.then(comparison).or_not())
            .map(|(left, right)| match right {
                None => left,
                Some((op, right)) => {
                    let span = left.span.merge(&right.span);
                    let value = Expr::biop(op, left, right);
                    Spanned::new(value, span)
                }
            })
            .labelled("and");

        let or = and
            .clone()
            .then(or_operator.then(and).or_not())
            .map(|(left, right)| match right {
                None => left,
                Some((op, right)) => {
                    let span = left.span.merge(&right.span);
                    let value = Expr::biop(op, left, right);
                    Spanned::new(value, span)
                }
            })
            .labelled("or");

        or
    })
}

fn declarations_parser<'src>(
    expr: impl Parser<Token<'src>, Spanned<Expr>, Error = ParseError<'src>> + 'src + Clone,
) -> impl Parser<Token<'src>, Vec<Spanned<Dec>>, Error = ParseError<'src>> + Clone {
    let id = select! {
        Token::Id(id) => Id(id.to_string()),
    }
    .map_with_span(Spanned::new)
    .labelled("identifier");

    let type_id = select! {
        Token::Id(id) => TypeId(id.to_string()),
    }
    .map_with_span(Spanned::new)
    .labelled("type identifier");

    let type_hint = just(Token::Colon)
        .ignore_then(type_id)
        .or_not()
        .labelled("type hint");

    let vardec = just(Token::Var)
        .ignore_then(id)
        .then(type_hint.clone())
        .then_ignore(just(Token::ColonEq))
        .then(expr.clone())
        .map(|((id, ty), expr)| VarDec {
            id,
            ty,
            expr: Box::new(expr),
        })
        .labelled("variable declaration");

    let tyfield = id
        .then_ignore(just(Token::Colon))
        .then(type_id)
        .map(|(key, ty)| TyField { key, ty })
        .map_with_span(Spanned::new)
        .labelled("type field");
    let tyfields = tyfield.separated_by(just(Token::Comma));
    let function_params = tyfields
        .clone()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .labelled("function parameters");

    let record_type = tyfields
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map(Ty::Record)
        .labelled("record type");

    let array_type = just(Token::Array)
        .ignore_then(just(Token::Of))
        .ignore_then(type_id)
        .map(Ty::Array)
        .labelled("array type");

    let ty = choice((
        type_id.map(|name| Ty::Name(name.value)),
        record_type,
        array_type,
    ))
    .map_with_span(Spanned::new);

    let tydec = just(Token::Type)
        .ignore_then(type_id)
        .then_ignore(just(Token::Eq))
        .then(ty)
        .map(|(id, ty)| TyDec { id, ty })
        .labelled("type declaration");

    let fundec = just(Token::Fn)
        .ignore_then(id.labelled("function name"))
        .then(function_params)
        .then(type_hint.labelled("return type"))
        .then_ignore(just(Token::Eq))
        .then(expr)
        .map(|(((name, params), return_type), body)| FnDec {
            name,
            params,
            return_type,
            body: Box::new(body),
        });

    let dec = choice((
        tydec.map(Dec::TypeDec),
        vardec.map(Dec::VarDec),
        fundec.map(Dec::FnDec),
    ))
    .map_with_span(Spanned::new);

    dec.repeated()
}

fn lvalue_parser<'src>(
    expr: impl Parser<Token<'src>, Spanned<Expr>, Error = ParseError<'src>> + 'src + Clone,
) -> impl Parser<Token<'src>, Spanned<LValue>, Error = ParseError<'src>> + Clone {
    let id = select! {
        Token::Id(id) => Id(id.to_string()),
    }
    .labelled("identifier");

    let key = just(Token::Dot)
        .ignore_then(id)
        .map(Accessor::Key)
        .map_with_span(Spanned::new)
        .labelled("record field");

    let index = expr
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map(|expr| Accessor::Index(expr.value))
        .map_with_span(Spanned::new)
        .labelled("array index");

    let accessor = key.or(index);
    let path = accessor.repeated();

    id.map(LValue::Variable)
        .map_with_span(Spanned::new)
        .then(path)
        .foldl(|lvalue, accessor| lvalue.concat(accessor))
        .labelled("left-side value")
}

enum Accessor {
    Key(Id),
    Index(Expr),
}

impl Spanned<LValue> {
    fn concat(self, accessor: Spanned<Accessor>) -> Self {
        match accessor.value {
            Accessor::Key(key) => {
                let whole_span = self.span.merge(&accessor.span);
                let key = Spanned::new(key, accessor.span);
                let new_lvalue = LValue::RecordField(Box::new(self), key);
                Spanned::new(new_lvalue, whole_span)
            }
            Accessor::Index(index) => {
                let whole_span = self.span.merge(&accessor.span);
                let index = Spanned::new(index, accessor.span);
                let new_lvalue = LValue::ArrayIndex(Box::new(self), index);
                Spanned::new(new_lvalue, whole_span)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use chumsky::Stream;

    use super::*;

    #[test]
    fn parser_accepts_simple_expression() {
        let tokens = vec![(Token::Num(10), Span::new(0, 2))];
        let len = 3;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = program_parser().parse(stream);
        let expected = Ok(Program(Expr::Num(10)));

        assert_eq!(result, expected);
    }

    #[test]
    fn parser_rejects_when_some_tokens_are_left_uncosumed() {
        let tokens = vec![
            (Token::Num(10), Span::new(0, 2)),
            (Token::Num(10), Span::new(0, 2)),
        ];
        let len = 3;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = program_parser().parse(stream);
        let expected = Err(vec![Simple::expected_input_found(
            Span::new(0, 2),
            [None],
            Some(Token::Num(10)),
        )
        .with_label("product expression")]);

        assert_eq!(result, expected);
    }

    #[test]
    fn lvalue_parser_accepts_variable() {
        let tokens = vec![(Token::Id("foo"), Span::new(0, 3))];
        let len = 4;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = lvalue_parser(expr_parser()).parse(stream);

        let expected = Ok(Spanned::new(LValue::var("foo"), Span::new(0, 3)));

        assert_eq!(result, expected);
    }

    #[test]
    fn lvalue_parser_accepts_record_field() {
        let tokens = vec![
            (Token::Id("foo"), Span::new(0, 3)),
            (Token::Dot, Span::new(4, 5)),
            (Token::Id("bar"), Span::new(6, 9)),
        ];
        let len = 10;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = lvalue_parser(expr_parser()).parse(stream);

        let expected = Ok(Spanned::new(
            LValue::RecordField(
                Box::new(Spanned::new(
                    LValue::Variable(Id("foo".to_string())),
                    Span::new(0, 3),
                )),
                Spanned::new(Id("bar".to_string()), Span::new(4, 9)),
            ),
            Span::new(0, 9),
        ));

        assert_eq!(result, expected);
    }

    #[test]
    fn lvalue_parser_accepts_array_index() {
        let tokens = vec![
            (Token::Id("foo"), Span::new(0, 3)),
            (Token::LBracket, Span::new(4, 5)),
            (Token::Num(10), Span::new(6, 8)),
            (Token::RBracket, Span::new(8, 9)),
        ];
        let len = 10;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = lvalue_parser(expr_parser()).parse(stream);

        let expected = Ok(Spanned::new(
            LValue::ArrayIndex(
                Box::new(Spanned::new(
                    LValue::Variable(Id("foo".to_string())),
                    Span::new(0, 3),
                )),
                Spanned::new(Expr::Num(10), Span::new(4, 9)),
            ),
            Span::new(0, 9),
        ));

        assert_eq!(result, expected);
    }

    #[test]
    fn lvalue_parser_rejects_invalid_syntax() {
        let tokens = vec![
            (Token::Num(10), Span::new(0, 3)),
            (Token::Dot, Span::new(4, 5)),
            (Token::Id("foo"), Span::new(6, 8)),
        ];
        let len = 9;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = lvalue_parser(expr_parser()).parse(stream);
        let expected = Err(vec![Simple::expected_input_found(
            Span::new(0, 3),
            Vec::new(),
            Some(Token::Num(10)),
        )
        .with_label("identifier")]);

        assert_eq!(result, expected);
    }

    #[test]
    fn expr_parser_accepts_no_value() {
        // ()
        let tokens = vec![
            (Token::LParen, Span::new(0, 1)),
            (Token::RParen, Span::new(1, 2)),
        ];
        let len = 3;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = expr_parser().parse(stream);
        let expected = Ok(Spanned::new(Expr::NoValue, Span::new(0, 2)));

        assert_eq!(result, expected);
    }

    #[test]
    fn expr_parser_accepts_if_expression_without_else_clause() {
        // if 1 then 2
        let tokens = vec![
            (Token::If, Span::new(0, 2)),
            (Token::Num(1), Span::new(3, 4)),
            (Token::Then, Span::new(5, 9)),
            (Token::Num(2), Span::new(10, 11)),
        ];
        let len = 12;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = expr_parser().parse(stream);
        let expected = Ok(Spanned::new(
            Expr::If(Box::new(If {
                cond: Spanned::new(Expr::Num(1), Span::new(3, 4)),
                then: Spanned::new(Expr::Num(2), Span::new(10, 11)),
                else_: None,
            })),
            Span::new(0, 11),
        ));

        assert_eq!(result, expected);
    }

    #[test]
    fn expr_parser_accepts_if_expression_with_else_clause() {
        // if 1 then 2 else 3
        let tokens = vec![
            (Token::If, Span::new(0, 2)),
            (Token::Num(1), Span::new(3, 4)),
            (Token::Then, Span::new(5, 9)),
            (Token::Num(2), Span::new(10, 11)),
            (Token::Else, Span::new(12, 16)),
            (Token::Num(3), Span::new(17, 18)),
        ];
        let len = 19;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = expr_parser().parse(stream);
        let expected = Ok(Spanned::new(
            Expr::If(Box::new(If {
                cond: Spanned::new(Expr::Num(1), Span::new(3, 4)),
                then: Spanned::new(Expr::Num(2), Span::new(10, 11)),
                else_: Some(Spanned::new(Expr::Num(3), Span::new(17, 18))),
            })),
            Span::new(0, 18),
        ));

        assert_eq!(result, expected);
    }

    #[test]
    fn declarations_parser_accepts_function_declaration() {
        // function foo(x: int, y: string): int = 42
        let tokens = vec![
            (Token::Fn, Span::new(0, 2)),
            (Token::Id("foo"), Span::new(3, 6)),
            (Token::LParen, Span::new(7, 8)),
            (Token::Id("x"), Span::new(8, 9)),
            (Token::Colon, Span::new(10, 11)),
            (Token::Id("int"), Span::new(12, 15)),
            (Token::Comma, Span::new(15, 16)),
            (Token::Id("y"), Span::new(17, 18)),
            (Token::Colon, Span::new(19, 20)),
            (Token::Id("string"), Span::new(21, 27)),
            (Token::RParen, Span::new(27, 28)),
            (Token::Colon, Span::new(29, 30)),
            (Token::Id("int"), Span::new(31, 34)),
            (Token::Eq, Span::new(35, 36)),
            (Token::Num(42), Span::new(37, 39)),
        ];
        let len = 40;
        let end_of_input = Span::new(len, len + 1);
        let stream = Stream::from_iter(end_of_input, tokens.into_iter());

        let result = declarations_parser(expr_parser()).parse(stream);
        let expected = Ok(vec![Spanned::new(
            Dec::FnDec(FnDec {
                name: Spanned::new(Id("foo".to_string()), Span::new(3, 6)),
                params: vec![
                    Spanned::new(
                        TyField {
                            key: Spanned::new(Id("x".to_string()), Span::new(8, 9)),
                            ty: Spanned::new(TypeId("int".to_string()), Span::new(12, 15)),
                        },
                        Span::new(8, 15),
                    ),
                    Spanned::new(
                        TyField {
                            key: Spanned::new(Id("y".to_string()), Span::new(17, 18)),
                            ty: Spanned::new(TypeId("string".to_string()), Span::new(21, 27)),
                        },
                        Span::new(17, 27),
                    ),
                ],
                return_type: Some(Spanned::new(TypeId("int".to_string()), Span::new(31, 34))),
                body: Box::new(Spanned::new(Expr::Num(42), Span::new(37, 39))),
            }),
            Span::new(0, 39),
        )]);

        assert_eq!(result, expected);
    }
}
