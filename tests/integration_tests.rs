use compiler_book::{ast::*, parse, Span, Spanned};
use pretty_assertions::assert_eq;

#[test]
fn parse_accepts_assignment() {
    let input = "a := 1";
    let output = parse(input);
    let expected = Ok(Program(spanned(
        0,
        6,
        Expr::assign(spanned(0, 1, LValue::var("a")), spanned(5, 6, Expr::num(1))),
    )));
    assert_eq!(output, expected);
}

#[test]
fn parse_handles_arithmetic_operator_precedence() {
    let input = "1 + 2 * 3 - 4 / 5";
    let output = parse(input);
    let expected = Ok(Program(spanned(
        0,
        17,
        Expr::biop(
            spanned(10, 11, BiOp::Minus),
            spanned(
                0,
                9,
                Expr::biop(
                    spanned(2, 3, BiOp::Plus),
                    spanned(0, 1, Expr::num(1)),
                    spanned(
                        4,
                        9,
                        Expr::biop(
                            spanned(6, 7, BiOp::Mul),
                            spanned(4, 5, Expr::num(2)),
                            spanned(8, 9, Expr::num(3)),
                        ),
                    ),
                ),
            ),
            spanned(
                12,
                17,
                Expr::biop(
                    spanned(14, 15, BiOp::Div),
                    spanned(12, 13, Expr::num(4)),
                    spanned(16, 17, Expr::num(5)),
                ),
            ),
        ),
    )));
    assert_eq!(output, expected);
}

#[test]
fn parse_handles_all_operator_precedence() {
    // Equivalent to `((-1) <> 0) | (((1 + (2 * 3)) - (4 / 5)) = 6) & (7 <= 8))`
    let input = "-1 <> 0 | 1 + 2 * 3 - 4 / 5 = 6 & 7 <= 8";
    let output = parse(input);
    let expected = Ok(Program(spanned(
        0,
        40,
        Expr::biop(
            spanned(8, 9, BiOp::Or),
            spanned(
                0,
                7,
                Expr::biop(
                    spanned(3, 5, BiOp::Neq),
                    spanned(0, 2, Expr::neg(spanned(1, 2, Expr::num(1)))),
                    spanned(6, 7, Expr::num(0)),
                ),
            ),
            spanned(
                10,
                40,
                Expr::biop(
                    spanned(32, 33, BiOp::And),
                    spanned(
                        10,
                        31,
                        Expr::biop(
                            spanned(28, 29, BiOp::Eq),
                            spanned(
                                10,
                                27,
                                Expr::biop(
                                    spanned(20, 21, BiOp::Minus),
                                    spanned(
                                        10,
                                        19,
                                        Expr::biop(
                                            spanned(12, 13, BiOp::Plus),
                                            spanned(10, 11, Expr::num(1)),
                                            spanned(
                                                14,
                                                19,
                                                Expr::biop(
                                                    spanned(16, 17, BiOp::Mul),
                                                    spanned(14, 15, Expr::num(2)),
                                                    spanned(18, 19, Expr::num(3)),
                                                ),
                                            ),
                                        ),
                                    ),
                                    spanned(
                                        22,
                                        27,
                                        Expr::biop(
                                            spanned(24, 25, BiOp::Div),
                                            spanned(22, 23, Expr::num(4)),
                                            spanned(26, 27, Expr::num(5)),
                                        ),
                                    ),
                                ),
                            ),
                            spanned(30, 31, Expr::num(6)),
                        ),
                    ),
                    spanned(
                        34,
                        40,
                        Expr::biop(
                            spanned(36, 38, BiOp::Le),
                            spanned(34, 35, Expr::num(7)),
                            spanned(39, 40, Expr::num(8)),
                        ),
                    ),
                ),
            ),
        ),
    )));
    assert_eq!(output, expected);
}

#[test]
fn parse_accepts_for_expression() {
    let input = "for i := 0 to 10 do i";
    let output = parse(input);
    let expected = Ok(Program(spanned(
        0,
        21,
        Expr::for_(
            spanned(4, 5, Id("i".to_string())),
            Range {
                start: spanned(9, 10, Expr::num(0)),
                end: spanned(14, 16, Expr::num(10)),
            },
            spanned(20, 21, Expr::lvalue(LValue::var("i"))),
        ),
    )));
    assert_eq!(output, expected);
}

#[test]
fn parse_accepts_comparison_expression() {
    let input = "1 = 1";
    let output = parse(input);
    let expected = Ok(Program(spanned(
        0,
        5,
        Expr::biop(
            spanned(2, 3, BiOp::Eq),
            spanned(0, 1, Expr::num(1)),
            spanned(4, 5, Expr::num(1)),
        ),
    )));
    assert_eq!(output, expected);
}

#[test]
fn parse_accepts_string_literal() {
    let input = r#""hello""#;
    let output = parse(input);
    let expected = Ok(Program(spanned(0, 7, Expr::string("hello".to_string()))));
    assert_eq!(output, expected);
}

#[test]
fn complex() {
    let input = r#"for j := 0 to N-1 do print(if col[i]=j then " O" else " .")"#;
    let output = parse(input);
    let expected = Ok(Program(spanned(
        0,
        59,
        Expr::for_(
            spanned(4, 5, Id("j".to_string())),
            Range {
                start: spanned(9, 10, Expr::num(0)),
                end: spanned(
                    14,
                    17,
                    Expr::biop(
                        spanned(15, 16, BiOp::Minus),
                        spanned(14, 15, Expr::lvalue(LValue::var("N"))),
                        spanned(16, 17, Expr::num(1)),
                    ),
                ),
            },
            spanned(
                21,
                59,
                Expr::FuncCall(
                    spanned(21, 26, Id("print".to_string())),
                    vec![spanned(
                        27,
                        58,
                        Expr::if_(
                            spanned(
                                30,
                                38,
                                Expr::biop(
                                    spanned(36, 37, BiOp::Eq),
                                    spanned(
                                        30,
                                        36,
                                        Expr::lvalue(LValue::array_index(
                                            spanned(30, 33, LValue::var("col")),
                                            spanned(33, 36, Expr::lvalue(LValue::var("i"))),
                                        )),
                                    ),
                                    spanned(37, 38, Expr::lvalue(LValue::var("j"))),
                                ),
                            ),
                            spanned(44, 48, Expr::string(" O".to_string())),
                            Some(spanned(54, 58, Expr::string(" .".to_string()))),
                        ),
                    )],
                ),
            ),
        ),
    )));
    assert_eq!(output, expected);
}

#[test]
fn parse_accepts_complex_program() {
    let input = r###"
        let
            var N := 8

            type intArray = array of int

            var row := intArray [N] of 0
            var col := intArray [N] of 0
            var diag1 := intArray [N+N-1] of 0
            var diag2 := intArray [N+N-1] of 0

            function printboard() = (
                for i := 0 to N-1 do (
                    for j := 0 to N-1 do print(if col[i]=j then " O" else " .");
                    print("\n")
                );
                print("\n")
            )
            
            function try(c:int) =
                if c=N
                    then printboard()
                    else for r := 0 to N-1 do
                        if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0
                            then (
                                row[r]:=1;
                                diag1[r+c]:=1;
                                diag2[r+7-c]:=1;
                                col[c]:=r;
                                try(c+1);
                                row[r]:=0;
                                diag1[r+c]:=0;
                                diag2[r+7-c]:=0
                            )
        in
            try(0)
        end
    "###;

    let output = parse(input);
    output.unwrap();
}

fn spanned<T>(start: usize, end: usize, value: T) -> Spanned<T> {
    Spanned::new(value, Span::new(start, end))
}
