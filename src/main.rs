use std::str::Chars;
use std::usize;

use druid::widget::Label;
use druid::{AppLauncher, Widget, WindowDesc};

#[derive(Copy, Clone, Default, Debug, Eq, PartialEq)]
pub struct Localization {
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
}

impl Localization {
    fn advance_by(&self, lines: usize, cols: usize) -> Self {
        Self {
            start_line: self.end_line,
            start_col: self.end_col,
            end_line: self.end_line + lines,
            end_col: self.end_col + cols,
        }
    }

    fn combine_first_last(&self, other: &Self) -> Self {
        Self {
            start_line: self.start_line,
            start_col: self.start_col,
            end_line: other.end_line,
            end_col: other.end_col,
        }
    }

    fn combine_last(&self, other: &Self) -> Self {
        Self {
            start_line: self.end_line,
            start_col: self.end_col,
            end_line: other.end_line,
            end_col: other.end_col,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Number {
        value: usize,
        local: Localization,
    },
    Call {
        args: Vec<Expression<'a>>, // first element is Function name,rest are arguments
        local: Localization,
    },
    Tuple {
        args: Vec<Expression<'a>>,
        local: Localization,
    },
    Variable {
        name: &'a str,
        local: Localization,
    },
}

impl<'a> Expression<'a> {
    fn number(value: usize, local: Localization) -> Self {
        Self::Number { value, local }
    }
    fn call(args: Vec<Expression<'a>>, local: Localization) -> Self {
        Self::Call { args, local }
    }

    fn tuple(args: Vec<Expression<'a>>, local: Localization) -> Self {
        Self::Tuple { args, local }
    }

    fn variable(name: &'a str, local: Localization) -> Self {
        Self::Variable { name, local }
    }

    fn get_location(&self) -> Localization {
        match self {
            Expression::Number { local, .. }
            | Expression::Call { local, .. }
            | Expression::Tuple { local, .. }
            | Expression::Variable { local, .. } => *local,
        }
    }


    fn display_as_tree(&self,n:usize){
        (0..n).for_each(|_| print!("--"));
        match self {
            Expression::Number { value, local } => {
                println!(" {value} : Number @ {}:{} -- {}:{}",local.start_line,local.start_col,local.end_line,local.end_col)
            },
            Expression::Call { args, local } => {
                println!("Call @ {}:{} -- {}:{}",local.start_line,local.start_col,local.end_line,local.end_col);
                args[0].display_as_tree(n+1);
                args[1..].iter().for_each(|e| e.display_as_tree(n+2))
            },
            Expression::Tuple { args, local } => {
                println!("Tuple @ {}:{} -- {}:{}",local.start_line,local.start_col,local.end_line,local.end_col);
                args.iter().for_each(|e| e.display_as_tree(n+2))
            },
            Expression::Variable { name, local } => {
                println!(" {name} : Variable @ {}:{} -- {}:{}",local.start_line,local.start_col,local.end_line,local.end_col)
            },
        }
    }
}

use parser_combinator::either::Either;
//Parsers
use parser_combinator::parser::{match_anything, match_literal};
use parser_combinator::*;

#[derive(Clone, Debug)]
pub enum ParseErrors<'a> {
    GenericErr(Localization, &'static str, Chars<'a>),
    Empty,
}

//Tokens
pub fn whitespace<'a>(
    input: Chars<'a>,
    state: Localization,
) -> parser_combinator::ParseResult<Chars<'a>, Localization, char, ParseErrors> {
    let space = match_anything(|local: Localization| local.advance_by(0, 1)).validate(
        |character: &char| character == &' ',
        "alphabetic character".to_string(),
    );
    let newline = match_anything(|local: Localization| local.advance_by(1, 0)).validate(
        |character: &char| character == &'\n',
        "alphabetic character".to_string(),
    );

    space
        .or_else(newline)
        .with_error_using_state(|(_a, _b), state, rest| {
            ParseErrors::GenericErr(state, "whitespace failed", rest)
        })
        .parse(input, state)
}

macro_rules! token_implementer {
    ($type:ident,$repr:literal) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, (&'a str, Localization), ParseErrors<'a>>
            for $type
        {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, (&'a str, Localization), ParseErrors<'a>>
            {
                match_literal($repr.chars(), |local: Localization| {
                    local.advance_by(0, $repr.len())
                })
                .transform_with_state(|id, state| (id.as_str(), state))
                .with_error_using_state(|_, state, input| {
                    ParseErrors::GenericErr(state, $repr, input)
                })
                .skip(whitespace)
                .parse(input, state)
            }
        }
    };

    ($type:ident,$repr:expr) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, (&'a str, Localization), ParseErrors<'a>>
            for $type
        {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, (&'a str, Localization), ParseErrors<'a>>
            {
                let predicate = $repr;
                (token_with_predicate(predicate).skip(whitespace)).parse(input, state)
            }
        }
    };
}

fn token_with_predicate(
    predicate: impl Fn(&char) -> bool,
) -> impl for<'a> Fn(
    Chars<'a>,
    Localization,
) -> ParseResult<
    'a,
    Chars<'a>,
    Localization,
    (&'a str, Localization),
    ParseErrors<'a>,
> {
    move |mut input: Chars<'_>, mut state: Localization| {
        let old_state = state;
        let state_transformer = |_: &char, local: Localization| local.advance_by(0, 1);
        let mut orig = input.clone();
        let mut c = 0;
        match input.next() {
            Some(x) => {
                if !predicate(&x) {
                    return Err(ParseErrors::GenericErr(state, "lowercase variable", orig));
                }
            }
            None => return Err(ParseErrors::GenericErr(state, "lowercase variable", orig)),
        }
        let orig2 = orig.clone();
        let mut orig3 = orig.clone();
        loop {
            match orig.next() {
                Some(x) => {
                    if !predicate(&x) {
                        break;
                    } else {
                        c += 1;
                        state = state_transformer(&x, state);
                        orig3.next();
                        
                    }
                }
                None => break,
            }
        }
        let s = orig2.as_str();
        Ok(((&s[..c], old_state), state, orig3))
    }
}

token_implementer!(PlusToken, "+");
token_implementer!(MinusToken, "-");
token_implementer!(StarToken, "*");
token_implementer!(SlashToken, "/");
token_implementer!(CarrotToken, "^");
token_implementer!(LParenToken, "(");
token_implementer!(RParenToken, ")");
token_implementer!(DotToken, ".");
token_implementer!(CommaToken, ",");
token_implementer!(EqualToken, "=");
token_implementer!(SemiColonToken, ";");
token_implementer!(LowerCaseToken, |character: &char| character.is_alphabetic()
    && character.is_ascii_lowercase());
token_implementer!(IntegerToken, |character: &char| character.is_numeric());

//Grammar

macro_rules! grammar_snippet {
    //TODO s
    // well just think about this more it could be represented more elegantly.
    // It would be cool if we could merge snippets into one big grammar macro with all kinds of special syntax
    //  But thats for future
    //  An easy quality of life improvement would using closures fix variable names in the macro itself for production and error
    //  And therefore allievieating need to supply a closure
    ($type:ident :=  $rule:expr ; production := $production:expr ) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, Expression<'a>, ParseErrors<'a>> for $type {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, Expression<'a>, ParseErrors<'a>> {
                $rule.transform_with_state($production).skip(whitespace).parse(input, state)
            }
        }
    };

    ($type:ident :=  $rule:expr  ; precondition :=$cond:expr ;error :=   $err:expr ) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, Expression<'a>, ParseErrors<'a>> for $type {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, Expression<'a>, ParseErrors<'a>> {
                if !$cond(input.clone(), state.clone()) {
                    return $err;
                }
                $rule.skip(whitespace)
                    /*.transform(|x| {
                        println!("{} :    {:?}", stringify!($type), x);
                        x
                    })*/
                    .parse(input, state)
            }
        }
    };

    ($type:ident :=  $rule:expr  ; error :=   $err:expr) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, Expression<'a>, ParseErrors<'a>> for $type {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, Expression<'a>, ParseErrors<'a>> {
                $rule
                    
                    .with_error_using_state($err)
                    /*.transform(|x| {
                        println!("{} :    {:?}", stringify!($type), x);
                        x
                    })*/
                    .skip(whitespace)
                    .parse(input, state)
            }
        }
    };

    ($type:ident :=  $rule:expr  ; production := $production:expr ; error :=   $err:expr     ) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, Expression<'a>, ParseErrors<'a>> for $type {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, Expression<'a>, ParseErrors<'a>> {
                $rule
                    .transform_with_state($production)
                    .with_error_using_state($err)
                    /*.transform(|x| {
                        println!("{} :    {:?}", stringify!($type), x);
                        x
                    })*/
                    .skip(whitespace)
                    .parse(input, state)
            }
        }
    };
}

pub fn build_right_assoc<'a>(
    list: (Expression<'a>, Vec<(Expression<'a>, Expression<'a>)>),
    state: Localization,
) -> Expression<'a> {
    let (first, rest) = list;
    if rest.is_empty() {
        first
    } else {
        rest.into_iter().fold(first, |left, (op, right)| {
            let loc = left.get_location().combine_last(&right.get_location());
            Expression::call(vec![op, left, right], loc)
        })
    }
}

pub fn build_left_assoc<'a>(
    tree: Either<(Expression<'a>, Expression<'a>, Expression<'a>), Expression<'a>>,
    _state: Localization,
) -> Expression<'a> {
    match tree {
        Either::Left((x, name, y)) => {
            let loc = name.get_location();
            let args = vec![name, x, y];
            Expression::call(args, loc)
        }
        Either::Right(x) => x,
    }
}

grammar_snippet! {
    Toplevel         := Expr.pair(SemiColonToken).first();
    production := |x,_| x;
    error      := |err,st,rest| err.fold(|x| x, |_| ParseErrors::Empty)
}


grammar_snippet! {
    Expr         := SumExpr;
    precondition := |mut input : Chars,_| input.any(|char| !char.is_whitespace());
    error        :=  Err(ParseErrors::Empty)
}

grammar_snippet! {
    SumExpr    := Self.right_assoc(SubExpr, Plus);
    production := build_right_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "sum", rest)
}

grammar_snippet! {
    SubExpr    := Self.right_assoc(MulExpr, Minus);
    production := build_right_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "subtraction", rest)
}

grammar_snippet! {
    MulExpr    := Self.right_assoc(DivExpr, Star);
    production := build_right_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "multiplication", rest)
}

grammar_snippet! {
    DivExpr    := Self.right_assoc(ExpExpr, Slash);
    production := build_right_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "division", rest)
}

grammar_snippet! {
    ExpExpr    := Self.left_assoc(Carrot, AtomicExpr.or_else(NegExpr));
    production := build_left_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "exponentiation", rest)
}

grammar_snippet! {
    NegExpr    := Minus.pair(Expr);
    production := |(name,x),_state| { let local = name.get_location();let args = vec![name,x];  Expression::call (  args, local )};
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "negation", rest)
}

grammar_snippet! {
    AtomicExpr    := CallExpr.or_else(NumExpr).or_else(VarExpr).or_else(BracketedExpr);
    error         := |_err,st,rest| ParseErrors::GenericErr(st, "atom", rest)
}

grammar_snippet! {
    BracketedExpr    := LParenToken.triple(Expr, RParenToken).second();
    error            := |_err,st,rest| ParseErrors::GenericErr(st, "bracketed", rest)
}

grammar_snippet! {
    CallExpr    := VarExpr.pair(LParenToken.triple(Expr.separated_by(CommaToken), RParenToken));
    production := | (name,(_, (expr, vec), (_, local2))),_state|
    {
        let mut args = vec![name,expr];
        args.extend(vec.into_iter().map(|(_,x)| x));
        Expression::call ( args, local2 )
    };
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "Call", rest)
}

grammar_snippet! {
    NumExpr := IntegerToken;
    production := |(val, local), state|

    {
        let value = str::parse::<usize>(val).unwrap();
                                       Expression::number(value, local.combine_last(&state))
    };
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "Number", rest)
}

grammar_snippet! {
    VarExpr := LowerCaseToken;
    production := |(name, local), state| Expression::variable(name, local.combine_last(&state));
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "Variable", rest)
}

grammar_snippet! {
    Plus := PlusToken;
    production := |(name,local),state| Expression::variable(name,local.combine_last(&state))
}
grammar_snippet! {
    Minus := MinusToken;
    production := |(name,local),state| Expression::variable(name,local.combine_last(&state))
}
grammar_snippet! {
    Star := StarToken;
    production := |(name,local),state| Expression::variable(name,local.combine_last(&state))
}
grammar_snippet! {
    Slash := SlashToken;
    production := |(name,local),state| Expression::variable(name,local.combine_last(&state))
}
grammar_snippet! {
    Carrot := CarrotToken;
    production := |(name,local),state| Expression::variable(name,local.combine_last(&state))
}


#[test]
fn number_expr_should_suceed() {
    let a = "123    ".chars();
    let res = SumExpr.parse(a, Localization::default()).unwrap();
    println!("{:?}", res);
    assert!(false)
}

#[test]
fn var_expr_should_suceed() {
    let a = "xey    ".chars();
    let res = SumExpr.parse(a, Localization::default()).unwrap();
    println!("{:?}", res);
    assert!(false)
}

#[test]
fn sum_expr_should_suceed() {
    let a = " 1 * 2 + 3 * 4 + 5 +  6 + 7 + 8".chars();
    let res = SumExpr.parse(a, Localization::default()).unwrap();
    println!("{:?}", res);
    assert!(false)
}

use std::io::{self, BufRead};

fn main()  {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        print!(">> ");
        let lines = line.unwrap();
        println!("Result :");
        println!("       :{lines}");
        let a = Expr.parse(lines.chars(), Localization::default()).unwrap();
        println!("       :{}",a.2.as_str());
        a.0.display_as_tree(0)
    }
}

fn build_ui() -> impl Widget<()> {
    Label::new("Hello world")
}

fn main2() {
    let main_window = WindowDesc::new(|| build_ui())
        .window_size((600.0, 400.0))
        .title("My first Druid App");
    let initial_data = ();

    AppLauncher::with_window(main_window)
        .launch(initial_data)
        .expect("Failed to launch application");
}
