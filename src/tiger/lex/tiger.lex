%filenames = "scanner"

 /*
 * Please don't modify the lines above.
 */

%x COMMENT STR STR_ESCAPE_CHAR STR_ESCAPE_CHAR_WHITESPACE

LETTER   [a-zA-Z]
DIGIT    [0-9]

%%

 /*
  * Below is examples, which you can wipe out
  * and write regular expressions and actions of your own.
  *
  * All the tokens:
  *   Parser::ID
  *   Parser::STRING
  *   Parser::INT
  *   Parser::COMMA
  *   Parser::COLON
  *   Parser::SEMICOLON
  *   Parser::LPAREN
  *   Parser::RPAREN
  *   Parser::LBRACK
  *   Parser::RBRACK
  *   Parser::LBRACE
  *   Parser::RBRACE
  *   Parser::DOT
  *   Parser::PLUS
  *   Parser::MINUS
  *   Parser::TIMES
  *   Parser::DIVIDE
  *   Parser::EQ
  *   Parser::NEQ
  *   Parser::LT
  *   Parser::LE
  *   Parser::GT
  *   Parser::GE
  *   Parser::AND
  *   Parser::OR
  *   Parser::ASSIGN
  *   Parser::ARRAY
  *   Parser::IF
  *   Parser::THEN
  *   Parser::ELSE
  *   Parser::WHILE
  *   Parser::FOR
  *   Parser::TO
  *   Parser::DO
  *   Parser::LET
  *   Parser::IN
  *   Parser::END
  *   Parser::OF
  *   Parser::BREAK
  *   Parser::NIL
  *   Parser::FUNCTION
  *   Parser::VAR
  *   Parser::TYPE
  */

 /* reserved words */

"array" {adjust(); return Parser::ARRAY;}
"if" {adjust(); return Parser::IF;}
"then" {adjust(); return Parser::THEN;}
"else" {adjust(); return Parser::ELSE;}
"while" {adjust(); return Parser::WHILE;}
"for" {adjust(); return Parser::FOR;}
"to" {adjust(); return Parser::TO;}
"do" {adjust(); return Parser::DO;}
"let" {adjust(); return Parser::LET;}
"in" {adjust(); return Parser::IN;}
"end" {adjust(); return Parser::END;}
"of" {adjust(); return Parser::OF;}
"break" {adjust(); return Parser::BREAK;}
"nil" {adjust(); return Parser::NIL;}
"function" {adjust(); return Parser::FUNCTION;}
"var" {adjust(); return Parser::VAR;}
"type" {adjust(); return Parser::TYPE;}


{LETTER}({DIGIT}|{LETTER}|_)* {adjust(); return Parser::ID;}


"/*" {
  more();
  ++comment_level_;
  begin(StartCondition__::COMMENT);
}

<COMMENT> "*/" {
  if(--comment_level_ == 1){
    adjust();
    begin(StartCondition__::INITIAL);
  }else{
    more();
  }
}

<COMMENT> "/*" {
  more();
  ++comment_level_;
}

<COMMENT> \n  {more();}

<COMMENT> .  {more();}

<COMMENT> <<EOF>> {
  adjust();
  errormsg_->Error(errormsg_->tok_pos_, "illegal comment: unexpected EOF");
  comment_level_ = 1;
  begin(StartCondition__::INITIAL);
}


\" {
  more();
  begin(StartCondition__::STR);
}

<STR> \\ {
  more();
  begin(StartCondition__::STR_ESCAPE_CHAR);
}

<STR> \" {
  adjust();
  setMatched(string_buf_);
  string_buf_.clear();
  begin(StartCondition__::INITIAL);
  return Parser::STRING;
}

<STR> . {
  more();
  string_buf_.push_back(matched().back());
}

<STR> <<EOF>> {
  adjust();
  errormsg_->Error(errormsg_->tok_pos_, "illegal string: unexpected EOF");
  begin(StartCondition__::INITIAL); 
}


<STR_ESCAPE_CHAR> n {
  more();
  string_buf_.push_back('\n');
  begin(StartCondition__::STR);
}

<STR_ESCAPE_CHAR> t {
  more();
  string_buf_.push_back('\t');
  begin(StartCondition__::STR);
}

<STR_ESCAPE_CHAR> \" {
  more();
  string_buf_.push_back('\"');
  begin(StartCondition__::STR);
}

<STR_ESCAPE_CHAR> \\ {
  more();
  string_buf_.push_back('\\');
  begin(StartCondition__::STR);
}

<STR_ESCAPE_CHAR> \^(@|[A-Z]) {
  more();
  string_buf_.push_back(matched().back() - '@');
  begin(StartCondition__::STR);
}

<STR_ESCAPE_CHAR> {DIGIT}{3} {
  more();
  string_buf_.push_back(std::stoi(std::string(matched().end() - 3, matched().end())));
  begin(StartCondition__::STR);
}

<STR_ESCAPE_CHAR> [ \t\n] {
  more();
  begin(StartCondition__::STR_ESCAPE_CHAR_WHITESPACE);
}

<STR_ESCAPE_CHAR> \n {
  more();
  begin(StartCondition__::STR_ESCAPE_CHAR_WHITESPACE);
}

<STR_ESCAPE_CHAR> . {
  adjust();
  errormsg_->Error(errormsg_->tok_pos_, "illegal string: escape character: unknown");
  begin(StartCondition__::INITIAL);
}

<STR_ESCAPE_CHAR> <<EOF>> {
  adjust();
  errormsg_->Error(errormsg_->tok_pos_, "illegal string: escape character: unexpected EOF");
  begin(StartCondition__::INITIAL);
}

<STR_ESCAPE_CHAR_WHITESPACE> [ \t] {
  more();
  }

<STR_ESCAPE_CHAR_WHITESPACE> \n {
  more();
  errormsg_->Newline();
}

<STR_ESCAPE_CHAR_WHITESPACE> \\ {
  more();
  begin(StartCondition__::STR);
}

<STR_ESCAPE_CHAR_WHITESPACE> . {
  adjust();
  errormsg_->Error(errormsg_->tok_pos_, "illegal string: escape character: whitespace: unexpected character");
  begin(StartCondition__::INITIAL);
}

<STR_ESCAPE_CHAR_WHITESPACE> <<EOF>> {
  adjust();
  errormsg_->Error(errormsg_->tok_pos_, "illegal string: escape character: whitespace: unexpected EOF");
  begin(StartCondition__::INITIAL);
}


{DIGIT}+ {adjust(); return Parser::INT;}


":=" {adjust(); return Parser::ASSIGN;}
">=" {adjust(); return Parser::GE;}
"<>" {adjust(); return Parser::NEQ;}
"<=" {adjust(); return Parser::LE;}

\, {adjust(); return Parser::COMMA;}
\: {adjust(); return Parser::COLON;}
\; {adjust(); return Parser::SEMICOLON;}
\( {adjust(); return Parser::LPAREN;}
\) {adjust(); return Parser::RPAREN;}
\[ {adjust(); return Parser::LBRACK;}
\] {adjust(); return Parser::RBRACK;}
\{ {adjust(); return Parser::LBRACE;}
\} {adjust(); return Parser::RBRACE;}
\. {adjust(); return Parser::DOT;}
\+ {adjust(); return Parser::PLUS;}
\- {adjust(); return Parser::MINUS;}
\* {adjust(); return Parser::TIMES;}
\/ {adjust(); return Parser::DIVIDE;}
\= {adjust(); return Parser::EQ;}
\< {adjust(); return Parser::LT;}
\> {adjust(); return Parser::GT;}
\& {adjust(); return Parser::AND;}
\| {adjust(); return Parser::OR;}

 /*
  * skip white space chars.
  * space, tabs and LF
  */

[ \t]+ {adjust();}

\n {adjust(); errormsg_->Newline();}

 /* illegal input */
. {adjust(); errormsg_->Error(errormsg_->tok_pos_, "illegal token: unknown");}
