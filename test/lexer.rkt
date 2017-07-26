#lang racket

(require dssl2/private/lexer
         parser-tools/lex)

; : String -> [List-of Token]
(define (scan-string str)
  (define lexer (new-dssl2-lexer "test" (open-input-string str) #f))
  (let loop ([tokens-accumulator '()])
    (define token (position-token-token (lexer)))
    (if (or (eof-object? token) (eq? 'EOF (token-name token)))
      (reverse tokens-accumulator)
      (loop (cons (show-token token) tokens-accumulator)))))

; : Token -> Any
; Turns a token into a single value or testing. If the token has a value
; we use that, otherwise we use the name.
(define (show-token token)
  (or (token-value token) (token-name token)))

(module+ test
  (require rackunit)

  (define (test-scan str result)
    (check-equal? (scan-string str) result))

  (test-scan "hello"
             '(hello NEWLINE))
  (test-scan "let z = 9"
             '(LET z EQUALS 9 NEWLINE))
  (test-scan "let z = 9\n"
             '(LET z EQUALS 9 NEWLINE NEWLINE))
  (test-scan "def f(x):\n    x + 1"
             '(DEF f LPAREN x RPAREN COLON INDENT x PLUS 1
                   DEDENT NEWLINE))
  (test-scan "test:\n  test:\n    a\n  b"
             '(TEST COLON INDENT TEST COLON INDENT a DEDENT NEWLINE b
                    DEDENT NEWLINE))
  (test-scan "test:\n  a\n  b\nc"
             '(TEST COLON INDENT a NEWLINE b DEDENT NEWLINE c NEWLINE))
  (test-scan "if a:\n  e"
             '(IF a COLON INDENT e DEDENT NEWLINE))
  (test-scan "if a:\n  e[0]"
             '(IF a COLON INDENT e LBRACK 0 RBRACK DEDENT NEWLINE))
  (test-scan "lambda x: y == 1"
             '(LAMBDA x COLON INDENT y == 1 DEDENT NEWLINE))
  (test-scan "[ ]"
             '(LBRACK RBRACK NEWLINE))
  (test-scan "let x = lambda y: z\nw"
             '(LET x EQUALS LAMBDA y COLON INDENT z
                   DEDENT NEWLINE w NEWLINE))
  (test-scan "if a: c\nelse: e"
             '(IF a COLON INDENT c DEDENT
                  ELSE COLON INDENT e DEDENT NEWLINE))
  (test-scan "else: [ ]"
             '(ELSE COLON INDENT LBRACK RBRACK DEDENT NEWLINE)))
