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
             '(hello))
  (test-scan "let z = 9"
             '(LET z EQUALS 9))
  (test-scan "let z = 9\n"
             '(LET z EQUALS 9 NEWLINE))
  (test-scan "def f(x):\n    x + 1"
             '(DEF f LPAREN x RPAREN COLON INDENT x PLUS 1))
  (test-scan "test:\n  test:\n    a\n  b"
             '(TEST COLON INDENT TEST COLON INDENT a DEDENT b))
  )
