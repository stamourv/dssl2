#lang racket

(provide dssl2-empty-tokens dssl2-tokens new-dssl2-lexer
         natural float hexadecimal octal binary
         comment sq-str-char dq-str-char identifier)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

(define-empty-tokens dssl2-empty-tokens
  (EOF
   INDENT
   DEDENT
   NEWLINE
   LPAREN
   RPAREN
   LBRACK
   RBRACK
   LBRACE
   RBRACE
   COMMA
   PERIOD
   COLON
   SEMICOLON
   EQUALS
   PLUS         ; two different precedences
   MINUS        ; two different precedences
   LAMBDA
   ASSERT
   ASSERT-EQ
   LET
   IF
   ELIF
   ELSE
   WHILE
   FOR
   IN
   BREAK
   CONTINUE
   OBJECT
   REQUIRE
   RETURN
   PASS
   TEST
   TIME
   DEF
   DEFSTRUCT))

(define-tokens dssl2-tokens
  (IDENT
   OP0  ; or
   OP1  ; and
   OP2  ; == <= >= != < > === !==
   OP3  ; (|)
   OP4  ; ^
   OP5  ; &
   OP6  ; << >>
   OP7  ; (+ -)
   OP8  ; * / %
   OP9  ; (unary) ~ ! (+ -)
   OP10 ; **
   LITERAL))

(define-lex-abbrevs
  [natural     (:+ numeric)]
  [exponent    (:: (:or #\e #\E) (:? #\-) natural)]
  [pointfloat  (:or (:: natural #\. (:* numeric))
                    (:: (:* numeric) #\. natural))]
  [float       (:or (:: pointfloat (:? exponent))
                    (:: natural exponent))]
  [hexdigit    (:or numeric (char-range #\a #\f) (char-range #\A #\F))]
  [hexadecimal (:: (:? #\-) (:or "0x" "0X") (:+ hexdigit))]
  [octdigit    (char-range #\0 #\7)]
  [octal       (:: (:? #\-) (:or "0o" "0O") (:+ octdigit))]
  [binary      (:: (:? #\-) (:or "0b" "0B") (:+ (:or #\0 #\1)))]
  [comment     (:: #\# (:* (:- any-char #\newline)))]
  [sq-str-char (:or (:- any-char (:or #\\ #\' #\newline))
                    (:: #\\ any-char))]
  [dq-str-char (:or (:- any-char (:or #\\ #\" #\newline))
                    (:: #\\ any-char))]
  [identifier  (:: alphabetic
                   (:* (:or alphabetic numeric #\_))
                   (:? (:or #\! #\?)))])

; new-dssl2-lexer : string? input-port? boolean? -> [ -> Token]
(define (new-dssl2-lexer src port interactive?)
  (define (lexical-error pos msg . args)
    (define offset (position-offset pos))
    (raise-read-error (apply format msg args)
                      src
                      (position-line pos)
                      (position-col pos)
                      offset
                      (and offset (max 1 (- (file-position port) offset)))))
  (define the-lexer
    (lexer-src-pos
      [(eof)                    (token-EOF)]
      [#\(                      (token-LPAREN)]
      [#\[                      (token-LBRACK)]
      [#\{                      (token-LBRACE)]
      [#\)                      (token-RPAREN)]
      [#\]                      (token-RBRACK)]
      [#\}                      (token-RBRACE)]
      [#\,                      (token-COMMA)]
      [#\.                      (token-PERIOD)]
      [#\:                      (token-COLON)]
      [#\;                      (token-SEMICOLON)]
      [#\=                      (token-EQUALS)]
      [#\+                      (token-PLUS)]
      [#\-                      (token-MINUS)]
      ["let"                    (token-LET)]
      ["assert"                 (token-ASSERT)]
      ["assert_eq"              (token-ASSERT-EQ)]
      ["if"                     (token-IF)]
      ["elif"                   (token-ELIF)]
      ["else"                   (token-ELSE)]
      ["while"                  (token-WHILE)]
      ["for"                    (token-FOR)]
      ["in"                     (token-IN)]
      ["break"                  (token-BREAK)]
      ["continue"               (token-CONTINUE)]
      ["object"                 (token-OBJECT)]
      ["require"                (token-REQUIRE)]
      ["return"                 (token-RETURN)]
      ["pass"                   (token-PASS)]
      ["lambda"                 (token-LAMBDA)]
      [#\λ                      (token-LAMBDA)]
      ["True"                   (token-LITERAL #t)]
      ["False"                  (token-LITERAL #f)]
      ["def"                    (token-DEF)]
      ["defstruct"              (token-DEFSTRUCT)]
      ["test"                   (token-TEST)]
      ["time"                   (token-TIME)]
      ["or"                     (token-OP0 (string->symbol lexeme))]
      ["and"                    (token-OP1 (string->symbol lexeme))]
      [(:or "==" #\< #\> "<=" ">=" "!=" "===" "!==")
                                (token-OP2 (string->symbol lexeme))]
      [#\|                      (token-OP3 (string->symbol lexeme))]
      [#\^                      (token-OP4 (string->symbol lexeme))]
      [#\&                      (token-OP5 (string->symbol lexeme))]
      [(:or "<<" ">>")          (token-OP6 (string->symbol lexeme))]
      [(:or #\* #\/ #\%)        (token-OP8 (string->symbol lexeme))]
      [(:or #\! #\~)            (token-OP9 (string->symbol lexeme))]
      ["**"                     (token-OP10 (string->symbol lexeme))]
      [(:: #\" (:* dq-str-char) #\")
       (token-LITERAL
         (interpret-string (remove-first-and-last lexeme)))]
      [(:: #\' (:* sq-str-char) #\')
       (token-LITERAL
         (interpret-string (remove-first-and-last lexeme)))]
      [(:: #\" (:* dq-str-char))
       (lexical-error start-pos "Unterminated string")]
      [(:: #\' (:* sq-str-char))
       (lexical-error start-pos "Unterminated string")]
      [natural                  (token-LITERAL (read-string lexeme))]
      [float                    (token-LITERAL (read-string lexeme))]
      [hexadecimal              (token-LITERAL (interpret-non-dec lexeme))]
      [octal                    (token-LITERAL (interpret-non-dec lexeme))]
      [binary                   (token-LITERAL (interpret-non-dec lexeme))]
      ["inf"                    (token-LITERAL +inf.0)]
      ["nan"                    (token-LITERAL +nan.0)]
      [identifier               (token-IDENT (string->symbol lexeme))]
      [comment
       (return-without-pos (the-lexer port))]
      [#\space
       (return-without-pos (the-lexer port))]
      [#\newline
       (return-without-pos (the-lexer port))]
      [#\tab
       (lexical-error start-pos "Tabs are not allowed in DSSL2")]
      [any-char
       (lexical-error start-pos "Unexpected character ‘~a’" lexeme)]
      [(special)
       (lexical-error start-pos "Unexpected special" lexeme)]
      [(special-comment)
       (lexical-error start-pos "Unexpected special comment" lexeme)]))

  (port-count-lines! port)

  ;; Above is the base lexer, which ignores whichspace. Below this is
  ;; the state machine that restores newlines and indentation.

  (define stack '(0))
  (define queue '())

  (define (push value)
    (set! stack (cons value stack)))
  (define (pop)
    (set! stack (rest stack)))

  (define previous-line 1)

  (define (yield token)
    (set! previous-line (position-line (position-token-end-pos token)))
    (set! queue (append queue (cons token '()))))

  (define (pop-indents-to n pos)
    (when (and (number? (first stack)) (< n (first stack)))
      (yield (position-token (token-DEDENT) pos pos))
      (pop)
      (pop-indents-to n pos)))

  (define (balance-delimiters token)
    (define name (token-name (position-token-token token)))
    (define start-pos (position-token-start-pos token))
    (case name
      [(LPAREN)
       (push 'RPAREN)
       (yield token)]
      [(LBRACK)
       (push 'RBRACK)
       (yield token)]
      [(LBRACE)
       (push 'RBRACE)
       (yield token)]
      [(RPAREN RBRACK RBRACE)
       (pop-indents-to 0 start-pos)
       (unless (eq? name (first stack))
         (lexical-error start-pos "‘~a’ matched by ‘~a”"
                        (first stack) name))
       (pop)
       (yield token)]
      [else (yield token)]))

  (define (fill-queue)
    (define token (the-lexer port))
    (define start-pos (position-token-start-pos token))
    (define end-pos (position-token-end-pos token))
    (define old-previous-line previous-line)

    (pop-indents-to (position-col start-pos) start-pos)

    (when (and (> (position-line start-pos) old-previous-line)
               (number? (first stack))
               (<= (position-col start-pos) (first stack))
               (not (memq (token-name (position-token-token token))
                          '(ELSE ELIF))))
      (yield (position-token (token-NEWLINE) start-pos start-pos)))

    (define name (token-name (position-token-token token)))

    (case name
      [(EOF)
       (pop-indents-to 0 start-pos)
       (yield (position-token (token-NEWLINE) start-pos start-pos))
       (yield token)]
      [(COLON)
       (yield token)
       (define next-token (the-lexer port))
       (define next-start (position-token-start-pos next-token))
       (cond
         [(and (number? (first stack))
               (< (position-col next-start) (first stack)))
          (lexical-error next-start "Got dedent where indent expected")]
         [else
           (set! stack (cons (position-col next-start) stack))
           (yield (position-token (token-INDENT) next-start next-start))
           (balance-delimiters next-token)])]
      [else
        (balance-delimiters token)]))

  (λ ()
     (when (empty? queue) (fill-queue))
     (define result (first queue))
     (set! queue (rest queue))
     result))

; string? -> string?
; Removes the first and last characters of a string.
(define (remove-first-and-last str)
  (substring str 1 (sub1 (string-length str))))

; string? -> string?
; Interprets the escapes in a string literal.
(define (interpret-string lit)
  (define (loop chars)
    (cond
      [(empty? chars)    '()]
      [(eq? #\\ (first chars))
       (define (the-rest) (loop (rest (rest chars))))
       (case (second chars)
         [(#\a)         (cons #\007 (the-rest))]
         [(#\b)         (cons #\backspace (the-rest))]
         [(#\f)         (cons #\page (the-rest))]
         [(#\n)         (cons #\newline (the-rest))]
         [(#\r)         (cons #\return (the-rest))]
         [(#\t)         (cons #\tab (the-rest))]
         [(#\v)         (cons #\vtab (the-rest))]
         [(#\newline)   (the-rest)]
         [(#\x)         (cons (hex->char (third chars) (fourth chars))
                              (loop (list-tail chars 4)))]
         [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                        (cons
                          (oct->char (second chars)
                                     (third chars)
                                     (fourth chars))
                          (loop (list-tail chars 4)))]
         [else          (cons (second chars) (the-rest))])]
      [else
        (cons (first chars)
              (loop (rest chars)))]))
  (list->string (loop (string->list lit))))

; char? char? -> char?
; Converts two hex digits to the represented character.
(define (hex->char digit1 digit2)
  (integer->char
    (read-string (list->string (list #\# #\x digit1 digit2)))))

; char? char? char? -> char?
; Converts three octal digits to the represented character.
(define (oct->char digit1 digit2 digit3)
  (integer->char
    (read-string (list->string (list #\# #\o digit1 digit2 digit3)))))

; string? -> number?
; Interprets a Python non-decimal number.
(define (interpret-non-dec str)
  (read-string (string-append "#" (substring str 1))))

; string? -> number?
; Counts the spaces on the last line of the string.
(define (last-spaces str)
  (string-length (regexp-replace #rx".*\n" str "")))

; string? -> any?
; Reads from a string
(define (read-string str)
  (read (open-input-string str)))

(module+ test
  (define a-lexer (new-dssl2-lexer (current-input-port)))

  (let loop ()
    (define token (a-lexer))
    (unless (eq? (position-token-token token) 'EOF)
      (displayln token)
      (loop))))
