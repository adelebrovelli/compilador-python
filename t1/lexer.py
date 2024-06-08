import re

# Tokens
tokens = [
    ('NUMBER',    r'\d+'),
    ('ID',        r'[a-zA-Z_]\w*'),
    ('ASSIGN',    r'='),
    ('PLUS',      r'\+'),
    ('MINUS',     r'-'),
    ('MULT',      r'\*'),
    ('DIV',       r'/'),
    ('LPAREN',    r'\('),
    ('RPAREN',    r'\)'),
    ('LBRACE',    r'\{'),
    ('RBRACE',    r'\}'),
    ('LBRACKET',  r'\['),
    ('RBRACKET',  r'\]'),
    ('SEMICOLON', r';'),
    ('WHITESPACE', r'\s+'),
]

def lexer(code):
    pos = 0
    tokens_list = []

    while pos < len(code):
        match = None
        for token_expr in tokens:
            pattern, tag = token_expr
            regex = re.compile(tag)
            match = regex.match(code, pos)
            if match:
                text = match.group(0)
                if pattern != 'WHITESPACE':
                    token = (pattern, text)
                    tokens_list.append(token)
                break
        if not match:
            raise SyntaxError(f"Illegal character: {code[pos]}")
        else:
            pos = match.end(0)

    return tokens_list
