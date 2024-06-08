import ply.lex as lex

# Definição dos tokens
tokens = [
    'ID', 'NUM_INT', 'NUM_DEC', 'TEXTO', 'EQUALS', 'PLUS_EQUALS', 'MINUS_EQUALS',
    'TIMES_EQUALS', 'DIV_EQUALS', 'MOD_EQUALS', 'AND_EQUALS', 'OR_EQUALS',
    'PLUS', 'MINUS', 'TIMES', 'DIV', 'MOD', 'AND', 'OR', 'NOT',
    'LT', 'GT', 'LE', 'GE', 'EQ', 'NE', 'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE',
    'SEMICOLON', 'COMMA', 'DOT', 'ARROW', 'INCREMENT', 'DECREMENT', 'COMMENT',
    'MULTI_COMMENT', 'LBRACKET', 'RBRACKET', 'COLON', 'DOTS'
]

reserved = {
    'int': 'INT',
    'float': 'FLOAT',
    'double': 'DOUBLE',
    'char': 'CHAR',
    'boolean': 'BOOLEAN',
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'switch': 'SWITCH',
    'case': 'CASE',
    'default': 'DEFAULT',
    'break': 'BREAK',
    'continue': 'CONTINUE',
    'return': 'RETURN',
    'struct': 'STRUCT',
    'void': 'VOID',
    'println': 'PRINTLN'
}

tokens = tokens + list(reserved.values())

t_EQUALS = r'='
t_PLUS_EQUALS = r'\+='
t_MINUS_EQUALS = r'-='
t_TIMES_EQUALS = r'\*='
t_DIV_EQUALS = r'/='
t_MOD_EQUALS = r'%='
t_AND_EQUALS = r'&&='
t_OR_EQUALS = r'\|\|='
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIV = r'/'
t_MOD = r'%'
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'
t_LT = r'<'
t_GT = r'>'
t_LE = r'<='
t_GE = r'>='
t_EQ = r'=='
t_NE = r'!='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_COMMA = r','
t_DOT = r'\.'
t_ARROW = r'->'
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COLON = r':'
t_DOTS = r'\.\.\.'

def t_COMMENT(t):
    r'//.*'
    pass  # Comentário de linha única, ignorar

def t_MULTI_COMMENT(t):
    r'/\*[^*]*\*+(?:[^/*][^*]*\*+)*/'
    pass  # Comentário de múltiplas linhas, ignorar

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_NUM_DEC(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_NUM_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_TEXTO(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = str(t.value)
    return t

t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

if __name__ == "__main__":
    import sys
    file_path = "C:\\Users\\santo\\Desktop\\ProjetoSemantico\\comp\\input.txt"
    with open(file_path, 'r') as file:
        data = file.read()
    lexer.input(data)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
