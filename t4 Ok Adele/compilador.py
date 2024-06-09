import ply.lex as lex
import ply.yacc as yacc

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

# Tokens
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
    r'/\*.*?\*/'
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

# Ambiente de símbolos
symbol_table = {}

# Função auxiliar para verificar tipos
def check_type(op, left, right):
    if op in ['+', '-', '*', '/', '%']:
        if left == right == 'int' or left == right == 'float':
            return left
        elif (left == 'int' and right == 'float') or (left == 'float' and right == 'int'):
            return 'float'
    elif op in ['<', '>', '<=', '>=', '==', '!=']:
        if left == right:
            return 'boolean'
    elif op in ['&&', '||']:
        if left == right == 'boolean':
            return 'boolean'
    elif op == '=':
        if left == right:
            return left
    print(f"Type error: {left} {op} {right} is not allowed")
    return None

# Definição das regras de precedência dos operadores
precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQ', 'NE'),
    ('left', 'LT', 'LE', 'GT', 'GE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIV', 'MOD')
)

# Definição das regras de gramática
def p_programa(p):
    '''programa : declaracao_list'''
    p[0] = ('programa', p[1])

def p_declaracao_list(p):
    '''declaracao_list : declaracao_list declaracao
                       | declaracao'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_declaracao(p):
    '''declaracao : declaracao_variavel
                  | declaracao_funcao
                  | estrutura_controle
                  | comentario
                  | expressao SEMICOLON'''
    p[0] = p[1]

def p_declaracao_variavel(p):
    '''declaracao_variavel : tipo ID SEMICOLON
                           | tipo ID EQUALS expressao SEMICOLON'''
    if len(p) == 4:
        symbol_table[p[2]] = p[1]
        p[0] = ('declaracao_variavel', p[1], p[2])
    else:
        expr_type = p[4][2]
        if p[1] == expr_type:
            symbol_table[p[2]] = p[1]
            p[0] = ('declaracao_variavel', p[1], p[2], p[4])
        else:
            print(f"Type error: cannot assign {expr_type} to {p[1]}")
            p[0] = ('error',)

def p_tipo(p):
    '''tipo : INT
            | FLOAT
            | DOUBLE
            | CHAR
            | BOOLEAN
            | VOID'''
    p[0] = p[1]

def p_declaracao_funcao(p):
    '''declaracao_funcao : tipo ID LPAREN parametros RPAREN bloco'''
    p[0] = ('declaracao_funcao', p[1], p[2], p[4], p[6])

def p_parametros(p):
    '''parametros : parametro
                  | parametro COMMA parametros
                  | vazio'''
    if len(p) == 2:
        p[0] = [p[1]]
    elif len(p) == 4:
        p[0] = [p[1]] + p[3]
    else:
        p[0] = []

def p_parametro(p):
    '''parametro : tipo ID
                 | tipo ID LBRACKET RBRACKET
                 | tipo DOTS ID'''
    if len(p) == 3:
        p[0] = ('parametro', p[1], p[2])
    elif len(p) == 5:
        p[0] = ('parametro_array', p[1], p[2])
    else:
        p[0] = ('parametro_variadico', p[1], p[3])

def p_bloco(p):
    '''bloco : LBRACE declaracao_list RBRACE'''
    p[0] = ('bloco', p[2])

def p_estrutura_controle(p):
    '''estrutura_controle : if
                          | while
                          | for
                          | switch'''
    p[0] = p[1]

def p_if(p):
    '''if : IF LPAREN expressao RPAREN bloco'''
    p[0] = ('if', p[3], p[5])

def p_while(p):
    '''while : WHILE LPAREN expressao RPAREN bloco'''
    p[0] = ('while', p[3], p[5])

def p_for(p):
    '''for : FOR LPAREN expressao SEMICOLON expressao SEMICOLON expressao RPAREN bloco'''
    p[0] = ('for', p[3], p[5], p[7], p[9])

def p_switch(p):
    '''switch : SWITCH LPAREN expressao RPAREN LBRACE case_list RBRACE'''
    p[0] = ('switch', p[3], p[6])

def p_case_list(p):
    '''case_list : case_list case
                 | case'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_case(p):
    '''case : CASE expressao COLON declaracao_list
            | DEFAULT COLON declaracao_list'''
    if len(p) == 5:
        p[0] = ('case', p[2], p[4])
    else:
        p[0] = ('default', p[3])

def p_comentario(p):
    '''comentario : COMMENT
                  | MULTI_COMMENT'''
    p[0] = p[1]

def p_expressao(p):
    '''expressao : ID
                 | NUM_INT
                 | NUM_DEC
                 | TEXTO
                 | ID EQUALS expressao
                 | expressao PLUS expressao
                 | expressao MINUS expressao
                 | expressao TIMES expressao
                 | expressao DIV expressao
                 | expressao MOD expressao
                 | LPAREN expressao RPAREN
                 | PRINTLN LPAREN TEXTO RPAREN'''
    if len(p) == 2:
        if isinstance(p[1], int):
            p[0] = ('numero_inteiro', p[1], 'int')
        elif isinstance(p[1], float):
            p[0] = ('numero_decimal', p[1], 'float')
        elif isinstance(p[1], str) and p[1][0] == '"' and p[1][-1] == '"':
            p[0] = ('texto', p[1], 'string')
        else:
            p[0] = ('id', p[1], symbol_table.get(p[1], 'desconhecido'))
    elif len(p) == 4 and p[2] == '=':
        expr_type = p[3][2] if isinstance(p[3], tuple) else 'desconhecido'
        var_type = symbol_table.get(p[1], 'desconhecido')
        if var_type == expr_type:
            p[0] = ('atribuicao', p[1], p[3], expr_type)
        else:
            print(f"Type error: cannot assign {expr_type} to {var_type}")
            p[0] = ('error',)
    elif len(p) == 4:
        left_type = p[1][2] if isinstance(p[1], tuple) else 'desconhecido'
        right_type = p[3][2] if isinstance(p[3], tuple) else 'desconhecido'
        result_type = check_type(p[2], left_type, right_type)
        if result_type:
            p[0] = ('operacao', p[1], p[2], p[3], result_type)
        else:
            p[0] = ('error',)
    elif len(p) == 4 and p[1] == '(':
        p[0] = p[2]
    elif len(p) == 5:
        p[0] = ('println', p[3])
    else:
        print(f"Syntax error at '{p.value}'")

def p_vazio(p):
    '''vazio :'''
    p[0] = None

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}'")
    else:
        print("Syntax error at EOF")

parser = yacc.yacc()

def test_parser(file_path):
    with open(file_path, 'r') as file:
        data = file.read()
    result = parser.parse(data)
    print(result)

# Código para ler o arquivo input.txt
file_path = 'input.txt'
test_parser(file_path)
